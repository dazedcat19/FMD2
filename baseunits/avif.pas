unit avif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MemBitmap, Dynlibs;

var
  AvifLibHandle: TLibHandle = 0;
  DLLAvifName: String = {$IFDEF LINUX} 'libavif.so' {$ELSE} 'libavif.dll' {$ENDIF};

function IsAvifModuleLoaded: Boolean;
procedure InitAvifModule;
procedure DestroyAvifModule;
function AvifToMemBitmap(avifStream: TMemoryStream): TMemBitmap;
function AvifGetVersion: String;

implementation

uses
  SyncObjs;

type
  avifResult = Integer;
  avifRGBFormat = Integer;

const
  AVIF_RESULT_OK = 0;
  AVIF_RGB_FORMAT_BGRA = 4; // Matches AVIF_RGB_FORMAT_BGRA in avif.h

type
  // Core libavif structs needed for the decode layout
  avifRGBImage = record
    width: UInt32;
    height: UInt32;
    depth: UInt32;
    format: avifRGBFormat;
    chromaUpsampling: Integer;
    chromaDownsampling: Integer;
    ignoreAlpha: Integer;
    alphaPremultiplied: Integer;
    isFloat: Integer;
    maxThreads: Integer;
    pixels: PByte;
    rowBytes: UInt32;
  end;
  PAvifRGBImage = ^avifRGBImage;

  // C-decl functions mapped from libavif
  TavifVersion = function(): PAnsiChar; cdecl;
  TavifDecoderCreate = function(): Pointer; cdecl;
  TavifDecoderDestroy = procedure(decoder: Pointer); cdecl;
  TavifDecoderReadMemory = function(decoder: Pointer; image: Pointer; const data: PByte; size: SizeInt): avifResult; cdecl;
  TavifImageCreateEmpty = function(): Pointer; cdecl;
  TavifImageDestroy = procedure(image: Pointer); cdecl;
  TavifRGBImageSetDefaults = procedure(rgb: PAvifRGBImage; const image: Pointer); cdecl;
  TavifImageYUVToRGB = function(const image: Pointer; rgb: PAvifRGBImage): avifResult; cdecl;

var
  pavifVersion: TavifVersion = nil;
  pavifDecoderCreate: TavifDecoderCreate = nil;
  pavifDecoderDestroy: TavifDecoderDestroy = nil;
  pavifDecoderReadMemory: TavifDecoderReadMemory = nil;
  pavifImageCreateEmpty: TavifImageCreateEmpty = nil;
  pavifImageDestroy: TavifImageDestroy = nil;
  pavifRGBImageSetDefaults: TavifRGBImageSetDefaults = nil;
  pavifImageYUVToRGB: TavifImageYUVToRGB = nil;
  
  avifCS: TCriticalSection;
  avifLibLoaded: Boolean = False;

resourcestring
  SErrLoadFailed = 'Can not load AVIF codec library "%s". Check your installation.';

function IsAvifModuleLoaded: Boolean;
begin
  Result := avifLibLoaded;
end;

procedure InitAvifModule;
begin
  if IsAvifModuleLoaded then Exit;
  avifCS.Enter;
  try
    if not IsAvifModuleLoaded then begin
      AvifLibHandle := LoadLibrary(PChar(DLLAvifName));
      if AvifLibHandle <> 0 then begin
        pavifVersion := TavifVersion(GetProcAddress(AvifLibHandle, 'avifVersion'));
        pavifDecoderCreate := TavifDecoderCreate(GetProcAddress(AvifLibHandle, 'avifDecoderCreate'));
        pavifDecoderDestroy := TavifDecoderDestroy(GetProcAddress(AvifLibHandle, 'avifDecoderDestroy'));
        pavifDecoderReadMemory := TavifDecoderReadMemory(GetProcAddress(AvifLibHandle, 'avifDecoderReadMemory'));
        pavifImageCreateEmpty := TavifImageCreateEmpty(GetProcAddress(AvifLibHandle, 'avifImageCreateEmpty'));
        pavifImageDestroy := TavifImageDestroy(GetProcAddress(AvifLibHandle, 'avifImageDestroy'));
        pavifRGBImageSetDefaults := TavifRGBImageSetDefaults(GetProcAddress(AvifLibHandle, 'avifRGBImageSetDefaults'));
        pavifImageYUVToRGB := TavifImageYUVToRGB(GetProcAddress(AvifLibHandle, 'avifImageYUVToRGB'));
        
        avifLibLoaded := True;
      end else
        raise EInOutError.CreateFmt(SErrLoadFailed, [DLLAvifName]);
    end;
  finally
    avifCS.Leave;
  end;
end;

procedure DestroyAvifModule;
begin
  avifCS.Enter;
  try
    if IsAvifModuleLoaded then begin
      if AvifLibHandle <> 0 then begin
        pavifVersion := nil;
        pavifDecoderCreate := nil;
        pavifDecoderDestroy := nil;
        pavifDecoderReadMemory := nil;
        pavifImageCreateEmpty := nil;
        pavifImageDestroy := nil;
        pavifRGBImageSetDefaults := nil;
        pavifImageYUVToRGB := nil;
        
        FreeLibrary(AvifLibHandle);
        AvifLibHandle := 0;
      end;
      avifLibLoaded := False;
    end;
  finally
    avifCS.Leave;
  end;
end;

function AvifGetVersion: String;
var
  pVer: PAnsiChar;
begin
  Result := '';
  if IsAvifModuleLoaded and Assigned(pavifVersion) then begin
    pVer := pavifVersion();
    if Assigned(pVer) then
      Result := String(pVer);
  end;
end;

function AvifToMemBitmap(avifStream: TMemoryStream): TMemBitmap;
var
  decoder: Pointer;
  image: Pointer;
  rgb: avifRGBImage;
  res: avifResult;
  r: TMemBitmap;
  stride: Integer;
begin
  Result := nil;
  if avifStream = nil then Exit;
  if not IsAvifModuleLoaded then Exit;

  decoder := pavifDecoderCreate();
  if decoder = nil then Exit;

  image := pavifImageCreateEmpty();
  if image = nil then begin
    pavifDecoderDestroy(decoder);
    Exit;
  end;

  r := nil;
  try
    // Decode the AVIF file from memory
    res := pavifDecoderReadMemory(decoder, image, avifStream.Memory, avifStream.Size);
    if res <> AVIF_RESULT_OK then Exit;

    // Set defaults (populates rgb.width and rgb.height based on the parsed image)
    FillChar(rgb, SizeOf(rgb), 0);
    pavifRGBImageSetDefaults(@rgb, image);

    // Force output to standard 8-bit BGRA to match TMemBitmap memory layout
    rgb.format := AVIF_RGB_FORMAT_BGRA;
    rgb.depth := 8;

    r := TMemBitmap.Create(rgb.width, rgb.height);
    stride := (r.ScanLine[1] - r.ScanLine[0]) * SizeOf(TMemPixel);

    // Map libavif's pixel destination directly into our TMemBitmap's memory to avoid allocating twice
    rgb.pixels := PByte(r.ScanLine[0]);
    rgb.rowBytes := stride;

    // Perform color space conversion YUV -> RGB
    res := pavifImageYUVToRGB(image, @rgb);
    if res = AVIF_RESULT_OK then begin
      Result := r;
      r := nil; // Release local reference to prevent Free in the finally block
    end;

  finally
    if Assigned(r) then r.Free;
    pavifImageDestroy(image);
    pavifDecoderDestroy(decoder);
  end;
end;

initialization
  avifCS := TCriticalSection.Create;

finalization
  DestroyAvifModule;
  avifCS.Free;

end.