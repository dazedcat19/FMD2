unit webp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MemBitmap, Dynlibs;

var
  WebPLibHandle: TLibHandle = 0;
  DLLWebPName: String = {$IFDEF LINUX} 'libwebp.so' {$ELSE} 'libwebp.dll' {$ENDIF};

function IsWebPModuleLoaded: Boolean;
procedure InitWebPModule;
procedure DestroyWebPModule;
function WebPToMemBitmap(webp: TMemoryStream): TMemBitmap;
function WebPGetVersion: String;
function IsAnimatedWebP(webp: TMemoryStream): Boolean;

implementation

uses
  SyncObjs;

type
  TWebPGetInfo = function (data: Pointer; data_size: UInt32; width, height: pInt32): Int32; cdecl;
  TWebPDecodeBGRAInto = function (data: Pointer; data_size: UInt32; output: Pointer; output_size: UInt32; stride: Int32): pInt32; cdecl;
  TWebPGetDecoderVersion = function (): Int32; cdecl;

var
  pWebPGetInfo: TWebPGetInfo = nil;
  pWebPDecodeBGRAInto: TWebPDecodeBGRAInto = nil;
  pWebPGetDecoderVersion: TWebPGetDecoderVersion = nil;
  webpCS: TCriticalSection;
  webpLibLoaded: Boolean = False;

resourcestring
  SErrLoadFailed = 'Can not load WebP codec library "%s". Check your installation.';

function IsWebPModuleLoaded: Boolean;
begin
  Result := webpLibLoaded;
end;

procedure InitWebPModule;
begin
  if IsWebPModuleLoaded then Exit;
  webpCS.Enter;
  try
    if not IsWebPModuleLoaded then begin
      WebPLibHandle := LoadLibrary(PChar(DLLWebPName));
      if WebPLibHandle <> 0 then begin
        pWebPGetInfo := TWebPGetInfo(GetProcAddress(WebPLibHandle, 'WebPGetInfo'));
        pWebPDecodeBGRAInto := TWebPDecodeBGRAInto(GetProcAddress(WebPLibHandle, 'WebPDecodeBGRAInto'));
        pWebPGetDecoderVersion := TWebPGetDecoderVersion(GetProcAddress(WebPLibHandle, 'WebPGetDecoderVersion'));
        webpLibLoaded := True;
      end else
        raise EInOutError.CreateFmt(SErrLoadFailed, [DLLWebPName]);
    end;
  finally
    webpCS.Leave;
  end;
end;

procedure DestroyWebPModule;
begin
  webpCS.Enter;
  try
    if IsWebPModuleLoaded then begin
      if WebPLibHandle <> 0 then begin
        pWebPGetInfo := nil;
        pWebPDecodeBGRAInto := nil;
        pWebPGetDecoderVersion := nil;
        FreeLibrary(WebPLibHandle);
        WebPLibHandle := 0;
      end;
      webpLibLoaded := False;
    end;
  finally
    webpCS.Leave;
  end;
end;

function WebPGetInfo(data: Pointer; data_size: UInt32; width, height: pInt32): Int32;
begin
  if IsWebPModuleLoaded and Assigned(pWebPGetInfo) then
    Result := pWebPGetInfo(data, data_size, width, height)
  else
    Result := 0;
end;

function WebPDecodeBGRAInto(data: Pointer; data_size: UInt32; output: Pointer; output_size: UInt32; stride: Int32): pInt32;
begin
  if IsWebPModuleLoaded and Assigned(pWebPDecodeBGRAInto) then
    Result := pWebPDecodeBGRAInto(data, data_size, output, output_size, stride)
  else
    Result := nil;
end;

function WebPGetDecoderVersion: Int32;
begin
  if IsWebPModuleLoaded and Assigned(pWebPGetDecoderVersion) then
    Result := pWebPGetDecoderVersion()
  else
    Result := 0;
end;

function IsAnimatedWebP(webp: TMemoryStream): Boolean;
var
  Hdr: array[0..3] of Char;
  Flags: Byte;
  OldPos: Int64;
begin
  Result := False;
  if (webp = nil) or (webp.Size < 21) then Exit;
  OldPos := webp.Position;
  try
    webp.Position := 0;
    if webp.Read(Hdr, 4) <> 4 then Exit;
    if Hdr <> 'RIFF' then Exit;
    webp.Position := 8;
    if webp.Read(Hdr, 4) <> 4 then Exit;
    if Hdr <> 'WEBP' then Exit;
    webp.Position := 12;
    if webp.Read(Hdr, 4) <> 4 then Exit;
    if Hdr <> 'VP8X' then Exit;
    webp.Position := 20;
    if webp.Read(Flags, 1) <> 1 then Exit;
    Result := (Flags and $02) <> 0;
  finally
    webp.Position := OldPos;
  end;
end;

function WebPGetVersion: String;
var
  ver: Int32;
begin
  Result := '';
  ver := WebPGetDecoderVersion;
  if ver > 0 then begin
    Result += chr(((ver shr 16) and $ff) + $30) + '.';
    Result += chr(((ver shr  8) and $ff) + $30) + '.';
    Result += chr(( ver         and $ff) + $30);
  end;
end;

function WebPToMemBitmap(webp: TMemoryStream): TMemBitmap;
var
  e, width, height, stride: Integer;
  scan: pInt32;
  r: TMemBitmap;
begin
  Result := nil;
  if webp = nil then Exit;

  e := WebPGetInfo(webp.Memory, webp.Size, @width, @height);
  if e = 0 then Exit;

  r := TMemBitmap.Create(width, height);
  stride := (r.ScanLine[1] - r.ScanLine[0]) * SizeOf(TMemPixel);
  scan := WebPDecodeBGRAInto(webp.Memory, webp.Size,
    r.ScanLine[0], stride * height, stride);

  if scan <> PLongint(r.ScanLine[0]) then begin
    r.Free;
    Exit;
  end;

  Result := r;
end;

initialization
  webpCS := TCriticalSection.Create;

finalization
  DestroyWebPModule;
  webpCS.Free;

end.

