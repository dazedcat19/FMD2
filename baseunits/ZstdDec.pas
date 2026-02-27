unit ZstdDec;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dynlibs;

var
  ZstdLibHandle: TLibHandle = 0;
  DLLZstdName: String = {$IFDEF WINDOWS} 'libzstd.dll' {$ELSE} 
                        {$IFDEF DARWIN} 'libzstd.dylib' {$ELSE} 'libzstd.so' {$ENDIF} {$ENDIF};

function IsZstdModuleLoaded: Boolean;
procedure InitZstdModule;
procedure DestroyZstdModule;
function ZstdGetVersion: String;
function ZstdDecodeStream(inStream, outStream: TMemoryStream): Boolean;

implementation

uses
  SyncObjs;

type
  TZSTD_inBuffer = record
    src: Pointer;
    size: SizeUInt;
    pos: SizeUInt;
  end;

  TZSTD_outBuffer = record
    dst: Pointer;
    size: SizeUInt;
    pos: SizeUInt;
  end;

  // Function prototypes for dynamic mapping
  TZSTD_createDStream = function: Pointer; cdecl;
  TZSTD_freeDStream = function(zds: Pointer): SizeUInt; cdecl;
  TZSTD_initDStream = function(zds: Pointer): SizeUInt; cdecl;
  TZSTD_decompressStream = function(zds: Pointer; output: Pointer; input: Pointer): SizeUInt; cdecl;
  TZSTD_isError = function(code: SizeUInt): Cardinal; cdecl;
  TZSTD_versionNumber = function: Cardinal; cdecl;

var
  pZSTD_createDStream: TZSTD_createDStream = nil;
  pZSTD_freeDStream: TZSTD_freeDStream = nil;
  pZSTD_initDStream: TZSTD_initDStream = nil;
  pZSTD_decompressStream: TZSTD_decompressStream = nil;
  pZSTD_isError: TZSTD_isError = nil;
  pZSTD_versionNumber: TZSTD_versionNumber = nil;
  
  zstdCS: TCriticalSection;
  zstdLibLoaded: Boolean = False;

resourcestring
  SErrLoadFailed = 'Can not load Zstd codec library "%s". Check your installation.';

function IsZstdModuleLoaded: Boolean;
begin
  Result := zstdLibLoaded;
end;

procedure InitZstdModule;
begin
  if IsZstdModuleLoaded then Exit;
  zstdCS.Enter;
  try
    if not IsZstdModuleLoaded then begin
      ZstdLibHandle := LoadLibrary(PChar(DLLZstdName));
      if ZstdLibHandle <> 0 then begin
        pZSTD_createDStream := TZSTD_createDStream(GetProcAddress(ZstdLibHandle, 'ZSTD_createDStream'));
        pZSTD_freeDStream := TZSTD_freeDStream(GetProcAddress(ZstdLibHandle, 'ZSTD_freeDStream'));
        pZSTD_initDStream := TZSTD_initDStream(GetProcAddress(ZstdLibHandle, 'ZSTD_initDStream'));
        pZSTD_decompressStream := TZSTD_decompressStream(GetProcAddress(ZstdLibHandle, 'ZSTD_decompressStream'));
        pZSTD_isError := TZSTD_isError(GetProcAddress(ZstdLibHandle, 'ZSTD_isError'));
        pZSTD_versionNumber := TZSTD_versionNumber(GetProcAddress(ZstdLibHandle, 'ZSTD_versionNumber'));
        zstdLibLoaded := True;
      end else
        raise EInOutError.CreateFmt(SErrLoadFailed, [DLLZstdName]);
    end;
  finally
    zstdCS.Leave;
  end;
end;

procedure DestroyZstdModule;
begin
  zstdCS.Enter;
  try
    if IsZstdModuleLoaded then begin
      if ZstdLibHandle <> 0 then begin
        pZSTD_createDStream := nil;
        pZSTD_freeDStream := nil;
        pZSTD_initDStream := nil;
        pZSTD_decompressStream := nil;
        pZSTD_isError := nil;
        pZSTD_versionNumber := nil;
        FreeLibrary(ZstdLibHandle);
        ZstdLibHandle := 0;
      end;
      zstdLibLoaded := False;
    end;
  finally
    zstdCS.Leave;
  end;
end;

function ZstdGetVersion: String;
var
  v: Cardinal;
begin
  Result := 'Unknown';
  if IsZstdModuleLoaded and Assigned(pZSTD_versionNumber) then
  begin
    v := pZSTD_versionNumber();
    // Zstd version is returned as: (major * 10000 + minor * 100 + release)
    Result := Format('%d.%d.%d', [v div 10000, (v div 100) mod 100, v mod 100]);
  end;
end;

const
  BUFFER_SIZE = 131072; // 128 KB chunk size

function ZstdDecodeStream(inStream, outStream: TMemoryStream): Boolean;
var
  DStream: Pointer;
  Ret: SizeUInt;
  Input: TZSTD_inBuffer;
  Output: TZSTD_outBuffer;
  BufferOut: array of Byte;
begin
  Result := False;
  if not IsZstdModuleLoaded then InitZstdModule;

  DStream := pZSTD_createDStream();
  if DStream = nil then Exit;

  try
    Ret := pZSTD_initDStream(DStream);
    if pZSTD_isError(Ret) <> 0 then Exit;

    inStream.Position := 0;
    outStream.Clear;

    Input.src := inStream.Memory;
    Input.size := SizeUInt(inStream.Size);
    Input.pos := 0;

    SetLength(BufferOut, BUFFER_SIZE);

    while Input.pos < Input.size do
    begin
      Output.dst := @BufferOut[0];
      Output.size := BUFFER_SIZE;
      Output.pos := 0;

      Ret := pZSTD_decompressStream(DStream, @Output, @Input);
      
      if pZSTD_isError(Ret) <> 0 then Exit;

      if Output.pos > 0 then
      begin
        outStream.Write(BufferOut[0], Output.pos);
      end;
    end;

    Result := True;
    inStream.Position := 0;
    outStream.Position := 0;
  finally
    pZSTD_freeDStream(DStream);
  end;
end;

initialization
  zstdCS := TCriticalSection.Create;

finalization
  DestroyZstdModule;
  zstdCS.Free;

end.