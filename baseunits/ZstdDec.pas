unit ZstdDec;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ZstdDecodeStream(inStream, outStream: TMemoryStream): Boolean;

implementation

const
  {$IFDEF WINDOWS}
  ZSTD_LIB = 'libzstd.dll';
  {$ELSE}
    {$IFDEF DARWIN}
    ZSTD_LIB = 'libzstd.dylib';
    {$ELSE}
    ZSTD_LIB = 'libzstd.so';
    {$ENDIF}
  {$ENDIF}

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

  function ZSTD_createDStream: Pointer; cdecl; external ZSTD_LIB;
  function ZSTD_freeDStream(zds: Pointer): SizeUInt; cdecl; external ZSTD_LIB;
  function ZSTD_initDStream(zds: Pointer): SizeUInt; cdecl; external ZSTD_LIB;
  function ZSTD_decompressStream(zds: Pointer; output: Pointer; input: Pointer): SizeUInt; cdecl; external ZSTD_LIB;
  function ZSTD_isError(code: SizeUInt): Cardinal; cdecl; external ZSTD_LIB;

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
  DStream := ZSTD_createDStream();
  if DStream = nil then Exit;

  try
    Ret := ZSTD_initDStream(DStream);
    if ZSTD_isError(Ret) <> 0 then Exit;

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

      Ret := ZSTD_decompressStream(DStream, @Output, @Input);
      
      // If the ret code represents an error, fail immediately
      if ZSTD_isError(Ret) <> 0 then Exit;

      // Flush buffer to the memory stream
      if Output.pos > 0 then
      begin
        outStream.SetSize(outStream.Size + Output.pos);
        outStream.Write(BufferOut[0], Output.pos);
      end;
    end;

    Result := True;
    inStream.Position := 0;
    outStream.Position := 0;
  finally
    ZSTD_freeDStream(DStream);
  end;
end;

end.