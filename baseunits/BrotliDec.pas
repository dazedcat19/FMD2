unit BrotliDec;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dynlibs;

var
  BrotliLibHandle: TLibHandle = 0;
  DLLBrotliName: String = {$IFDEF WINDOWS} 'libbrotlidec.dll' {$ELSE} 
                          {$IFDEF DARWIN} 'libbrotlidec.dylib' {$ELSE} 'libbrotlidec.so' {$ENDIF} {$ENDIF};

function IsBrotliModuleLoaded: Boolean;
procedure InitBrotliModule;
procedure DestroyBrotliModule;
function BrotliGetVersion: String;
function BrotliDecodeStream(inStream, outStream: TMemoryStream): Boolean;

implementation

uses
  SyncObjs;

type
  TBrotliSize = {$IFDEF CPU386} Integer {$ELSE} Int64 {$ENDIF};

const
  BROTLI_DECODER_PARAM_LARGE_WINDOW = 1;
  BROTLI_DECODER_RESULT_ERROR = 0;
  BROTLI_DECODER_RESULT_SUCCESS = 1;
  BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT = 3;
  BUFFER_SIZE = 1 shl 19;

type
  TBrotliDecoderCreateInstance = function(const alloc_func, free_func, opaque: Pointer): Pointer; cdecl;
  TBrotliDecoderDestroyInstance = procedure(const state: Pointer); cdecl;
  TBrotliDecoderSetParameter = function(const state: Pointer; const BrotliDecoderParameter: Integer; const Value: Cardinal): Integer; cdecl;
  TBrotliDecoderDecompressStream = function(const state: Pointer; var available_in: TBrotliSize; var next_in: Pointer; var available_out: TBrotliSize; var next_out: Pointer; total_out: Pointer = nil): Integer; cdecl;
  TBrotliDecoderVersion = function: Cardinal; cdecl;

var
  pBrotliDecoderCreateInstance: TBrotliDecoderCreateInstance = nil;
  pBrotliDecoderDestroyInstance: TBrotliDecoderDestroyInstance = nil;
  pBrotliDecoderSetParameter: TBrotliDecoderSetParameter = nil;
  pBrotliDecoderDecompressStream: TBrotliDecoderDecompressStream = nil;
  pBrotliDecoderVersion: TBrotliDecoderVersion = nil;

  brotliCS: TCriticalSection;
  brotliLibLoaded: Boolean = False;

resourcestring
  SErrLoadFailed = 'Can not load Brotli codec library "%s". Check your installation.';

function IsBrotliModuleLoaded: Boolean;
begin
  Result := brotliLibLoaded;
end;

procedure InitBrotliModule;
begin
  if IsBrotliModuleLoaded then Exit;
  brotliCS.Enter;
  try
    if not IsBrotliModuleLoaded then begin
      BrotliLibHandle := LoadLibrary(PChar(DLLBrotliName));
      if BrotliLibHandle <> 0 then begin
        pBrotliDecoderCreateInstance := TBrotliDecoderCreateInstance(GetProcAddress(BrotliLibHandle, 'BrotliDecoderCreateInstance'));
        pBrotliDecoderDestroyInstance := TBrotliDecoderDestroyInstance(GetProcAddress(BrotliLibHandle, 'BrotliDecoderDestroyInstance'));
        pBrotliDecoderSetParameter := TBrotliDecoderSetParameter(GetProcAddress(BrotliLibHandle, 'BrotliDecoderSetParameter'));
        pBrotliDecoderDecompressStream := TBrotliDecoderDecompressStream(GetProcAddress(BrotliLibHandle, 'BrotliDecoderDecompressStream'));
        pBrotliDecoderVersion := TBrotliDecoderVersion(GetProcAddress(BrotliLibHandle, 'BrotliDecoderVersion'));
        brotliLibLoaded := True;
      end else
        raise EInOutError.CreateFmt(SErrLoadFailed, [DLLBrotliName]);
    end;
  finally
    brotliCS.Leave;
  end;
end;

procedure DestroyBrotliModule;
begin
  brotliCS.Enter;
  try
    if IsBrotliModuleLoaded then begin
      if BrotliLibHandle <> 0 then begin
        pBrotliDecoderCreateInstance := nil;
        pBrotliDecoderDestroyInstance := nil;
        pBrotliDecoderSetParameter := nil;
        pBrotliDecoderDecompressStream := nil;
        pBrotliDecoderVersion := nil;
        FreeLibrary(BrotliLibHandle);
        BrotliLibHandle := 0;
      end;
      brotliLibLoaded := False;
    end;
  finally
    brotliCS.Leave;
  end;
end;

function BrotliGetVersion: String;
var
  v: Cardinal;
begin
  Result := 'Unknown';
  if IsBrotliModuleLoaded and Assigned(pBrotliDecoderVersion) then
  begin
    v := pBrotliDecoderVersion();
    Result := Format('%d.%d.%d', [v shr 24, (v shr 12) and $FFF, v and $FFF]);
  end;
end;

function BrotliDecodeStream(inStream, outStream: TMemoryStream): Boolean;
var
  State: Pointer;
  DecoderResult: Integer;
  AvailableIn: TBrotliSize;
  NextIn: Pointer;
  AvailableOut: TBrotliSize;
  NextOut: Pointer;
  BufferOut: array of Byte;

  procedure WriteOutput;
  var
    OutSize: TBrotliSize;
  begin
    OutSize := PAnsiChar(NextOut) - PAnsiChar(@BufferOut[0]);
    if OutSize = 0 then Exit;
    outStream.Write(BufferOut[0], OutSize);
  end;

begin
  Result := False;
  if not IsBrotliModuleLoaded then InitBrotliModule;

  State := pBrotliDecoderCreateInstance(nil, nil, nil);
  if State = nil then Exit;

  try
    if pBrotliDecoderSetParameter(State, BROTLI_DECODER_PARAM_LARGE_WINDOW, 1) = 0 then Exit;

    inStream.Position := 0;
    outStream.Clear;

    AvailableIn := inStream.Size;
    NextIn := inStream.Memory;
    SetLength(BufferOut, BUFFER_SIZE);

    while True do
    begin
      AvailableOut := BUFFER_SIZE;
      NextOut := @BufferOut[0];

      DecoderResult := pBrotliDecoderDecompressStream(
        State, AvailableIn, NextIn, AvailableOut, NextOut);

      case DecoderResult of
        BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT:
          WriteOutput;

        BROTLI_DECODER_RESULT_SUCCESS:
          begin
            WriteOutput;
            Result := True;
            Break;
          end;

        else Break;
      end;
    end;

    inStream.Position := 0;
    outStream.Position := 0;
  finally
    pBrotliDecoderDestroyInstance(State);
  end;
end;

initialization
  brotliCS := TCriticalSection.Create;

finalization
  DestroyBrotliModule;
  brotliCS.Free;

end.