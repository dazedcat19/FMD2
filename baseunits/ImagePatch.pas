unit ImagePatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TImagePatchItem = record
    Left, Top, Width, Height: Double;
    Data: String;
  end;

  { TImagePatch }

  TImagePatch = class
  private
    FItems: array of TImagePatchItem;
    FJpegQuality: Integer;
    function GetCount: Integer;
  public
    constructor Create;
    procedure Clear;
    procedure Add(ALeft, ATop, AWidth, AHeight: Double; const ABase64Data: String);
    function Apply(input, output: TStream): Boolean;
    property Count: Integer read GetCount;
    property JpegQuality: Integer read FJpegQuality write FJpegQuality;
  end;

implementation

uses
  base64, FPImage, FPReadJPEG, FPReadPNG, FPWriteJPEG, FPWritePNG,
  MemBitmap, webp, MultiLog;

constructor TImagePatch.Create;
begin
  inherited Create;
  FJpegQuality := 80;
  SetLength(FItems, 0);
end;

function TImagePatch.GetCount: Integer;
begin
  Result := Length(FItems);
end;

procedure TImagePatch.Clear;
begin
  SetLength(FItems, 0);
end;

procedure TImagePatch.Add(ALeft, ATop, AWidth, AHeight: Double; const ABase64Data: String);
var
  n: Integer;
begin
  n := Length(FItems);
  SetLength(FItems, n + 1);
  FItems[n].Left := ALeft;
  FItems[n].Top := ATop;
  FItems[n].Width := AWidth;
  FItems[n].Height := AHeight;
  FItems[n].Data := DecodeStringBase64(ABase64Data);
end;

function DecodeToMem(const AData: String; out AIsJpeg: Boolean): TMemBitmap;
var
  ms: TMemoryStream;
  reader: TFPCustomImageReader;
begin
  Result := nil;
  AIsJpeg := False;
  if Length(AData) < 4 then Exit;
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(AData[1], Length(AData));
    ms.Position := 0;

    try
      Result := WebPToMemBitmap(ms);
    except
      Result := nil;
    end;
    if Result <> nil then Exit;

    ms.Position := 0;
    reader := nil;
    if (Byte(AData[1]) = $FF) and (Byte(AData[2]) = $D8) then
    begin
      reader := TFPReaderJPEG.Create;
      AIsJpeg := True;
    end
    else
    if (Byte(AData[1]) = $89) and (Byte(AData[2]) = $50) then
      reader := TFPReaderPNG.Create
    else
      Exit;
    try
      Result := TMemBitmap.Create(0, 0);
      try
        Result.LoadFromStream(ms, reader);
      except
        FreeAndNil(Result);
        raise;
      end;
    finally
      reader.Free;
    end;
  finally
    ms.Free;
  end;
end;

function TImagePatch.Apply(input, output: TStream): Boolean;
var
  data: String;
  base, patch: TMemBitmap;
  isJpeg, dummy: Boolean;
  i, dx, dy: Integer;
  jw: TFPWriterJPEG;
  pw: TFPWriterPNG;
begin
  Result := False;
  if (input = nil) or (output = nil) then Exit;

  input.Position := 0;
  SetLength(data, input.Size - input.Position);
  if Length(data) = 0 then Exit;
  input.ReadBuffer(data[1], Length(data));

  base := nil;
  try
    base := DecodeToMem(data, isJpeg);
    if (base = nil) or (base.Width <= 0) or (base.Height <= 0) then
    begin
      Logger.SendError(Self.ClassName + '.Apply: could not decode source image');
      Exit;
    end;

    for i := 0 to High(FItems) do
    begin
      patch := DecodeToMem(FItems[i].Data, dummy);
      if patch = nil then
      begin
        Logger.SendError(Self.ClassName + '.Apply: could not decode patch #' + IntToStr(i));
        Continue;
      end;
      try
        dx := Round(base.Width * FItems[i].Left / 100);
        dy := Round(base.Height * FItems[i].Top / 100);
        base.PutImage(dx, dy, patch, dmSet);
            finally
        patch.Free;
      end;
    end;

    output.Position := 0;
    output.Size := 0;
    if isJpeg then
    begin
      jw := TFPWriterJPEG.Create;
      try
        jw.CompressionQuality := FJpegQuality;
        base.SaveToStream(output, jw);
      finally
        jw.Free;
      end;
    end
    else
    begin
      pw := TFPWriterPNG.Create;
      try
        pw.Indexed := False;
        pw.UseAlpha := base.HasTransparentPixels;
        base.SaveToStream(output, pw);
      finally
        pw.Free;
      end;
    end;
    output.Position := 0;
    Result := True;
  finally
    base.Free;
  end;
end;

end.