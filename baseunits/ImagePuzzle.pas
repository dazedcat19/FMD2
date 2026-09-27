unit ImagePuzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Types, IntfGraphics;

type
  TImagePuzzle = class
  private
    FHorBlock, FVerBlock, FMultiply: Integer;
    FMatrix: TIntegerDynArray;
    FFlips: TIntegerDynArray;
    function GetFlip(const Index: Integer): Integer;
    procedure Ensure32bit(var AIntf: TLazIntfImage);
  public
    constructor Create(horBlockCount, verBlockCount: Integer);
    procedure DeScramble(input, output: TStream);
    property HorBlock: Integer read FHorBlock;
    property VerBlock: Integer read FVerBlock;
    property Multiply: Integer read FMultiply write FMultiply default 1;
    property Matrix: TIntegerDynArray read FMatrix;
    property Flips: TIntegerDynArray read FFlips;
  end;

implementation

uses Math, MemBitmap, webp, GraphType, MultiLog;

constructor TImagePuzzle.Create(horBlockCount, verBlockCount: Integer);
var i: Integer;
begin
  FHorBlock := horBlockCount;
  FVerBlock := verBlockCount;
  SetLength(FMatrix, FHorBlock * FVerBlock);
  SetLength(FFlips, FHorBlock * FVerBlock);
  for i := 0 to High(FMatrix) do
  begin
    FMatrix[i] := i;
    FFlips[i] := 0;
  end;
end;

function TImagePuzzle.GetFlip(const Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < Length(FFlips)) then
  begin
    Result := FFlips[Index];
  end
  else
  begin
    Result := 0;
  end;
end;

procedure CopyMemBitmapToIntf(AMemBitmap: TMemBitmap; AIntf: TLazIntfImage);
var
  Desc: TRawImageDescription;
  x, y, w, h: Integer;
  rb, gb, bb, ab: Integer;
  src, dst: PByte;
begin
  Desc := AIntf.DataDescription;
  w := AMemBitmap.Width;
  h := AMemBitmap.Height;
  rb := Desc.RedShift div 8;
  gb := Desc.GreenShift div 8;
  bb := Desc.BlueShift div 8;
  ab := 3;
  if (bb = 0) and (gb = 1) and (rb = 2) and (Desc.AlphaPrec = 0) then
  begin
    for y := 0 to h - 1 do
    begin
      Move(AMemBitmap.ScanLine[y]^, AIntf.GetDataLineStart(y)^, w * 4);
    end;
    Exit;
  end;
  if (bb = 0) and (gb = 1) and (rb = 2) and (ab = 3) then
  begin
    for y := 0 to h - 1 do
    begin
      Move(AMemBitmap.ScanLine[y]^, AIntf.GetDataLineStart(y)^, w * 4);
    end;
    Exit;
  end;
  for y := 0 to h - 1 do
  begin
    src := PByte(AMemBitmap.ScanLine[y]);
    dst := AIntf.GetDataLineStart(y);
    for x := 0 to w - 1 do
    begin
      (dst + x * 4 + rb)^ := (src + x * 4 + 2)^;
      (dst + x * 4 + gb)^ := (src + x * 4 + 1)^;
      (dst + x * 4 + bb)^ := (src + x * 4 + 0)^;
      if Desc.AlphaPrec > 0 then
      begin
        (dst + x * 4 + ab)^ := (src + x * 4 + 3)^;
      end;
    end;
  end;
end;

procedure TImagePuzzle.Ensure32bit(var AIntf: TLazIntfImage);
var
  Intf32: TLazIntfImage;
  x, y, w, h: Integer;
begin
  if AIntf.DataDescription.Depth = 32 then
  begin
    Exit;
  end;
  w := AIntf.Width;
  h := AIntf.Height;
  Intf32 := TLazIntfImage.Create(w, h, [riqfRGB, riqfAlpha]);
  try
    for y := 0 to h - 1 do
    begin
      for x := 0 to w - 1 do
      begin
        Intf32.Colors[x, y] := AIntf.Colors[x, y];
      end;
    end;
    FreeAndNil(AIntf);
    AIntf := Intf32;
  except
    on E: Exception do
    begin
      Logger.SendException(Self.ClassName + '.Ensure32bit: conversion to 32bit failed', E);
      Intf32.Free;
    end;
  end;
end;

procedure TImagePuzzle.DeScramble(input, output: TStream);
var
  image, result: TPicture;
  memStream: TMemoryStream;
  tmpMemBitmap: TMemBitmap;
  tmpBitmap: TBitmap;
  srcIntf, dstIntf: TLazIntfImage;
  blockWidth, blockHeight: Double;
  i, row, col, x, k: Integer;
  dx1, dy1, dx2, dy2, sx1, sy1: Integer;
  tileWidth, tileHeight, srcRow, flags: Integer;
  width, height: Integer;
  srcLine, dstLine: PByte;
  ext: String = 'jpg';

  procedure LogAndFail(const AMsg: String);
  begin
    Logger.SendError(Self.ClassName + '.DeScramble: ' + AMsg);
    output.Size := 0;
    output.Position := 0;
  end;

begin
  if not Assigned(input) or not Assigned(output) then
  begin
    Exit;
  end;
  if Length(FMatrix) = 0 then
  begin
    LogAndFail('Matrix is not set');
    Exit;
  end;
  if Length(FMatrix) < HorBlock * VerBlock then
  begin
    LogAndFail('Invalid matrix size');
    Exit;
  end;
  image := TPicture.Create;
  result := TPicture.Create;
  memStream := TMemoryStream.Create;
  tmpBitmap := nil;
  srcIntf := nil;
  dstIntf := nil;
  try
    memStream.LoadFromStream(input);
    memStream.Position := 0;

    tmpMemBitmap := WebPToMemBitmap(memStream);
    if Assigned(tmpMemBitmap) then
    begin
      try
        ext := 'png';
        width := tmpMemBitmap.Width;
        height := tmpMemBitmap.Height;
        if (width <= 0) or (height <= 0) then
        begin
          LogAndFail('invalid webp size');
          Exit;
        end;
        srcIntf := TLazIntfImage.Create(width, height, [riqfRGB, riqfAlpha]);
        CopyMemBitmapToIntf(tmpMemBitmap, srcIntf);
      finally
        tmpMemBitmap.Free;
      end;
    end
    else
    begin
      memStream.Position := 0;
      image.LoadFromStream(memStream);
      if image.Graphic is TPortableNetworkGraphic then
      begin
        ext := 'png';
      end;
      if (image.Graphic = nil) or (image.Width <= 0) or (image.Height <= 0) then
      begin
        LogAndFail('could not decode image');
        Exit;
      end;
      tmpBitmap := TBitmap.Create;
      tmpBitmap.SetSize(image.Width, image.Height);
      tmpBitmap.PixelFormat := pf32bit;
      tmpBitmap.Canvas.Draw(0, 0, image.Graphic);
      width := tmpBitmap.Width;
      height := tmpBitmap.Height;
      srcIntf := TLazIntfImage.Create(width, height, [riqfRGB, riqfAlpha]);
      srcIntf.LoadFromBitmap(tmpBitmap.Handle, tmpBitmap.MaskHandle);
      Ensure32bit(srcIntf);
    end;
    if srcIntf.DataDescription.Depth <> 32 then
    begin
      LogAndFail('could not obtain 32bit pixel data');
      Exit;
    end;

    dstIntf := TLazIntfImage.Create(width, height, [riqfRGB, riqfAlpha]);
    for i := 0 to height - 1 do
    begin
      FillChar(dstIntf.GetDataLineStart(i)^, dstIntf.DataDescription.BytesPerLine, 255);
    end;

    if Multiply <= 1 then
    begin
      blockWidth := width div HorBlock;
      blockHeight := height div VerBlock;
    end
    else
    begin
      blockWidth := Trunc(width div (HorBlock * Multiply)) * Multiply;
      blockHeight := Trunc(height div (VerBlock * Multiply)) * Multiply;
    end;
    for i := 0 to HorBlock * VerBlock - 1 do
    begin
      if (Matrix[i] < 0) or (Matrix[i] >= HorBlock * VerBlock) then
      begin
        LogAndFail(Format('Matrix[%d]=%d out of range', [i, Matrix[i]]));
        Exit;
      end;
      row := Matrix[i] div HorBlock;
      col := Matrix[i] mod HorBlock;
      dx1 := Trunc(col * blockWidth);
      dy1 := Trunc(row * blockHeight);
      dx2 := Trunc((col + 1) * blockWidth);
      dy2 := Trunc((row + 1) * blockHeight);
      row := i div HorBlock;
      col := i mod HorBlock;
      sx1 := Trunc(col * blockWidth);
      sy1 := Trunc(row * blockHeight);
      tileWidth := dx2 - dx1;
      tileHeight := dy2 - dy1;
      if (tileWidth <= 0) or (tileHeight <= 0) then
      begin
        Continue;
      end;
      flags := GetFlip(i);
      for k := 0 to tileHeight - 1 do
      begin
        srcRow := sy1 + k;
        if (flags and 2) <> 0 then
        begin
          srcRow := sy1 + (tileHeight - 1 - k);
        end;
        srcLine := srcIntf.GetDataLineStart(srcRow) + sx1 * 4;
        dstLine := dstIntf.GetDataLineStart(dy1 + k) + dx1 * 4;
        if (flags and 1) <> 0 then
        begin
          for x := 0 to tileWidth - 1 do
          begin
            Move((srcLine + (tileWidth - 1 - x) * 4)^, (dstLine + x * 4)^, 4);
          end;
        end
        else
        begin
          Move(srcLine^, dstLine^, tileWidth * 4);
        end;
      end;
    end;

    result.Bitmap.LoadFromIntfImage(dstIntf);
    output.Position := 0;
    output.Size := 0;
    result.SaveToStreamWithFileExt(output, ext);
  finally
    dstIntf.Free;
    srcIntf.Free;
    FreeAndNil(tmpBitmap);
    memStream.Free;
    result.Free;
    image.Free;
  end;
end;

end.