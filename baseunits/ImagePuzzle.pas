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
    FRotations: TIntegerDynArray;
    FColWidths: TIntegerDynArray;
    FRowHeights: TIntegerDynArray;
    FUnevenGrid: Boolean;
    function GetFlip(const Index: Integer): Integer;
    procedure Ensure32bit(var AIntf: TLazIntfImage);
    procedure ResetCells;
    procedure CopyTileRotated(ASrc, ADst: TLazIntfImage;
      ASrcX, ASrcY, ASrcW, ASrcH, ADstX, ADstY, ARotation, AFlips: Integer);
  public
    constructor Create(horBlockCount, verBlockCount: Integer); overload;
    constructor Create(const AColWidths, ARowHeights: array of Integer); overload;
    function SetUnevenGrid(const AColWidths, ARowHeights: array of Integer): Boolean;
    procedure DeScramble(input, output: TStream);
    property HorBlock: Integer read FHorBlock;
    property VerBlock: Integer read FVerBlock;
    property Multiply: Integer read FMultiply write FMultiply default 1;
    property Matrix: TIntegerDynArray read FMatrix;
    property Flips: TIntegerDynArray read FFlips;
    property Rotations: TIntegerDynArray read FRotations;
    property ColWidths: TIntegerDynArray read FColWidths;
    property RowHeights: TIntegerDynArray read FRowHeights;
    property UnevenGrid: Boolean read FUnevenGrid;
  end;

implementation

uses Math, MemBitmap, webp, GraphType, MultiLog;

constructor TImagePuzzle.Create(horBlockCount, verBlockCount: Integer);
begin
  inherited Create;
  FMultiply := 1;
  FHorBlock := horBlockCount;
  FVerBlock := verBlockCount;
  if FHorBlock < 0 then FHorBlock := 0;
  if FVerBlock < 0 then FVerBlock := 0;
  FUnevenGrid := False;
  ResetCells;
end;

constructor TImagePuzzle.Create(const AColWidths, ARowHeights: array of Integer);
begin
  inherited Create;
  FMultiply := 1;
  FHorBlock := 0;
  FVerBlock := 0;
  FUnevenGrid := False;
  SetUnevenGrid(AColWidths, ARowHeights);
end;

procedure TImagePuzzle.ResetCells;
var i: Integer;
begin
  SetLength(FMatrix, FHorBlock * FVerBlock);
  SetLength(FFlips, FHorBlock * FVerBlock);
  SetLength(FRotations, FHorBlock * FVerBlock);
  for i := 0 to High(FMatrix) do
  begin
    FMatrix[i] := i;
    FFlips[i] := 0;
    FRotations[i] := 0;
  end;
end;

function TImagePuzzle.SetUnevenGrid(const AColWidths, ARowHeights: array of Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (Length(AColWidths) = 0) or (Length(ARowHeights) = 0) then
  begin
    Logger.SendError(Self.ClassName + '.SetUnevenGrid: column widths or row heights are empty');
    Exit;
  end;
  for i := 0 to Length(AColWidths) - 1 do
  begin
    if AColWidths[i] <= 0 then
    begin
      Logger.SendError(Self.ClassName + Format('.SetUnevenGrid: invalid column width %d at index %d', [AColWidths[i], i]));
      Exit;
    end;
  end;
  for i := 0 to Length(ARowHeights) - 1 do
  begin
    if ARowHeights[i] <= 0 then
    begin
      Logger.SendError(Self.ClassName + Format('.SetUnevenGrid: invalid row height %d at index %d', [ARowHeights[i], i]));
      Exit;
    end;
  end;
  FHorBlock := Length(AColWidths);
  FVerBlock := Length(ARowHeights);
  SetLength(FColWidths, FHorBlock);
  SetLength(FRowHeights, FVerBlock);
  for i := 0 to FHorBlock - 1 do
    FColWidths[i] := AColWidths[i];
  for i := 0 to FVerBlock - 1 do
    FRowHeights[i] := ARowHeights[i];
  FUnevenGrid := True;
  ResetCells;
  Result := True;
end;

procedure TImagePuzzle.CopyTileRotated(ASrc, ADst: TLazIntfImage;
  ASrcX, ASrcY, ASrcW, ASrcH, ADstX, ADstY, ARotation, AFlips: Integer);
var
  r00, r01, r10, r11: Integer;
  a00, a01, a10, a11: Integer;
  sx, sy, t2x, t2y, t1x, t1y: Integer;
  bx, by, outW, outH: Integer;
  x, y, ox, oy: Integer;
  srcLine, dstLine: PByte;
begin
  case ARotation of
    1: begin r00 := 0;  r01 := 1;  r10 := -1; r11 := 0;  end;
    2: begin r00 := -1; r01 := 0;  r10 := 0;  r11 := -1; end;
    3: begin r00 := 0;  r01 := -1; r10 := 1;  r11 := 0;  end;
  else
       begin r00 := 1;  r01 := 0;  r10 := 0;  r11 := 1;  end;
  end;
  outW := ASrcW;
  outH := ASrcH;
  if (ARotation mod 2) = 1 then
  begin
    outW := ASrcH;
    outH := ASrcW;
  end;
  if (AFlips and 1) <> 0 then sx := -1 else sx := 1;
  if (AFlips and 2) <> 0 then sy := -1 else sy := 1;
  a00 := r00 * sx;
  a01 := r01 * sy;
  a10 := r10 * sx;
  a11 := r11 * sy;
  if (AFlips and 1) <> 0 then t2x := outW else t2x := 0;
  if (AFlips and 2) <> 0 then t2y := outH else t2y := 0;
  case ARotation of
    1: begin t1x := 0;    t1y := outW; end;
    2: begin t1x := outW; t1y := outH; end;
    3: begin t1x := outH; t1y := 0;    end;
  else
       begin t1x := 0;    t1y := 0;    end;
  end;
  bx := r00 * t2x + r01 * t2y + t1x;
  if a00 + a01 < 0 then Dec(bx);
  by := r10 * t2x + r11 * t2y + t1y;
  if a10 + a11 < 0 then Dec(by);
  for y := 0 to ASrcH - 1 do
  begin
    srcLine := ASrc.GetDataLineStart(ASrcY + y) + ASrcX * 4;
    for x := 0 to ASrcW - 1 do
    begin
      ox := a00 * x + a01 * y + bx;
      oy := a10 * x + a11 * y + by;
      if (ox >= 0) and (ox < outW) and (oy >= 0) and (oy < outH)
        and (ADstX + ox < ADst.Width) and (ADstY + oy < ADst.Height) then
      begin
        dstLine := ADst.GetDataLineStart(ADstY + oy) + (ADstX + ox) * 4;
        Move((srcLine + x * 4)^, dstLine^, 4);
      end;
    end;
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
  dx1, dy1, sx1, sy1: Integer;
  tileWidth, tileHeight, srcRow, flags: Integer;
  width, height: Integer;
  srcLine, dstLine: PByte;
  ext: String = 'jpg';
  Uneven: Boolean;
  ColX, ColW, RowY, RowH: TIntegerDynArray;
  c, sumSize, rotation: Integer;

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

    Uneven := FUnevenGrid and (Length(FColWidths) > 0) and (Length(FRowHeights) > 0);
    SetLength(ColX, HorBlock);
    SetLength(ColW, HorBlock);
    SetLength(RowY, VerBlock);
    SetLength(RowH, VerBlock);
    if Uneven then
    begin
      sumSize := 0;
      for c := 0 to HorBlock - 1 do
      begin
        Inc(sumSize, FColWidths[c]);
      end;
      if sumSize <> width then
      begin
        LogAndFail(Format('uneven grid columns do not add up to image width: %d vs %d', [sumSize, width]));
        Exit;
      end;
      sumSize := 0;
      for c := 0 to VerBlock - 1 do
      begin
        Inc(sumSize, FRowHeights[c]);
      end;
      if sumSize <> height then
      begin
        LogAndFail(Format('uneven grid rows do not add up to image height: %d vs %d', [sumSize, height]));
        Exit;
      end;
      col := 0;
      for c := 0 to HorBlock - 1 do
      begin
        ColX[c] := col;
        ColW[c] := FColWidths[c];
        Inc(col, ColW[c]);
      end;
      row := 0;
      for c := 0 to VerBlock - 1 do
      begin
        RowY[c] := row;
        RowH[c] := FRowHeights[c];
        Inc(row, RowH[c]);
      end;
    end
    else
    begin
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
      for c := 0 to HorBlock - 1 do
      begin
        ColX[c] := Trunc(c * blockWidth);
        ColW[c] := Trunc((c + 1) * blockWidth) - ColX[c];
      end;
      for c := 0 to VerBlock - 1 do
      begin
        RowY[c] := Trunc(c * blockHeight);
        RowH[c] := Trunc((c + 1) * blockHeight) - RowY[c];
      end;
    end;
    for i := 0 to HorBlock * VerBlock - 1 do
    begin
      if (Matrix[i] < 0) or (Matrix[i] >= HorBlock * VerBlock) then
      begin
        LogAndFail(Format('Matrix[%d]=%d out of range', [i, Matrix[i]]));
        Exit;
      end;
      row := i div HorBlock;
      col := i mod HorBlock;
      sx1 := ColX[col];
      sy1 := RowY[row];
      row := Matrix[i] div HorBlock;
      col := Matrix[i] mod HorBlock;
      dx1 := ColX[col];
      dy1 := RowY[row];
      tileWidth := ColW[col];
      tileHeight := RowH[row];
      if (tileWidth <= 0) or (tileHeight <= 0) then
      begin
        Continue;
      end;
      if tileWidth > ColW[i mod HorBlock] then
      begin
        tileWidth := ColW[i mod HorBlock];
      end;
      if tileHeight > RowH[i div HorBlock] then
      begin
        tileHeight := RowH[i div HorBlock];
      end;
      flags := GetFlip(i);
      rotation := 0;
      if i < Length(FRotations) then
      begin
        rotation := ((FRotations[i] mod 4) + 4) mod 4;
      end;
      if rotation = 0 then
      begin
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
      end
      else
      begin
        CopyTileRotated(srcIntf, dstIntf, sx1, sy1,
          ColW[i mod HorBlock], RowH[i div HorBlock],
          dx1, dy1, rotation, flags);
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