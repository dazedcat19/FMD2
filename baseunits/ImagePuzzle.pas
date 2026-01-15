unit ImagePuzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Types;

type
  TImagePuzzle = class
  private
    FHorBlock, FVerBlock, FMultiply: Integer;
    FMatrix: TIntegerDynArray;
  public
    constructor Create(horBlockCount, verBlockCount: Integer);
    procedure DeScramble(input, output: TStream);
    property HorBlock: Integer read FHorBlock;
    property VerBlock: Integer read FVerBlock;
    property Multiply: Integer read FMultiply write FMultiply default 1;
    property Matrix: TIntegerDynArray read FMatrix;
  end;

implementation

uses Math, MemBitmap, webp;

constructor TImagePuzzle.Create(horBlockCount, verBlockCount: Integer);
var i: Integer;
begin
  FHorBlock := horBlockCount;
  FVerBlock := verBlockCount;
  SetLength(FMatrix, FHorBlock * FVerBlock);
  for i := 0 to High(FMatrix) do
    FMatrix[i] := i;
end;

procedure TImagePuzzle.DeScramble(input, output: TStream);
var
  image, result: TPicture;
  memStream: TMemoryStream;
  tmpMemBitmap: TMemBitmap;
  y: Integer;
  blockWidth, blockHeight: Double;
  iBlockWidth, iBaseHeight, iRemainder: Integer;
  destY, destH, srcY, srcH: Integer;
  i, row, col: Integer;
  x1, y1, x2, y2: Integer;
  dstrect, srcrect: TRect;
  ext: String = 'jpg';
begin
  if not Assigned(input) or not Assigned(output) then Exit;
  Assert(Assigned(Matrix), 'Matrix is not set');
  Assert(Length(Matrix) >= HorBlock * VerBlock, 'Invalid matrix size');
  image := TPicture.Create;
  result := TPicture.Create;
  memStream := TMemoryStream.Create;
  try
    memStream.LoadFromStream(input);
    memStream.Position := 0;
    tmpMemBitmap := WebPToMemBitmap(memStream);
    if Assigned(tmpMemBitmap) then
    begin
      ext := 'png';
      image.Bitmap.SetSize(tmpMemBitmap.Width, tmpMemBitmap.Height);
      image.Bitmap.PixelFormat := pf32bit;
      for y := 0 to tmpMemBitmap.Height - 1 do
        Move(tmpMemBitmap.ScanLine[y]^, image.Bitmap.ScanLine[y]^, tmpMemBitmap.Width * 4);
      tmpMemBitmap.Free;
    end
    else
    begin
      memStream.Position := 0;
      image.LoadFromStream(memStream);
      if image.Graphic is TPortableNetworkGraphic then
        ext := 'png';
    end;
    result.Bitmap.SetSize(image.Width, image.Height);
    if HorBlock = 1 then
    begin
      iBlockWidth := image.Width;
      iBaseHeight := image.Height div VerBlock;
      iRemainder := image.Height mod VerBlock;
      for i := 0 to VerBlock - 1 do 
      begin
        if i = 0 then
        begin
           destH := iBaseHeight + iRemainder;
           destY := 0;
        end
        else
        begin
           destH := iBaseHeight;
           destY := (iBaseHeight + iRemainder) + (i - 1) * iBaseHeight;
        end;
        dstrect := Rect(0, destY, iBlockWidth, destY + destH);
        row := Matrix[i];
        if row = VerBlock - 1 then
        begin
           srcH := iBaseHeight + iRemainder;
           srcY := image.Height - srcH;
        end
        else
        begin
           srcH := iBaseHeight;
           srcY := row * iBaseHeight;
        end;
        srcrect := Rect(0, srcY, iBlockWidth, srcY + srcH);
        result.Bitmap.Canvas.CopyRect(dstrect, image.Bitmap.Canvas, srcrect);
      end;
    end
    else
    begin
      if Multiply <= 1 then begin
        blockWidth := image.Width / HorBlock;
        blockHeight := image.Height / VerBlock;
      end
      else begin
        blockWidth := Trunc(image.Width / (HorBlock * Multiply)) * Multiply;
        blockHeight := Trunc(image.Height / (VerBlock * Multiply)) * Multiply;
      end;
      for i := 0 to HorBlock * VerBlock - 1 do begin
        row := Matrix[i] div HorBlock;
        col := Matrix[i] mod HorBlock;
        x1 := Trunc(col * blockWidth);
        y1 := Trunc(row * blockHeight);
        x2 := Trunc((col + 1) * blockWidth);
        y2 := Trunc((row + 1) * blockHeight);
        dstrect := Rect(x1, y1, x2, y2);
        row := i div HorBlock;
        col := i mod HorBlock;
        x1 := Trunc(col * blockWidth);
        y1 := Trunc(row * blockHeight);
        x2 := Trunc((col + 1) * blockWidth);
        y2 := Trunc((row + 1) * blockHeight);
        srcrect := Rect(x1, y1, x2, y2);
        result.Bitmap.Canvas.CopyRect(dstrect, image.Bitmap.Canvas, srcrect);
      end;
    end;
    output.Position := 0;
    output.Size := 0;
    result.SaveToStreamWithFileExt(output, ext);
  finally
    memStream.Free;
    result.Free;
    image.Free;
  end;
end;

end.