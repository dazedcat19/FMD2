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

uses Math;

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
  blockWidth, blockHeight: Extended;
  i, row, col: Integer;
  x1, y1: Integer;
  dstrect, srcrect: TRect;
  ext: String = 'jpg';
begin
  if not Assigned(input) or not Assigned(output) then Exit;
  Assert(Assigned(Matrix), 'Matrix is not set');
  Assert(Length(Matrix) >= HorBlock * VerBlock, 'Invalid matrix size');
  image := TPicture.Create;
  result := TPicture.Create;
  try
    image.LoadFromStream(input);
    if image.Graphic is TPortableNetworkGraphic then ext := 'png';
    result.Bitmap.SetSize(image.Width, image.Height);
    if Multiply <= 1 then begin
      blockWidth := float(image.Width) / HorBlock;
      blockHeight := float(image.Height) / VerBlock;
    end
    else begin
      blockWidth := trunc(float(image.Width) / (HorBlock * Multiply)) * Multiply;
      blockHeight := trunc(float(image.Height) / (VerBlock * Multiply)) * Multiply;
    end;
    for i := 0 to HorBlock * VerBlock - 1 do begin
      row := floor(float(Matrix[i]) / VerBlock);
      col := Matrix[i] - row * HorBlock;
      x1 := trunc(col * blockWidth);
      y1 := trunc(row * blockHeight);
      dstrect := Rect(x1, y1, Trunc(x1 + blockWidth), Trunc(y1 + blockHeight));
      row := floor(float(i) / HorBlock);
      col := i - row * HorBlock;
      x1 := trunc(col * blockWidth);
      y1 := trunc(row * blockHeight);
      srcrect := Rect(x1, y1, Trunc(x1 + blockWidth), Trunc(y1 + blockHeight));
      result.Bitmap.Canvas.CopyRect(dstrect, image.Bitmap.Canvas, srcrect);
    end;
    output.Position := 0;
    output.Size := 0;
    result.SaveToStreamWithFileExt(output, ext);
  finally
    result.Free;
    image.Free;
  end;
end;

end.

