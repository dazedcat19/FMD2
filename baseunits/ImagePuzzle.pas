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

uses Math, webp, MemBitmap;

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
  
  // --- New variables required by this solution ---
  memStream: TMemoryStream;
  tmpMemBitmap: TMemBitmap;
  y: Integer;
  // --- End of new variables ---
  
begin
  if not Assigned(input) or not Assigned(output) then Exit;
  Assert(Assigned(Matrix), 'Matrix is not set');
  Assert(Length(Matrix) >= HorBlock * VerBlock, 'Invalid matrix size');
  
  image := TPicture.Create;
  result := TPicture.Create;
  memStream := TMemoryStream.Create;
  try
    // Load the entire input into memory first
    memStream.LoadFromStream(input);
    memStream.Position := 0;

    // --- Start of new WebP loading logic ---

    // 1. Try to load as WebP using your webp.pas unit
    // InitWebPModule is called automatically by the 'webp' unit's initialization
    tmpMemBitmap := WebPToMemBitmap(memStream); 

    if Assigned(tmpMemBitmap) then
    begin
      // It's WebP!
      // We must save as PNG since webp.pas doesn't have an encoder.
      ext := 'png'; 

      // Prepare the TPicture's bitmap to receive the data
      image.Bitmap.SetSize(tmpMemBitmap.Width, tmpMemBitmap.Height);
      image.Bitmap.PixelFormat := pf32bit; // WebPDecodeBGRAInto gives 32-bit data

      // Manually copy the pixel data from TMemBitmap to TBitmap
      // This is the bridge between MemBitmap.pas and Graphics.pas
      for y := 0 to tmpMemBitmap.Height - 1 do
        Move(tmpMemBitmap.ScanLine[y]^, image.Bitmap.ScanLine[y]^, tmpMemBitmap.Width * 4);

      tmpMemBitmap.Free;
    end
    else
    begin
      // It's not WebP. Let TPicture handle it (as JPG or PNG)
      memStream.Position := 0;
      image.LoadFromStream(memStream);
      
      // Keep the original PNG check
      if image.Graphic is TPortableNetworkGraphic then 
        ext := 'png';
    end;
    // --- End of new WebP loading logic ---

    // The rest of the descramble logic is IDENTICAL
    
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
      row := floor(float(Matrix[i]) / HorBlock);
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
    
    // This will now save 'png' if the input was WebP or PNG
    result.SaveToStreamWithFileExt(output, ext); 
    
  finally
    memStream.Free;
    result.Free;
    image.Free;
  end;
end;

end.

