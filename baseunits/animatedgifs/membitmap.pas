unit MemBitmap;
{
 /***************************************************************************
                                membitmap.pas
                                --------------
                       Easy-to-use memory bitmap 32-bit
                       8-bit for each channel, transparency
                       Channels in that order : B G R A

                       - Drawing primitives
                       - Resample
                       - Reference counter
                       - Drawing on LCL canvas
                       - Loading and saving images

 ***************************************************************************/

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, Graphics;

type
    PMemPixel = ^TMemPixel;
    TMemPixel = packed record blue,green,red,alpha: byte; end;

    TDrawMode = (dmSet, dmSetExceptTransparent, dmDrawWithTransparency);

    { TMemBitmap }

    TMemBitmap = class(TFPCustomImage)
    private
       FBitmap: TBitmap; //LCL bitmap object
       FRefCount: integer; //reference counter

       function CheckEmpty: boolean;
       function ResampleLarger(newWidth, newHeight: integer): TMemBitmap;
       function ResampleSmaller(newWidth, newHeight: integer): TMemBitmap;
       function GetHasTransparentPixels: boolean;
       function GetAverageColor: TColor;
    protected
       {Pixel data}
       FData: PMemPixel;
       FWidth,FHeight,FNbPixels: integer;
       FRebuildBmp: Boolean; //if image has changed

       function GetScanLine(y: integer): PMemPixel;
       procedure RebuildBitmap;
       function GetBitmap: TBitmap;
       procedure Init;
       {TFPCustomImage}
       procedure SetInternalColor (x,y:integer; const Value:TFPColor); override;
       function GetInternalColor (x,y:integer) : TFPColor; override;
       procedure SetInternalPixel (x,y:integer; Value:integer); override;
       function GetInternalPixel (x,y:integer) : integer; override;
    public
       {Reference counter functions}
       function NewReference: TMemBitmap;
       procedure FreeReference;
       function GetUnique: TMemBitmap;

       {TFPCustomImage}
       constructor Create(AWidth,AHeight: integer); override;
       procedure SetSize (AWidth, AHeight : integer); override;
       procedure SaveToFile(const filename:String);

       {Loading functions}
       procedure Assign(Bitmap: TBitmap); overload;
       procedure Assign(MemBitmap: TMemBitmap); overload;
       constructor Create(AFilename: string);
       destructor Destroy; override;

       {Drawing primitives}
       procedure DrawPixels(c: TMemPixel; start,count: integer); overload;
       procedure SetPixel(x,y: integer; c: TColor); overload;
       procedure SetPixel(x,y: integer; c: TMemPixel); overload;
       procedure DrawPixel(x,y: integer; c: TMemPixel); overload;
       procedure AlphaPixel(x,y: integer; alpha: byte);
       class procedure DrawPixel(dest: PMemPixel; c: TMemPixel); overload; inline;
       function GetPixel(x,y: integer): TMemPixel;
       function GetPixelCycle(x,y: integer): TMemPixel;

       procedure AlphaHorizLine(x,y,x2: integer; alpha: byte);
       procedure SetHorizLine(x,y,x2: integer; c: TMemPixel);
       procedure DrawHorizLine(x,y,x2: integer; c: TMemPixel);
       procedure SetVertLine(x,y,y2: integer; c: TMemPixel);
       procedure DrawVertLine(x,y,y2: integer; c: TMemPixel);
       procedure AlphaVertLine(x,y,y2: integer; alpha: byte);
       procedure Rectangle(x,y,x2,y2: integer; c: TMemPixel; mode: TDrawMode);
       procedure Rectangle(x,y,x2,y2: integer; c: TColor);

       procedure FillRect(x,y,x2,y2: integer; c: TMemPixel; mode: TDrawMode);
       procedure FillRect(x,y,x2,y2: integer; c: TColor);
       procedure AlphaFillRect(x,y,x2,y2: integer; alpha:byte);

       procedure Fill(c: TColor); overload;
       procedure Fill(c: TMemPixel); overload;
       procedure Fill(c: TMemPixel; start,count: integer); overload;
       procedure AlphaFill(alpha: byte); overload;
       procedure AlphaFill(alpha: byte; start,count: integer); overload;
       procedure ReplaceColor(before,after: TColor); overload;
       procedure ReplaceColor(before,after: TMemPixel); overload;

       {LCL drawing functions}
       procedure InvalidateBitmap;
       procedure Draw(ACanvas: TCanvas; x,y: integer);
       procedure Draw(ACanvas: TCanvas; Rect: TRect);
       procedure GetImage(Canvas: TCanvas; x,y: integer; defaultColor: TColor);
       procedure DrawPartial(ARect: TRect; Canvas: TCanvas; x,y: integer);

       {Mem bitmap functions}
       procedure PutImage(x,y: integer; bmp: TMemBitmap; mode: TDrawMode);
       function Duplicate: TMemBitmap;
       function Resample(NewWidth, NewHeight: Integer): TMemBitmap;
       procedure VerticalFlip;

       property Data: PMemPixel read FData;
       property Width: Integer read FWidth;
       property Height: Integer read FHeight;
       property NbPixels: Integer read FNbPixels;
       property Empty: boolean read CheckEmpty;

       property ScanLine[y: integer]: PMemPixel read GetScanLine;
       property RefCount: integer read FRefCount;
       property Bitmap: TBitmap read GetBitmap; //don't forget to call InvalidateBitmap before
                                                //if you changed something
       property HasTransparentPixels: boolean read GetHasTransparentPixels;
       property AverageColor: TColor read GetAverageColor;
    end;

const
     MemPixelTransparent : TMemPixel = (blue:0; green:0; red:0; alpha:0);

function MemPixel(red,green,blue,alpha: byte): TMemPixel; overload;
function MemPixel(red,green,blue: byte): TMemPixel; overload;
function MemPixel(color: TColor): TMemPixel; overload;
function MemPixelToColor(c: TMemPixel): TColor;
operator = (const c1,c2:TMemPixel) : boolean;

implementation

uses FPWritePng, GraphType, LCLIntf, LCLType
{$IFDEF LCLgtk2}
  {$DEFINE gtkbugfix}
  ,gdk2,gtk2Def, gtk2Proc
{$ENDIF}
{$IFDEF LCLgtk}
  {$DEFINE gtkbugfix}
  ,gdk,gtkDef, gtkProc
{$ENDIF}
;

function MemPixel(red, green, blue, alpha: byte): TMemPixel;
begin
  result.red := red;
  result.green := green;
  result.blue := blue;
  result.alpha := alpha;
end;

function MemPixel(red, green, blue: byte): TMemPixel; overload;
begin
  result.red := red;
  result.green := green;
  result.blue := blue;
  result.alpha := 255;
end;

function MemPixel(color: TColor): TMemPixel; overload;
begin
  result.red := color;
  result.green := color shr 8;
  result.blue := color shr 16;
  result.alpha := 255;
end;

function MemPixelToColor(c: TMemPixel): TColor;
begin
  result := c.red + (c.green shl 8) + (c.blue shl 16);
end;

operator=(const c1, c2: TMemPixel): boolean;
begin
  if (c1.alpha=0) and (c2.alpha=0) then result := true else
    result := (c1.alpha=c2.alpha) and (c1.red=c2.red) and (c1.green=c2.green) and (c1.blue=c2.blue);
end;

{ TMemBitmap }

function TMemBitmap.CheckEmpty: boolean;
var i: integer; p: PMemPixel;
begin
     p := data;
     for i := NbPixels-1 downto 0 do
     begin
          if p^.alpha <> 0 then
          begin
             result := false;
             exit;
          end;
          inc(p);
     end;
     result := true;
end;

function TMemBitmap.GetScanLine(y: integer): PMemPixel;
begin
  if (y < 0) or (y >= height) then
    raise ERangeError.Create('Scanline: out of bounds') else
  begin
    result := data;
    inc(result, width*y);
  end;
end;

constructor TMemBitmap.Create(AWidth, AHeight: integer);
begin
     Init;
     inherited Create(AWidth,AHeight);
end;

constructor TMemBitmap.Create(AFilename: string);
begin
     LoadFromFile(Afilename);
end;

procedure TMemBitmap.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth = Width) and (AHeight = Height) then exit;
  inherited SetSize(AWidth, AHeight);
  if AWidth < 0 then AWidth := 0;
  if AHeight < 0 then AHeight := 0;
  FWidth := AWidth;
  FHeight := AHeight;
  FNbPixels := AWidth*AHeight;
  if FNbPixels<0 then raise EOutOfMemory.Create('Image too big');
  ReAllocMem(FData,NbPixels*sizeof(TMemPixel));
  if (NbPixels>0) and (FData=nil) then
    raise EOutOfMemory.Create('TMemBitmap: Not enough memory');
end;

procedure TMemBitmap.SaveToFile(const filename:String);
var ext: string; writer: TFPCustomImageWriter;
    pngWriter : TFPWriterPNG;
begin
     ext := AnsiLowerCase(ExtractFileExt(filename));

     if ext='.png' then
     begin
       pngWriter := TFPWriterPNG.Create;
       pngWriter.Indexed := false;
       pngWriter.UseAlpha := HasTransparentPixels;
       writer := pngWriter;
     end else
       writer := nil;

     if writer<> nil then
     begin
       inherited SaveToFile(Filename,writer);
       writer.free;
     end else
       inherited SaveToFile(Filename);
end;

procedure TMemBitmap.Assign(Bitmap: TBitmap);
begin
  SetSize(Bitmap.width,bitmap.height);
  GetImage(Bitmap.Canvas,0,0,clBlack);
end;

procedure TMemBitmap.Assign(MemBitmap: TMemBitmap);
begin
  SetSize(MemBitmap.Width,MemBitmap.height);
  PutImage(0,0,MemBitmap,dmSet);
end;

destructor TMemBitmap.Destroy;
begin
  freemem(FData);
  FBitmap.Free;
  inherited Destroy;
end;

procedure TMemBitmap.SetPixel(x, y: integer; c: TMemPixel);
begin
  if (x<0) or (y<0) or (x>=width) or (y>=height) then exit;
  (Scanline[y]+x)^ := c;
end;

procedure TMemBitmap.SetPixel(x, y: integer; c: TColor);
var p: PByte;
begin
    if (x<0) or (y<0) or (x>=width) or (y>=height) then exit;
    p := PByte(Scanline[y]+x);
    p^ := c shr 16; inc(p);
    p^ := c shr 8; inc(p);
    p^ := c; inc(p);
    p^ := 255;
end;

procedure TMemBitmap.DrawPixel(x, y: integer; c: TMemPixel);
begin
    if (x<0) or (y<0) or (x>=width) or (y>=height) then exit;
    DrawPixel(Scanline[y]+x, c);
end;

procedure TMemBitmap.AlphaPixel(x, y: integer; alpha: byte);
begin
    if (x<0) or (y<0) or (x>=width) or (y>=height) then exit;
    (Scanline[y]+x)^.alpha := alpha;
end;

class procedure TMemBitmap.DrawPixel(dest: PMemPixel; c: TMemPixel);
var p: PByte; a1f, a2f, a12, a12m: cardinal;
begin
    if c.alpha = 0 then exit;

    a12 := 65025 - (255-dest^.alpha)*(255-c.alpha);
    a12m := a12 shr 1;

    a1f := dest^.alpha*(255-c.alpha);
    a2f := c.alpha* 255;

    p := PByte(dest);
    p^ := (dest^.blue*a1f + c.blue*a2f + a12m ) div a12; inc(p);
    p^ := (dest^.green*a1f + c.green*a2f + a12m ) div a12; inc(p);
    p^ := (dest^.red*a1f + c.red*a2f + a12m ) div a12; inc(p);
    p^ := (a12 + a12 shr 7) shr 8;
end;
{var p: PByte; a1,r1,g1,b1, a2,r2,g2,b2, a1f, a2f, a12: single;
begin
    if c.alpha = 0 then exit;

    a2 := c.alpha/255;
    r2 := c.red/255;
    g2 := c.green/255;
    b2 := c.blue/255;

    a1 := dest^.alpha/255;
    r1 := dest^.red/255;
    g1 := dest^.green/255;
    b1 := dest^.blue/255;

    a12 := 1 - (1-a1)*(1-a2);

    a1f := a1*(1-a2)/a12* 255;
    a2f := a2/a12* 255;

    p := PByte(dest);
    p^ := round( b1*a1f + b2*a2f ); inc(p);
    p^ := round( g1*a1f + g2*a2f ); inc(p);
    p^ := round( r1*a1f + r2*a2f ); inc(p);
    p^ := round( a12 * 255);
end;}

function TMemBitmap.GetPixel(x, y: integer): TMemPixel;
begin
  if (x<0) or (y<0) or (x>=width) or (y>=height) then result := MemPixelTransparent else
   result := (Scanline[y]+x)^;
end;

function TMemBitmap.GetPixelCycle(x, y: integer): TMemPixel;
begin
  if (Width=0) or (Height=0) then result := MemPixelTransparent else
  begin
    x := x mod Width;
    if x<0 then inc(x,width);
    y := y mod Height;
    if y<0 then inc(y,height);
    result := (Scanline[y]+x)^;
  end;
end;

procedure TMemBitmap.AlphaHorizLine(x, y, x2: integer; alpha: byte);
var temp: integer;
begin
     if (x2<x) then
     begin
          temp := x;
          x := x2;
          x2 := temp;
     end;
     if (x >= width) or (x2 < 0) then exit;
     if x < 0 then x := 0;
     if x2 >= width then x2 := width-1;
     AlphaFill(alpha, y*width+x, x2-x+1);
end;

procedure TMemBitmap.InvalidateBitmap;
begin
  FRebuildBmp := True;
end;

procedure TMemBitmap.RebuildBitmap;
var RawImage : TRawImage; ABitmap, AMask: HBitmap;
begin
     if FBitmap = nil then FBitmap := TBitmap.Create;

     if Empty then
     begin
       FBitmap.Height := 0;
       FBitmap.Width := 0;
     end else
     begin
       RawImage.Init;
       RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(Width,Height);
       RawImage.Data:= PByte(data);
       RawImage.DataSize:= nbPixels*sizeof(TMemPixel);
       RawImage_CreateBitmaps(RawImage, ABitmap,AMask,False);
       FBitmap.Handle := ABitmap;
       FBitmap.MaskHandle := AMask;
     end;
end;

function TMemBitmap.GetBitmap: TBitmap;
begin
   if FRebuildBmp or (FBitmap = nil) then
   begin
     RebuildBitmap;
     FRebuildBmp := false;
   end;
   result := FBitmap;
end;

procedure TMemBitmap.Init;
begin
  FRefCount := 1;
  FBitmap := nil;
  FData := nil;
  FWidth := 0;
  FHeight := 0;
end;

procedure TMemBitmap.SetInternalColor(x, y: integer; const Value: TFPColor);
var p: PByte;
begin
    if (x<0) or (y<0) or (x>=width) or (y>=height) then exit;
    p := PByte(Scanline[y]+x);
    p^ := Value.blue shr 8; inc(p);
    p^ := Value.green shr 8; inc(p);
    p^ := Value.red shr 8; inc(p);
    p^ := Value.alpha shr 8;
end;

function TMemBitmap.GetInternalColor(x, y: integer): TFPColor;
var p: PByte; v: byte;
begin
    if (x<0) or (y<0) or (x>=width) or (y>=height) then exit;
    p := PByte(Scanline[y]+x);
    v := p^; result.blue := v shl 8+v; inc(p);
    v := p^; result.green := v shl 8+v; inc(p);
    v := p^; result.red := v shl 8+v; inc(p);
    v := p^; result.alpha := v shl 8+v;
end;

procedure TMemBitmap.SetInternalPixel(x, y: integer; Value: integer);
var p: PByte; c: TFPColor;
begin
    if (x<0) or (y<0) or (x>=width) or (y>=height) then exit;
    c := Palette.Color[Value];
    p := PByte(Scanline[y]+x);
    p^ := c.blue shr 8; inc(p);
    p^ := c.green shr 8; inc(p);
    p^ := c.red shr 8; inc(p);
    p^ := c.alpha shr 8;
end;

function TMemBitmap.GetInternalPixel(x, y: integer): integer;
var p: PByte; v: byte; c: TFPColor;
begin
    if (x<0) or (y<0) or (x>=width) or (y>=height) then exit;
    p := PByte(Scanline[y]+x);
    v := p^; c.blue := v shl 8+v; inc(p);
    v := p^; c.green := v shl 8+v; inc(p);
    v := p^; c.red := v shl 8+v; inc(p);
    v := p^; c.alpha := v shl 8+v;
    result := palette.IndexOf(c);
end;

function TMemBitmap.NewReference: TMemBitmap;
begin
  Inc(FRefCount);
  result := self;
end;

procedure TMemBitmap.FreeReference;
begin
  if self=nil then exit;

  if FRefCount > 0 then
  begin
       Dec(FRefCount);
       if FRefCount = 0 then
       begin
         self.Destroy;
       end;
  end;
end;

function TMemBitmap.GetUnique: TMemBitmap;
begin
  if FRefCount > 1 then
  begin
    Dec(FRefCount);
    result := self.Duplicate;
  end else
    result := self;
end;

procedure TMemBitmap.Draw(ACanvas: TCanvas; x, y: integer);
begin
     if self=nil then exit;
     ACanvas.Draw(x,y,Bitmap);
end;

procedure TMemBitmap.Draw(ACanvas: TCanvas; Rect: TRect);
begin
     if self=nil then exit;
     ACanvas.StretchDraw(Rect,Bitmap);
end;

procedure TMemBitmap.DrawHorizLine(x, y, x2: integer; c: TMemPixel);
var temp: integer;
begin
     if (x2<x) then
     begin
          temp := x;
          x := x2;
          x2 := temp;
     end;
     if (x >= width) or (x2 < 0) then exit;
     if x < 0 then x := 0;
     if x2 >= width then x2 := width-1;
     DrawPixels(c, y*width+x, x2-x+1);
end;

procedure TMemBitmap.SetHorizLine(x, y, x2: integer; c: TMemPixel);
var temp: integer;
begin
     if (x2<x) then
     begin
          temp := x;
          x := x2;
          x2 := temp;
     end;
     if (x >= width) or (x2 < 0) then exit;
     if x < 0 then x := 0;
     if x2 >= width then x2 := width-1;
     Fill(c, y*width+x, x2-x+1);
end;

procedure TMemBitmap.SetVertLine(x, y, y2: integer; c: TMemPixel);
var temp,n: integer; p: PMemPixel;
begin
     if (y2<y) then
     begin
          temp := y;
          y := y2;
          y2 := temp;
     end;
     if (y >= height) or (y2 < 0) or (x < 0) or (x >= width) then exit;
     if y < 0 then y := 0;
     if y2 >= height then y2 := height-1;
     p := scanline[y]+x;
     for n := y2-y downto 0 do
     begin
       p^ := c;
       inc(p,width);
     end;
end;

procedure TMemBitmap.DrawVertLine(x, y, y2: integer; c: TMemPixel);
var temp,n: integer; p: PMemPixel;
begin
     if (y2<y) then
     begin
          temp := y;
          y := y2;
          y2 := temp;
     end;
     if (y >= height) or (y2 < 0) or (x < 0) or (x >= width) then exit;
     if y < 0 then y := 0;
     if y2 >= height then y2 := height-1;
     p := scanline[y]+x;
     for n := y2-y downto 0 do
     begin
       DrawPixel(p,c);
       inc(p,width);
     end;
end;

procedure TMemBitmap.AlphaVertLine(x, y, y2: integer; alpha: byte);
var temp,n: integer; p: PMemPixel;
begin
     if (y2<y) then
     begin
          temp := y;
          y := y2;
          y2 := temp;
     end;
     if (y >= height) or (y2 < 0) or (x < 0) or (x >= width) then exit;
     if y < 0 then y := 0;
     if y2 >= height then y2 := height-1;
     p := scanline[y]+x;
     for n := y2-y downto 0 do
     begin
       p^.alpha := alpha;
       inc(p,width);
     end;
end;

procedure TMemBitmap.Rectangle(x, y, x2, y2: integer; c: TMemPixel;
  mode: TDrawMode);
var temp: integer;
begin
  if (x>x2) then begin  temp := x; x := x2; x2 := temp;  end;
  if (y>y2) then begin  temp := y; y := y2; y2 := temp;  end;
  if (x2-x<=1) or (y2-y<=1) then exit;
  case mode of
  dmDrawWithTransparency:
    begin
      DrawHorizLine(x,y,x2-1, c);
      DrawHorizLine(x,y2-1,x2-1, c);
      if y2-y > 2 then
      begin
        DrawVertLine(x,y+1,y2-2, c);
        DrawVertLine(x2-1,y+1,y2-2, c);
      end;
    end;
  dmSet:
    begin
      SetHorizLine(x,y,x2-1, c);
      SetHorizLine(x,y2-1,x2-1, c);
      if y2-y > 2 then
      begin
        SetVertLine(x,y+1,y2-2, c);
        SetVertLine(x2-1,y+1,y2-2, c);
      end;
    end;
  dmSetExceptTransparent: if (c.alpha <> 0) then Rectangle(x,y,x2,y2,c,dmSet);
  end;
end;

procedure TMemBitmap.Rectangle(x, y, x2, y2: integer; c: TColor);
begin
  Rectangle(x,y,x2,y2,MemPixel(c),dmSet);
end;

procedure TMemBitmap.FillRect(x, y, x2, y2: integer; c: TMemPixel;
  mode: TDrawMode);
var temp,yb: integer;
begin
  if (x>x2) then begin  temp := x; x := x2; x2 := temp;  end;
  if (y>y2) then begin  temp := y; y := y2; y2 := temp;  end;
  if (x2-x<=0) or (y2-y<=0) then exit;
  case mode of
  dmDrawWithTransparency:
    for yb := y to y2-1 do
      DrawHorizLine(x,yb,x2-1, c);
  dmSet:
    for yb := y to y2-1 do
      SetHorizLine(x,yb,x2-1, c);
  dmSetExceptTransparent: if (c.alpha <> 0) then FillRect(x,y,x2,y2,c,dmSet);
  end;
end;

procedure TMemBitmap.FillRect(x, y, x2, y2: integer; c: TColor);
begin
  FillRect(x,y,x2,y2,MemPixel(c),dmSet);
end;

procedure TMemBitmap.AlphaFillRect(x, y, x2, y2: integer; alpha: byte);
var temp,yb: integer;
begin
  if (x>x2) then begin  temp := x; x := x2; x2 := temp;  end;
  if (y>y2) then begin  temp := y; y := y2; y2 := temp;  end;
  if (x2-x<=0) or (y2-y<=0) then exit;
  for yb := y to y2-1 do
    AlphaHorizLine(x,yb,x2-1, alpha);
end;

procedure TMemBitmap.Fill(c: TColor);
begin
  Fill(MemPixel(c));
end;

procedure TMemBitmap.Fill(c: TMemPixel);
begin
     Fill(c, 0, width*height);
end;

procedure TMemBitmap.Fill(c: TMemPixel; start, count: integer);
var p: PMemPixel;
begin
     if start < 0 then
     begin
          count += start;
          start := 0;
     end;
     if start >= nbPixels then exit;
     if start+count > nbPixels then count := nbPixels-start;
     p := (Data+start);
     while count > 0 do
     begin
        p^ := c;
        inc(p);
        dec(count);
     end;
end;

procedure TMemBitmap.AlphaFill(alpha: byte);
begin
  AlphaFill(alpha,0,NbPixels);
end;

procedure TMemBitmap.AlphaFill(alpha: byte; start, count: integer);
var p: PMemPixel;
begin
     if start < 0 then
     begin
          count += start;
          start := 0;
     end;
     if start >= nbPixels then exit;
     if start+count > nbPixels then count := nbPixels-start;
     p := (Data+start);
     while count > 0 do
     begin
        p^.alpha := alpha;
        inc(p);
        dec(count);
     end;
end;

procedure TMemBitmap.ReplaceColor(before, after: TColor);
const colorMask = $00FFFFFF;
var p: PLongWord;
    n: integer;
    beforeBGR, afterBGR: LongWord;
begin
     beforeBGR := (before and $FF shl 16) + (before and $FF00) + (before shr 16 and $FF);
     afterBGR := (after and $FF shl 16) + (after and $FF00) + (after shr 16 and $FF);
     p := PLongWord(Data);
     for n := NbPixels-1 downto 0 do
     begin
       if p^ and colorMask = beforeBGR then
         p^ := (p^ and not ColorMask) or afterBGR;
       inc(p);
     end;
end;

procedure TMemBitmap.ReplaceColor(before, after: TMemPixel);
var p: PMemPixel;
    n: integer;
begin
     p := Data;
     for n := NbPixels-1 downto 0 do
     begin
       if p^ = before then p^ := after;
       inc(p);
     end;
end;

procedure TMemBitmap.DrawPixels(c: TMemPixel; start, count: integer);
var p: PMemPixel;
begin
     if c.alpha = 0 then exit;

     if start < 0 then
     begin
          count += start;
          start := 0;
     end;
     if start >= nbPixels then exit;
     if start+count > nbPixels then count := nbPixels-start;
     p := Data+start;
     while count > 0 do
     begin
        DrawPixel(p,c);
        inc(p);
        dec(count);
     end;
end;

procedure TMemBitmap.PutImage(x, y: integer; bmp: TMemBitmap; mode: TDrawMode);
var x2,y2,yb,minxb,minyb,maxxb,ignoreleft,copycount,bmpwidth,i: integer; source,dest: PMemPixel;
begin
     bmpwidth := bmp.width;

     if (x>= width) or (y>= height) or (x< -bmpwidth) or (y < -bmp.height) then exit;

     x2 := x+bmpwidth-1;
     y2 := y+bmp.height-1;

     if y < 0 then minyb := 0 else minyb := y;
     if y2 >= height then y2 := height-1;

     if x < 0 then
     begin
       ignoreleft := -x;
       minxb := 0;
     end else
     begin
       ignoreleft := 0;
       minxb := x;
     end;
     if x2 >= width then maxxb := width-1 else maxxb := x2;

     copycount := maxxb-minxb+1;

     source := bmp.ScanLine[minyb-y]+ignoreleft;
     dest := Scanline[minyb]+minxb;

     case mode of
     dmSet: begin
              copycount *= sizeof(TMemPixel);
              for yb := minyb to y2 do
              begin
                 move(source^,dest^, copycount);
                 inc(source, bmpwidth);
                 inc(dest, width);
              end;
            end;
     dmSetExceptTransparent:
       for yb := minyb to y2 do
       begin
          for i := copycount-1 downto 0 do
          begin
            if source^.alpha = 255 then dest^ := source^;
            inc(dest);
            inc(source);
          end;
          inc(source, bmpwidth-copycount);
          inc(dest, width-copycount);
       end;
     dmDrawWithTransparency:
       for yb := minyb to y2 do
       begin
          for i := copycount-1 downto 0 do
          begin
            DrawPixel(dest,source^);
            inc(dest);
            inc(source);
          end;
          inc(source, bmpwidth-copycount);
          inc(dest, width-copycount);
       end;
     end;
end;

function TMemBitmap.Duplicate: TMemBitmap;
begin
  result := TMemBitmap.Create(width,height);
  result.PutImage(0,0,self,dmSet);
end;

procedure TMemBitmap.GetImage(Canvas: TCanvas; x, y: integer; defaultColor: TColor);
var bmp: TBitmap; xb,yb: integer;
    source: PByte; dest: PByte;
  {$IFDEF gtkbugfix}
    Ofs: TPoint;
    dcSource,dcDest : TGtkDeviceContext;
  {$ENDIF}
    rectSource,rectDest: TRect;

 function ClipRect: boolean;
 var delta: integer;
 begin
    if rectSource.Left < 0 then
    begin
      delta := -rectSource.left;
      inc(rectSource.Left,delta);
      inc(rectDest.Left,delta);
    end;
    if rectSource.Top < 0 then
    begin
      delta := -rectSource.Top;
      inc(rectSource.Top,delta);
      inc(rectDest.Top,delta);
    end;
    if rectSource.Right > Canvas.Width then
    begin
      delta := rectSource.Right-Canvas.Width;
      dec(rectSource.Right,delta);
      dec(rectDest.Right,delta);
    end;
    if rectSource.Bottom > Canvas.Height then
    begin
      delta := rectSource.Bottom-Canvas.Height;
      dec(rectSource.Bottom,delta);
      dec(rectDest.Bottom,delta);
    end;
    result := (rectSource.Right>rectSource.Left) and
      (rectSource.Bottom>rectSource.Top) and
      (rectDest.Right>rectDest.Left) and
      (rectDest.Bottom>rectDest.Top);
 end;

begin
     bmp := TBitmap.Create;
     bmp.PixelFormat:= pf24bit;
     bmp.width := width;
     bmp.height := height;
     bmp.Canvas.Brush.Color := defaultColor;
     bmp.Canvas.FillRect(0,0,width,height);
     rectSource := classes.rect(x,y,x+width,y+height);
     rectDest := classes.rect(0,0,width,height);

  {$IFDEF gtkbugfix}
     dcDest := TGtkDeviceContext(bmp.Canvas.handle);
     dcSource := TGtkDeviceContext(Canvas.Handle);
     if (dcSource <> nil) and (dcDest <> nil) then
     begin
       Ofs := dcSource.Offset;
       rectSource.Left += Ofs.X;
       rectSource.Top += Ofs.Y;
       rectSource.Right += Ofs.X;
       rectSource.Bottom += Ofs.Y;
       if ClipRect then
       begin
         if (dcDest.Drawable<>nil) and (dcDest.GC <> nil) and (dcSource.Drawable <> nil) then
           gdk_window_copy_area(dcDest.Drawable, dcDest.GC, rectDest.Left,rectDest.top, dcSource.Drawable,rectSource.Left,rectSource.Top, rectSource.Right-rectSource.Left,rectSource.Bottom-rectSource.Top);
       end;
     end;
  {$ELSE}
     if ClipRect then
       bmp.Canvas.CopyRect(rectDest,Canvas,rectSource);
  {$ENDIF}

     if bmp.RawImage.Description.BitsPerPixel = 32 then
     begin
       for yb := 0 to height-1 do
       begin
         if bmp.rawImage.Description.LineOrder = riloTopToBottom then
           source := bmp.rawImage.Data+ bmp.rawImage.Description.BytesPerLine*cardinal(yb) else
             source := bmp.rawImage.Data+ bmp.rawImage.Description.BytesPerLine*cardinal(height-1-yb);
         dest := pbyte(ScanLine[yb]);
         for xb := 0 to width-1 do
         begin
           PWord(dest)^ := PWord(source)^;
           inc(dest,2); inc(source, 2);
           dest^ := source ^;
           inc(dest); inc(source, 2);
           dest^:= 255;
           inc(dest);
         end;
       end;
     end else
     if bmp.RawImage.Description.BitsPerPixel = 24 then
     begin
       for yb := 0 to height-1 do
       begin
         if bmp.rawImage.Description.LineOrder = riloTopToBottom then
           source := bmp.rawImage.Data+ bmp.rawImage.Description.BytesPerLine*cardinal(yb) else
             source := bmp.rawImage.Data+ bmp.rawImage.Description.BytesPerLine*cardinal(height-1-yb);
         dest := pbyte(ScanLine[yb]);
         for xb := 0 to width-1 do
         begin
           PWord(dest)^ := PWord(source)^;
           inc(dest,2); inc(source, 2);
           dest^ := source ^;
           inc(dest); inc(source);
           dest^:= 255;
           inc(dest);
         end;
       end;
     end;

     bmp.Free;
end;

function TMemBitmap.ResampleSmaller(newWidth, newHeight: integer
  ): TMemBitmap;
const maxvalue= 255;
var
   x_dest,y_dest : integer;
   inc_x_src, mod_x_src, acc_x_src, inc_y_src, mod_y_src, acc_y_src : integer;
   x_src,y_src,prev_x_src,prev_y_src: integer;
   x_src2,y_src2: integer;

   xb,yb : integer;
   alpha,v1,v2,v3,v4: double;
   nb: integer;
   c: TMemPixel;
   pdest,psrc: PMemPixel;
begin
     result := TMemBitmap.Create(NewWidth, NewHeight);
     if (newWidth = 0) or (newHeight = 0) or (Width= 0) or (Height = 0) then exit;
     inc_x_src := width div newWidth;
     mod_x_src := width mod newWidth;
     inc_y_src := height div newHeight;
     mod_y_src := height mod newHeight;

     y_src := 0;
     acc_y_src := 0;
     PDest := result.ScanLine[0];
     for y_dest := 0 to newHeight-1 do
     begin
       prev_y_src := y_src;
       inc(y_src,inc_y_src);
       inc(acc_y_src,mod_y_src);
       if acc_y_src >= newHeight then
       begin
            dec(acc_y_src,newHeight);
            inc(y_src);
       end;

       x_src := 0;
       acc_x_src := 0;
       for x_dest := 0 to newWidth-1 do
       begin
           prev_x_src := x_src;
           inc(x_src,inc_x_src);
           inc(acc_x_src,mod_x_src);
           if acc_x_src >= newWidth then
           begin
                dec(acc_x_src,newWidth);
                inc(x_src);
           end;

           if x_src > prev_x_src then x_src2 := x_src-1 else x_src2 := x_src;
           if y_src > prev_y_src then y_src2 := y_src-1 else y_src2 := y_src;

           v1 := 0;
           v2 := 0;
           v3 := 0;
           v4 := 0;
           nb := 0;
           for yb := prev_y_src to y_src2 do
           begin
             PSrc := Scanline[yb]+prev_x_src;
             for xb := prev_x_src to x_src2 do
             begin
                  c := PSrc^; inc(PSrc);
                  alpha := c.alpha/maxvalue;
                  v1 += c.red*alpha;
                  v2 += c.green*alpha;
                  v3 += c.blue*alpha;
                  v4 += alpha;
                  inc(nb);
             end;
           end;

           if (v4<>0) and (nb <> 0) then
           begin
             c.red := round(v1/v4);
             c.green := round(v2/v4);
             c.blue := round(v3/v4);
             c.alpha := round(v4/nb*maxvalue);
           end else
           begin
             c.alpha := 0;
             c.red := 0;
             c.green := 0;
             c.blue := 0;
           end;
           PDest^ := c;
           inc(PDest);
       end;
     end;
end;

function TMemBitmap.GetHasTransparentPixels: boolean;
var p: PMemPixel; n: integer;
begin
     p := Data;
     for n := NbPixels-1 downto 0 do
     begin
       if p^.alpha <> 255 then
       begin
            result := true;
            exit;
       end;
       inc(p);
     end;
     result := false;
end;

function TMemBitmap.GetAverageColor: TColor;
var n: integer; p: PMemPixel;
    r,g,b,sum: double;
    alpha: double;
begin
     sum := 0;
     r := 0;
     g := 0;
     b := 0;
     p := Data;
     for n := NbPixels-1 downto 0 do
     begin
       alpha := p^.alpha/255;
       sum += alpha;
       r += p^.red*alpha;
       g += p^.green*alpha;
       b += p^.blue*alpha;
     end;
     if sum=0 then result := clNone else
      result := round(r/sum) + round(g/sum) shl 8 + round(b/sum) shl 16;
end;

function TMemBitmap.ResampleLarger(newWidth, newHeight: integer): TMemBitmap;
const maxvalue = 255;
var
   x_src,y_src : integer;
   inc_x_dest, mod_x_dest, acc_x_dest, inc_y_dest, mod_y_dest, acc_y_dest : integer;
   x_dest,y_dest,prev_x_dest,prev_y_dest: integer;
   x_dest2,y_dest2,{x_src2,}y_src2: integer;

   xb,yb : integer;
   cUpLeft,cUpRight,cLowLeft,cLowRight: TMemPixel;
   factX,factY,factAddX,factAddY,factCorrX,factCorrY: single;
   factUpLeft,factUpRight,factLowLeft,factLowRight: single;
   factUpLeftAlpha,factUpRightAlpha,factLowLeftAlpha,factLowRightAlpha: single;
   cur: TMemPixel;
   alphaUpLeft, alphaUpRight, alphaLowLeft, alphaLowRight: single;
   sumFactAlpha: single;
   PDest,PSrc,PSrc2 : PMemPixel;

   temp: TMemBitmap;

begin
     result := TMemBitmap.Create(NewWidth, NewHeight);
     if (newWidth = 0) or (newHeight = 0) then exit;

     if (width=1) and (height=1) then
     begin
       result.Fill(GetPixel(0,0));
       exit;
     end else
     if width=1 then
     begin
       temp := TMemBitmap.Create(2,Height);
       temp.PutImage(0,0,self,dmSet);
       temp.PutImage(1,0,self,dmSet);
       result := temp.ResampleLarger(newWidth,newHeight);
       temp.Free;
       exit;
     end else
     if height=1 then
     begin
       temp := TMemBitmap.Create(Width,2);
       temp.PutImage(0,0,self,dmSet);
       temp.PutImage(0,1,self,dmSet);
       result := temp.ResampleLarger(newWidth,newHeight);
       temp.Free;
       exit;
     end;

     inc_x_dest := newwidth div (Width-1);
     mod_x_dest := newwidth mod (Width-1);
     inc_y_dest := newheight div (Height-1);
     mod_y_dest := newheight mod (Height-1);

     y_dest := 0;
     acc_y_dest := 0;
     for y_src := 0 to Height-2 do
     begin
       prev_y_dest := y_dest;
       inc(y_dest,inc_y_dest);
       inc(acc_y_dest,mod_y_dest);
       if acc_y_dest >= Height then
       begin
            dec(acc_y_dest,Height);
            inc(y_dest);
       end;

       y_src2 := y_src+1;
       PSrc := Scanline[y_src];
       PSrc2 := Scanline[y_src2];
       cUpLeft := PSrc^; inc(PSrc);
       cLowLeft := PSrc2^; inc(PSrc2);

       x_dest := 0;
       acc_x_dest := 0;
       for x_src := 0 to Width-2 do
       begin
           prev_x_dest := x_dest;
           inc(x_dest,inc_x_dest);
           inc(acc_x_dest,mod_x_dest);
           if acc_x_dest >= Width then
           begin
                dec(acc_x_dest,Width);
                inc(x_dest);
           end;

           //x_src2 := x_src+1;
           if x_src < width-2 then
           begin
                x_dest2 := x_dest-1;
                factAddX := 1/(x_dest2-prev_x_dest+1);
           end else
           begin
                x_dest2 := newWidth-1;
                factAddX := 1/(x_dest2-prev_x_dest);
           end;
           if y_src < height-2 then
           begin
                y_dest2 := y_dest-1;
                factAddY := 1/(y_dest2-prev_y_dest+1);
           end else
           begin
                y_dest2 := newHeight-1;
                factAddY := 1/(y_dest2-prev_y_dest);
           end;

           cUpRight := PSrc^; inc(PSrc);
           cLowRight := PSrc2^; inc(PSrc2);

           factY := 0;
           for yb := prev_y_dest to y_dest2 do
           begin
             factX := 0;
             PDest := result.scanline[yb]+prev_x_dest;
             for xb := prev_x_dest to x_dest2 do
             begin
                  factCorrX := 0.5-cos(factX*Pi)/2;
                  factCorrY := 0.5-cos(factY*Pi)/2;

                  alphaUpLeft := cUpLeft.alpha/maxvalue;
                  alphaUpRight := cUpRight.alpha/maxvalue;
                  alphaLowLeft := cLowLeft.alpha/maxvalue;
                  alphaLowRight := cLowRight.alpha/maxvalue;

                  factUpLeft := (1-factCorrX)*(1-factCorrY);
                  factUpRight := factCorrX*(1-factCorrY);
                  factLowLeft := (1-factCorrX)*factCorrY;
                  factLowRight := factCorrX*factCorrY;

                  factUpLeftAlpha := factUpLeft*alphaUpLeft;
                  factUpRightAlpha := factUpRight*alphaUpRight;
                  factLowLeftAlpha := factLowLeft*alphaLowLeft;
                  factLowRightAlpha := factLowRight*alphaLowRight;

                  sumFactAlpha := factUpLeftAlpha+factUpRightAlpha+factLowLeftAlpha+factLowRightAlpha;
                  if sumFactAlpha=0 then
                  begin
                       cur.alpha := 0;
                       cur.red := 0;
                       cur.green := 0;
                       cur.blue := 0;
                  end else
                  begin
                    cur.red := round((factUpLeftAlpha*cUpLeft.red + factUpRightAlpha*cUpRight.red +
                                 factLowLeftAlpha*cLowLeft.red + factLowRightAlpha*cLowRight.red)/sumFactAlpha);
                    cur.green := round((factUpLeftAlpha*cUpLeft.green + factUpRightAlpha*cUpRight.green +
                                 factLowLeftAlpha*cLowLeft.green + factLowRightAlpha*cLowRight.green)/sumFactAlpha);
                    cur.blue := round((factUpLeftAlpha*cUpLeft.blue + factUpRightAlpha*cUpRight.blue +
                                factLowLeftAlpha*cLowLeft.blue + factLowRightAlpha*cLowRight.blue)/sumFactAlpha);
                    cur.alpha := round(sumFactAlpha*maxvalue);
                  end;
                  PDest^ := cur; inc(PDest);
                  factX := factX + factAddX;
             end;
             factY := factY+factAddY;
           end;
           cUpLeft := cUpRight;
           cLowLeft := cLowRight;
       end;
     end;
end;

function TMemBitmap.Resample(NewWidth, NewHeight: Integer): TMemBitmap;
var temp,newtemp: TMemBitmap;
begin
     if (NewWidth = Width) and (NewHeight = Height) then
       result := Duplicate else
     if (NewWidth >= Width) and (NewHeight >= Height) then
       result := ResampleLarger(NewWidth,NewHeight) else
     if (NewWidth <= Width) and (NewHeight <= Height) then
       result := ResampleSmaller(NewWidth,NewHeight) else
     begin
          temp := self;

          if NewWidth < Width then
          begin
            newtemp := temp.ResampleSmaller(NewWidth,temp.Height);
            if (temp<>self) then temp.free;
            temp := newtemp;
          end;

          if NewHeight < Height then
          begin
            newtemp := temp.ResampleSmaller(temp.Width,NewHeight);
            if (temp<>self) then temp.free;
            temp := newtemp;
          end;

          if NewWidth > Width then
          begin
            newtemp := temp.ResampleLarger(NewWidth,temp.Height);
            if (temp<>self) then temp.free;
            temp := newtemp;
          end;

          if NewHeight > Height then
          begin
            newtemp := temp.ResampleLarger(temp.Width,NewHeight);
            if (temp<>self) then temp.free;
            temp := newtemp;
          end;

          if temp<>self then result := temp else
            result := self.Duplicate;
     end;
end;

procedure TMemBitmap.VerticalFlip;
var yb: integer;
    line: PMemPixel;
    linesize: integer;
begin
     if Data= nil then exit;

     linesize := Width*sizeof(TMemPixel);
     line := nil;
     getmem(line, linesize);
     for yb := 0 to (Height div 2)-1 do
     begin
        move(Scanline[yb]^, line^, linesize);
        move(Scanline[Height-1-yb]^, Scanline[yb]^, linesize);
        move(line^, Scanline[Height-1-yb]^, linesize);
     end;
     freemem(line);
end;

procedure TMemBitmap.DrawPartial(Arect: TRect; Canvas: TCanvas; x,y: integer);
var partial: TMemBitmap;
    copywidth,copyheight,widthleft,heightleft,curxin,curyin,xdest,ydest,tx,ty: integer;
begin
     tx := ARect.Right-ARect.Left;
     ty := ARect.Bottom-ARect.Top;

     if ARect.Left >= Width then ARect.Left := ARect.Left mod Width else
       if ARect.Left < 0 then ARect.Left := Width - ((-ARect.Left) mod Width);
     ARect.Right := ARect.Left+tx;

     if ARect.Top >= Height then ARect.Top := ARect.Top mod Height else
       if ARect.Top < 0 then ARect.Top := Height - ((-ARect.Top) mod Height);
     ARect.Bottom := ARect.Top+ty;

     if (ARect.Left = 0) and (ARect.Top = 0) and (ARect.Right = Width) and (ARect.Bottom = Height) then
     begin
          Draw(Canvas,x,y);
          exit;
     end;

     partial := TMemBitmap.Create(tx,ty);
     heightleft := partial.height;
     curyin := ARect.Top;
     ydest := -ARect.Top;
     while heightleft > 0 do
     begin
       if curyin + heightleft > height then copyheight := height-curyin else
         copyheight := heightleft;

       widthleft := partial.width;
       curxin := ARect.Left;
       xdest := -ARect.Left;
       while widthleft > 0 do
       begin
         if curxin + widthleft > width then copywidth := width-curxin else
           copywidth := widthleft;

         partial.PutImage(xdest,ydest,self,dmSet);

         curxin := 0;
         dec(widthleft, copywidth);
         inc(xdest,copywidth);
       end;
       curyin := 0;
       dec(heightleft, copyheight);
       inc(ydest,copyheight);
     end;
     partial.Draw(Canvas,x,y);
     partial.Free;
end;

end.

