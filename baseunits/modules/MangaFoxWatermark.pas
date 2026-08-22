{ MangaFoxWatermark.pas

  Copyright (C) 2016

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit MangaFoxWatermark;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FileUtil, LazFileUtils, Math, FPimage, FPReadJPEG,
  FPWriteJPEG, FPWritePNG, ImgInfos;

procedure SetTemplateDirectory(const ADirectory: String);
function LoadTemplate(const ADirectory: String = ''): Integer;
procedure ClearTemplate;
function RemoveWatermark(const AFileName: String; const ASaveAsPNG: Boolean = False): Boolean; inline;

implementation

type
  POneBitImage = ^TOneBitImage;
  TOneBitImage = record
    Width: Integer;
    Heigth: Integer;
    Bits: Pointer;
    BitsLength: Integer;
  end;

  { TWatermarkRemover }

  TWatermarkRemover = class
  private
    FCS_Templates: TRTLCriticalSection;
    FCS_RemoveWatermark: TRTLCriticalSection;
    FTemplates: array of TOneBitImage;
    FTemplateDirectory: String;
    FCleared: Boolean;
    procedure AddTemplate(const Image: TOneBitImage);
    function GetTemplate(const Index: Integer): POneBitImage;
    function GetTemplateCount: Integer;
  protected
    function AddFileToTemplates(const FileName: String): Boolean;
  public
    MinPSNR: Single;
    MinWhiteBorder: Integer;
    constructor Create;
    destructor Destroy; override;
    function LoadTemplate(const ADirectory: String = ''): Integer;
    procedure ClearTemplate;
    function RemoveWatermark(const AFileName: String; const SaveAsPNG: Boolean = False): Boolean;
    property TemplateDirectory: String read FTemplateDirectory write FTemplateDirectory;
    property TemplateCount: Integer read GetTemplateCount;
    property Template[const Index: Integer]: POneBitImage read GetTemplate;
  end;

var
  WatermarkRemover: TWatermarkRemover;

procedure SetTemplateDirectory(const ADirectory: String);
begin
  if WatermarkRemover = nil then
    WatermarkRemover := TWatermarkRemover.Create;
  WatermarkRemover.TemplateDirectory := ADirectory;
end;

function LoadTemplate(const ADirectory: String): Integer;
begin
  if WatermarkRemover = nil then
    WatermarkRemover := TWatermarkRemover.Create;
  Result := WatermarkRemover.LoadTemplate(ADirectory);
end;

procedure ClearTemplate;
begin
  if Assigned(WatermarkRemover) then
  begin
    WatermarkRemover.ClearTemplate;
    FreeAndNil(WatermarkRemover);
  end;
end;

function RemoveWatermark(const AFileName: String; const ASaveAsPNG: Boolean): Boolean;
begin
  Result := False;
  if Assigned(WatermarkRemover) then
    Result := WatermarkRemover.RemoveWatermark(AFileName, ASaveAsPNG);
end;

{ TWatermarkTemplates }

procedure TWatermarkRemover.AddTemplate(const Image: TOneBitImage);
var
  i: Integer;
begin
  i := Length(FTemplates);
  SetLength(FTemplates, i + 1);
  FTemplates[i] := Image;
end;

function TWatermarkRemover.GetTemplate(const Index: Integer): POneBitImage;
begin
  Result := @FTemplates[Index];
end;

function TWatermarkRemover.GetTemplateCount: Integer;
begin
  Result := Length(FTemplates);
end;

procedure OtsuThresholding(const Image: TOneBitImage);
var
  histogram: array[0..255] of Integer;
  threshold: Byte;
  meanTotal, variance, maxVariance, zerothCumuMoment, firstCumuMoment: Single;
  i: Integer;
  bit: PByte;
begin
  FillChar(histogram, SizeOf(histogram), 0);
  bit := Image.Bits;
  for i := 0 to Image.BitsLength - 1 do
  begin
    Inc(histogram[bit^]);
    Inc(bit);
  end;
  threshold := 0;
  meanTotal := 0;
  maxVariance := 0;
  firstCumuMoment := 0;
  zerothCumuMoment := 0;
  for i := 0 to 255 do
    meanTotal := meanTotal + (i * histogram[i] / Image.BitsLength);
  for i := 0 to 255 do
  begin
    zerothCumuMoment := zerothCumuMoment + histogram[i] / Image.BitsLength;
    firstCumuMoment := firstCumuMoment + (i * histogram[i] / Image.BitsLength);
    variance := meanTotal * zerothCumuMoment - firstCumuMoment;
    variance := variance * variance;
    if ((zerothCumuMoment <> 0) and (zerothCumuMoment <> 1)) then
    begin
      variance := variance / (zerothCumuMoment * (1 - zerothCumuMoment));
      if (maxVariance < variance) then
      begin
        maxVariance := variance;
        threshold := i;
      end;
    end;
  end;

  bit := Image.Bits;
  for i := 0 to Image.BitsLength - 1 do
  begin
    if bit^ > threshold then
      bit^ := 255
    else
      bit^ := 0;
    Inc(bit);
  end;
end;

procedure BuildImageToOneBit(const Image: TFPCustomImage;
  var OutImage: TOneBitImage;
  const Left: Integer = -1;
  const Top: Integer = -1;
  const Right: Integer = -1;
  const Bottom: Integer = -1);
var
  X, Y, L, R, T, B: Integer;
  Bit: PByte;
  ML, MT: Integer;
begin
  L := Left;
  T := Top;
  R := Right;
  B := Bottom;
  if (L < 0) or (L > Image.Width) then L := 0;
  if (T < 0) or (T > Image.Height) then T := 0;
  if (R < 0) or (R <= L) then R := Image.Width;
  if (B < 0) or (B <= T) then B := Image.Height;
  OutImage.Width := R - L;
  OutImage.Heigth := B - T;
  OutImage.BitsLength := OutImage.Width * OutImage.Heigth;
  Getmem(OutImage.Bits, OutImage.BitsLength);
  Bit := OutImage.Bits;
  if (R <= Image.Width) and (B <= Image.Height) then
  begin
    for Y := T to B - 1 do
      for X := L to R - 1 do
      begin
        Bit^ := CalculateGray(Image.Colors[X, Y]) shr 8;
        Inc(Bit);
      end;
  end
  else
  begin
    FillChar(OutImage.Bits^, OutImage.BitsLength, $ff);
    ML := (R - Image.Width) div 2;
    MT := ((B - Image.Height) div 2) * OutImage.Width;
    if MT > 0 then
      Inc(Bit, MT);
    if ML > 0 then
    begin
      Inc(Bit, ML);
      ML := ML + (R - Image.Width - ML);
    end;
    for Y := T to B - 1 do
    begin
      for X := L to Image.Width - 1 do
      begin
        Bit^ := CalculateGray(Image.Colors[X, Y]) shr 8;
        Inc(Bit);
      end;
      if ML > 0 then
        Inc(Bit, ML);
    end;
  end;
  OtsuThresholding(OutImage);
end;

function TWatermarkRemover.AddFileToTemplates(const FileName: String): Boolean;
var
  FS: TFileStream;
  IMG: TFPCustomImage;
  H: TFPCustomImageReaderClass;
  R: TFPCustomImageReader;
  BIMG: TOneBitImage;
begin
  Result := False;
  H := GetImageFileReaderClass(FileName);
  if H = nil then Exit;
  IMG := TFPMemoryImage.Create(0, 0);
  try
    try
      R := H.Create;
      FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      IMG.LoadFromStream(FS, R);
    finally
      R.Free;
      FS.Free;
    end;
    FillChar(BIMG, SizeOf(BIMG), 0);
    BuildImageToOneBit(IMG, BIMG);
    AddTemplate(BIMG);
    Result := True;
  finally
    IMG.Free;
  end;
end;

constructor TWatermarkRemover.Create;
begin
  InitCriticalSection(FCS_Templates);
  InitCriticalSection(FCS_RemoveWatermark);
  FCleared := True;
  MinPSNR := 9.0;
  MinWhiteBorder := 4;
end;

destructor TWatermarkRemover.Destroy;
begin
  ClearTemplate;
  DoneCriticalsection(FCS_Templates);
  DoneCriticalsection(FCS_RemoveWatermark);
end;

function TWatermarkRemover.LoadTemplate(const ADirectory: String): Integer;
var
  Files: TStrings;
  D: String;
  I: Integer;
begin
  Result := 0;
  FCleared := False;
  if (ADirectory <> '') and (ADirectory <> FTemplateDirectory) then
    FTemplateDirectory := ADirectory;
  if FTemplateDirectory = '' then Exit;
  D := CleanAndExpandDirectory(FTemplateDirectory);
  if not DirectoryExists(D) then Exit;
  if TryEnterCriticalsection(FCS_Templates) <> 0 then
    try
      Files := TStringList.Create;
      ClearTemplate;
      FCleared := False;
      FindAllFiles(Files, CleanAndExpandDirectory(D), '*.*', False);
      if Files.Count = 0 then Exit;
      for I := 0 to Files.Count - 1 do
        if AddFileToTemplates(Files[I]) then
          Inc(Result);
    finally
      Files.Free;
      LeaveCriticalsection(FCS_Templates);
    end;
end;

procedure TWatermarkRemover.ClearTemplate;
var
  i: Integer;
begin
  for i := Low(FTemplates) to High(FTemplates) do
    Freemem(FTemplates[i].Bits);
  SetLength(FTemplates, 0);
  FCleared := True;
end;

function CalculatePSNR(const Image1, Image2: TOneBitImage): Single;
var
  MSE: Single;
  i: Integer;
  bit1, bit2: PByte;
begin
  Result := 0.0;
  if Image1.BitsLength <> Image2.BitsLength then
    Exit;
  MSE := 0.0;
  bit1 := Image1.Bits;
  bit2 := Image2.Bits;
  for i := 0 to Image1.BitsLength - 1 do
  begin
    if bit1^ <> bit2^ then
      MSE := MSE + sqr(abs(bit2^ - bit1^));
    Inc(bit1);
    Inc(bit2);
  end;
  MSE := MSE / Image1.BitsLength;
  if Sqrt(MSE) < 0.0001 then
    Result := 1e06
  else
    Result := 10 * log10(sqr(255) / MSE);
end;

function TWatermarkRemover.RemoveWatermark(const AFileName: String;
  const SaveAsPNG: Boolean): Boolean;
var
  Handler: TImageHandlerRec;
  Reader: TFPCustomImageReader;
  Writer: TFPCustomImageWriter;
  Image, ImageTemp: TFPCustomImage;
  FileStream: TFileStream;
  BestIndex, I, L, T, R, B: Integer;
  BestValue, PSNR: Single;
  TIMG: TOneBitImage;
  NewFileName: String;
  GrayScale: Boolean;
  InvalidBorder: Boolean;
  Bit: PByte;
begin
  Result := False;
  EnterCriticalsection(FCS_RemoveWatermark);
  try
    if TemplateCount = 0 then
    begin
      if not FCleared then Exit;
      LoadTemplate;
    end;
    if TemplateCount = 0 then Exit;
    Handler := GetImageHandlerByFile(AFileName);
    if Handler.Ext = '' then Exit;
    Image := TFPMemoryImage.Create(0, 0);
    try
      GrayScale := False;
      try
        Reader := Handler.ReaderClass.Create;
        FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
        Image.LoadFromStream(FileStream, Reader);
        if Reader is TFPReaderJPEG then
          GrayScale := TFPReaderJPEG(Reader).GrayScale;
      finally
        Reader.Free;
        FileStream.Free;
      end;
      BestIndex := -1;
      BestValue := 0.0;
      FillChar(TIMG, SizeOf(TIMG), 0);
      for I := 0 to TemplateCount - 1 do
        if Image.Height >= FTemplates[i].Heigth then
        begin
          if Image.Width > FTemplates[i].Width then
            L := (Image.Width - FTemplates[i].Width) div 2
          else
            L := 0;
          R := L + FTemplates[i].Width;
          T := Image.Height - FTemplates[i].Heigth;
          B := Image.Height;
          BuildImageToOneBit(Image, TIMG, L, T, R, B);
          try
            InvalidBorder := False;
            if MinWhiteBorder > 0 then
            begin
              Bit := TIMG.Bits;
              R := MinWhiteBorder * TIMG.Width;
              for L := 0 to R - 1 do
              begin
                if Bit^ = 0 then
                begin
                  InvalidBorder := True;
                  Break;
                end;
                Inc(Bit);
              end;
            end;
            if not InvalidBorder then
            begin
              PSNR := CalculatePSNR(TIMG, FTemplates[I]);
              if PSNR > BestValue then
              begin
                BestValue := PSNR;
                BestIndex := I;
              end;
              Bit := TIMG.Bits;
            end;
          finally
            Freemem(TIMG.Bits);
          end;
        end;
      if (BestValue >= MinPSNR) and (BestIndex <> -1) then
        try
          ImageTemp := TFPMemoryImage.Create(Image.Width, Image.Height - FTemplates[BestIndex].Heigth);
          for T := 0 to ImageTemp.Height - 1 do
            for L := 0 to ImageTemp.Width - 1 do
              ImageTemp.Colors[L, T] := Image.Colors[L, T];
          if SaveAsPNG then
          begin
            Handler.WriterClass := TFPWriterPNG;
            Handler.WExt := 'png';
          end;
          NewFileName := ExtractFileNameWithoutExt(AFileName) + '.' + Handler.WExt;
          if FileExists(AFileName) then
            DeleteFile(AFileName);
          if FileExists(NewFileName) then
            DeleteFile(NewFileName);
          if not FileExists(NewFileName) then
          begin
            try
              Writer := Handler.WriterClass.Create;
              FileStream := TFileStream.Create(NewFileName, fmCreate);
              if Writer is TFPWriterJPEG then
                TFPWriterJPEG(Writer).GrayScale := GrayScale
              else
              if Writer is TFPWriterPNG then
              begin
                if GrayScale then
                  TFPWriterPNG(Writer).Indexed := True;
              end;
              ImageTemp.SaveToStream(FileStream, Writer);
            finally
              Writer.Free;
              FileStream.Free;
            end;
            Result := FileExists(NewFileName);
          end;
        finally
          ImageTemp.Free;
        end;
    finally
      Image.Free;
    end;
  finally
    LeaveCriticalsection(FCS_RemoveWatermark);
  end;
end;

initialization

finalization
  if Assigned(WatermarkRemover) then WatermarkRemover.Free;

end.
