{ Img2Pdf.pas

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
unit Img2Pdf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FPimage, ImgInfos, MemBitmap,
  FPReadJPEG, FPWriteJPEG, FPReadPNG, JPEGLib, JdAPImin, JDataSrc, Jerror,
  zstream, AnimatedGif, MultiLog, Process, ImageMagickManager;

type
  TCompressionQuality = 0..100;

  TImg2PDF = class;

  { TPageInfo }

  TPageInfo = class
  public
    constructor Create(const AOwner: TImg2PDF);
    destructor Destroy; override;
  public
    Owner: TImg2PDF;
    FileName: String;
    Ext: String;
    Width: Integer;
    Height: Integer;
    BitsPerComponent: Integer;
    ColorSpace: String;
    Filter: String;
    Stream: TMemoryStream;
  public
    procedure GetImageInfos;
    procedure LoadImageData;
    procedure FlushImageData;
  end;

  TPDFInfos = record
    Title,
    Subject,
    Author,
    Creator,
    Producer,
    Keywords: String;
    CreationDate: TDateTime;
    ModDate: TDateTime;
  end;

  { TImg2PDF }

  TImg2PDF = class
  private
    FPageInfos: TFPList;
    function GetPageInfo(const Index: Integer): TPageInfo;
  public
    constructor Create;
    destructor Destroy; override;
  public
    Infos: TPDFInfos;
    CompressionQuality: TCompressionQuality;
    function AddImage(const AFileName: String): Integer;
    procedure DeleteImage(const Index: Integer);
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToFile(const AFileName: String);
  public
    property PageInfo[const Index: Integer]: TPageInfo read GetPageInfo;
  end;

implementation

uses webp, FPWritePNG;

type

  { TFPReaderPNGInfo }

  TFPReaderPNGInfo = class(TFPReaderPNG)
  public
    property Header;
  end;

const
  CRLF = #13#10;
  PDF_VERSION = '%PDF-1.3';
  PDF_FILE_END = '%%EOF';
  PDF_MAX_GEN_NUM = 65535;

var
  JPEGError: jpeg_error_mgr;

{ TFPReaderPNGToPageInfo }

function PDFString(const S: String): String;
begin
  Result := S;
  if Pos(Result, '\') <> -1 then
    Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
  if Pos(Result, ')') <> -1 then
    Result := StringReplace(Result, ')', '\)', [rfReplaceAll]);
  if Pos(Result, '(') <> -1 then
    Result := StringReplace(Result, '(', '\(', [rfReplaceAll]);
end;

function PDFInt(const I: Integer; PadLength: Integer): String;
begin
  Result := IntToStr(I);
  PadLength := PadLength - Length(Result);
  if PadLength > 0 then
    Result := StringOfChar('0', PadLength) + Result;
end;

{ TPageInfo }

constructor TPageInfo.Create(const AOwner: TImg2PDF);
begin
  Owner := AOwner;
  FileName := '';
  Ext := '';
  Width := 0;
  Height := 0;
  BitsPerComponent := 8;
  ColorSpace := '';
  Filter := '';
end;

destructor TPageInfo.Destroy;
begin
  FlushImageData;
  inherited Destroy;
end;

procedure TPageInfo.GetImageInfos;
begin
  Ext := GetImageFileSize(FileName, Width, Height);
end;

procedure JPEGToPageInfo(const PageInfo: TPageInfo);
var
  AFS: TFileStream;
  JDS: jpeg_decompress_struct;
begin
  PageInfo.Filter := 'DCTDecode';
  try
    AFS := TFileStream.Create(PageInfo.FileName, fmOpenRead or fmShareDenyWrite);
    FillChar(JDS{%H-}, SizeOf(JDS), 0);
    JDS.err := @JPEGError;
    jpeg_CreateDecompress(@JDS, JPEG_LIB_VERSION, SizeOf(JDS));
    try
      jpeg_stdio_src(@JDS, @AFS);
      jpeg_read_header(@JDS, True);
      case JDS.jpeg_color_space of
        JCS_GRAYSCALE: PageInfo.ColorSpace := 'DeviceGray';
        JCS_RGB: PageInfo.ColorSpace := 'DeviceRGB';
        JCS_CMYK: PageInfo.ColorSpace := 'DeviceCMYK';
        else
          PageInfo.ColorSpace := 'DeviceRGB';
      end;
    finally
      jpeg_Destroy_Decompress(@JDS);
    end;
    PageInfo.Stream.LoadFromStream(AFS);
  finally
    AFS.Free;
  end;
end;

procedure JPEGCompressToPageInfo(const PageInfo: TPageInfo);
var
  IMG: TFPMemoryImage;
  RDR: TFPCustomImageReader;
  AFS: TFileStream;
  WRT: TFPWriterJPEG;
begin
  PageInfo.Filter := 'DCTDecode';
  IMG := TFPMemoryImage.Create(0, 0);
  try
    try
      RDR := GetImageExtReaderClass(PageInfo.Ext).Create;
      AFS := TFileStream.Create(PageInfo.FileName, fmOpenRead or fmShareDenyWrite);
      IMG.LoadFromStream(AFS, RDR);
      if (RDR is TFPReaderJPEG) and TFPReaderJPEG(RDR).GrayScale then
        PageInfo.ColorSpace := 'DeviceGray'
      else
        PageInfo.ColorSpace := 'DeviceRGB';
    finally
      RDR.Free;
      AFS.Free;
    end;
    WRT := TFPWriterJPEG.Create;
    try
      WRT.CompressionQuality := PageInfo.Owner.CompressionQuality;
      WRT.GrayScale := (PageInfo.ColorSpace = 'DeviceGray');
      IMG.SaveToStream(PageInfo.Stream, WRT);
    finally
      WRT.Free;
    end;
  finally
    IMG.Free;
  end;
end;

procedure PNGToPageInfo(const PageInfo: TPageInfo; const AStream: TStream = nil);
var
  AFS: TStream;
  IMG: TFPCustomImage;
  RDR: TFPReaderPNGInfo;
  AMS: TMemoryStream;
  X, Y: Integer;
  CLW, C: TFPColor;
  isGrayScale: Boolean;
begin
  IMG := TFPMemoryImage.Create(0, 0);
  try
    try
      if AStream = nil then
        AFS := TFileStream.Create(PageInfo.FileName, fmOpenRead or fmShareDenyWrite)
      else
        AFS := AStream;
      RDR := TFPReaderPNGInfo.Create;
      try
        RDR.CheckContents(AFS);
        if RDR.Header.ColorType = 3 then
          IMG.UsePalette := True;
        AFS.Position := 0;
        IMG.LoadFromStream(AFS, RDR);
      finally
        if AStream = nil then
          FreeAndNil(AFS);
      end;

      PageInfo.Filter := 'FlateDecode';
      PageInfo.BitsPerComponent := 8;

      AMS := TMemoryStream.Create;
      try
        case RDR.Header.ColorType of
          0, 4:
          begin
            PageInfo.ColorSpace := 'DeviceGray';
            for Y := 0 to IMG.Height - 1 do
              for X := 0 to IMG.Width - 1 do
                AMS.WriteByte(IMG.Colors[X, Y].red shr 8);
          end;
          3:
          begin
            PageInfo.ColorSpace := 'Indexed ';
            if Assigned(IMG.Palette) then
            begin
              isGrayScale := True;
              for X := 0 to IMG.Palette.Count - 1 do
              begin
                C := IMG.Palette.Color[X];
                if (C.red <> C.green) or (C.red <> C.blue) or (C.green <> C.blue) then
                begin
                  isGrayScale := False;
                  Break;
                end;
              end;
              if isGrayScale then
                PageInfo.ColorSpace := PageInfo.ColorSpace + '/DeviceGray'
              else
                PageInfo.ColorSpace := PageInfo.ColorSpace + '/DeviceRGB';
              PageInfo.ColorSpace := PageInfo.ColorSpace + ' ' + IntToStr(IMG.Palette.Count - 1) + ' <';
              if isGrayScale then
                for X := 0 to IMG.Palette.Count - 1 do
                  PageInfo.ColorSpace := PageInfo.ColorSpace + IntToHex(IMG.Palette.Color[X].red shr 8, 2)
              else
                for X := 0 to IMG.Palette.Count - 1 do
                begin
                  C := IMG.Palette.Color[X];
                  PageInfo.ColorSpace := PageInfo.ColorSpace +
                    IntToHex(C.red shr 8, 2) + IntToHex(C.green shr 8, 2) + IntToHex(C.blue shr 8, 2);
                end;
              PageInfo.ColorSpace := PageInfo.ColorSpace + '>';
            end;

            for Y := 0 to IMG.Height - 1 do
              for X := 0 to IMG.Width - 1 do
                AMS.WriteByte(Byte(IMG.Pixels[X, Y]));
          end;
          else
          begin
            PageInfo.ColorSpace := 'DeviceRGB';
            FillChar(CLW{%H-}, SizeOf(CLW), $FF);
            for Y := 0 to IMG.Height - 1 do
              for X := 0 to IMG.Width - 1 do
              begin
                C := IMG.Colors[X, Y];
                if C.alpha < $FFFF then
                  C := AlphaBlend(CLW, C);
                AMS.WriteByte(C.Red shr 8);
                AMS.WriteByte(C.Green shr 8);
                AMS.WriteByte(C.blue shr 8);
              end;
          end;
        end;
        with Tcompressionstream.Create(cldefault, PageInfo.Stream) do
          try
            AMS.Position := 0;
            Write(AMS.Memory^, AMS.Size);
            flush;
          finally
            Free;
          end;
      finally
        AMS.Free;
      end;
    finally
      RDR.Free;
    end
  finally
    IMG.Free;
  end;
end;

procedure WEBPToPageInfo(const PageInfo: TPageInfo);
var
  AMS: TMemoryStream;
  MBM: TMemBitmap;
  WRT: TFPWriterPNG;
begin
  AMS := TMemoryStream.Create;
  try
    AMS.LoadFromFile(PageInfo.FileName);
    MBM := nil;
    try
      MBM := WebPToMemBitmap(AMS);
      if Assigned(MBM) then
      try
        WRT := TFPWriterPNG.create;
        WRT.Indexed := False;
        WRT.UseAlpha := MBM.HasTransparentPixels;
        WRT.CompressionLevel := clnone;
        MBM.SaveToStream(AMS, WRT);
      finally
        WRT.Free;
      end;
    finally
      if Assigned(MBM) then
        MBM.Free;
    end;
    PNGToPageInfo(PageInfo, AMS);
  finally
    AMS.Free;
  end;
end;

{ --- Does not work yet. --- }

{ procedure GIFToPageInfo(const PageInfo: TPageInfo); }
{ var }
  { AMS: TMemoryStream; }
  { MBM: TAnimatedGif; }
  { WRT: TFPWriterPNG; }
{ begin }
  { AMS := TMemoryStream.Create; }
  { try }
    { MBM := TAnimatedGif.Create(PageInfo.FileName); }
    { MBM.CurrentImage := 0; }
    { try }
      { if Assigned(MBM) then }
      { try }
        { WRT := TFPWriterPNG.create; }
        { WRT.Indexed := False; }
        { WRT.UseAlpha := MBM.MemBitmap.HasTransparentPixels; }
        { WRT.CompressionLevel := clnone; }
        { MBM.MemBitmap.SaveToStream(AMS, WRT); }
      { finally }
        { WRT.Free; }
      { end; }
    { finally }
      { MBM.Free; }
    { end; }
    { PNGToPageInfo(PageInfo, AMS); }
  { finally }
    { AMS.Free; }
  { end; }
{ end; }

procedure ImageToPageInfo(const PageInfo: TPageInfo);
var
  IMG: TFPCustomImage;
  RDR: TFPCustomImageReader;
  AFS: TFileStream;
  AMS: TMemoryStream;
  CLW, C: TFPColor;
  X, Y: Integer;
begin
  PageInfo.Filter := 'FlateDecode';
  PageInfo.ColorSpace := 'DeviceRGB';
  IMG := TFPMemoryImage.Create(0, 0);
  try
    try
      RDR := GetImageExtReaderClass(PageInfo.Ext).Create;
      AFS := TFileStream.Create(PageInfo.FileName, fmOpenRead or fmShareDenyWrite);
      IMG.LoadFromStream(AFS, RDR);
    finally
      RDR.Free;
      AFS.Free;
    end;
    AMS := TMemoryStream.Create;
    try
      FillChar(CLW{%H-}, SizeOf(CLW), $FF);
      for Y := 0 to IMG.Height - 1 do
        for X := 0 to IMG.Width - 1 do
        begin
          C := IMG.Colors[X, Y];
          if C.alpha < $FFFF then
            C := AlphaBlend(CLW, C);
          AMS.WriteByte(C.Red shr 8);
          AMS.WriteByte(C.Green shr 8);
          AMS.WriteByte(C.blue shr 8);
        end;
      with Tcompressionstream.Create(cldefault, PageInfo.Stream) do
        try
          AMS.Position := 0;
          Write(AMS.Memory^, AMS.Size);
          flush;
        finally
          Free;
        end;
    finally
      AMS.Free;
    end;
  finally
    IMG.Free;
  end;
end;

procedure TPageInfo.LoadImageData;
var
  tmpFile, tmpDir: String;
  proc: TProcess;
begin
  if Assigned(Stream) then Exit;
  if Ext = '' then Exit;
  Stream := TMemoryStream.Create;
  try
    if (Ext = 'jpg') and (Owner.CompressionQuality >= 75) then
      JPEGToPageInfo(Self)
    else
    if Owner.CompressionQuality < 100 then
      JPEGCompressToPageInfo(Self)
    else
    if Ext = 'png' then
      PNGToPageInfo(Self)
    else
    if Ext = 'webp' then
      WEBPToPageInfo(Self)
    { else }
    { if Ext = 'gif' then }
      { GIFToPageInfo(Self) }
    else
      ImageToPageInfo(Self);
  except
    on E: Exception do
    begin
      FreeAndNil(Stream);
      if TImageMagickManager.Instance.Enabled then
      begin
        tmpDir := ExtractFilePath(FileName);
        tmpFile := tmpDir + 'pdf_tmp_' + ChangeFileExt(ExtractFileName(FileName), '.jpg');
        proc := TProcess.Create(nil);
        try
          if TImageMagickManager.Instance.PathFound then
            proc.Executable := TImageMagickManager.Instance.MagickPath + 'magick'
          else
            proc.Executable := 'magick';
          proc.Parameters.Add(FileName);
          proc.Parameters.Add('-quality');
          proc.Parameters.Add(IntToStr(Owner.CompressionQuality));
          proc.Parameters.Add(tmpFile);
          proc.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
          proc.ShowWindow := swoHIDE;
          proc.Execute;
          proc.WaitOnExit;
          if FileExists(tmpFile) then
          begin
            Ext := 'jpg';
            Width := 0;
            Height := 0;
            GetImageInfos;
            Stream := TMemoryStream.Create;
            JPEGToPageInfo(Self);
            DeleteFile(tmpFile);
          end;
        finally
          proc.Free;
        end;
      end;
    end;
  end;
end;

procedure TPageInfo.FlushImageData;
begin
  if Assigned(Stream) then
    FreeAndNil(Stream);
end;

{ TImg2PDF }

function TImg2PDF.GetPageInfo(const Index: Integer): TPageInfo;
begin
  Result := TPageInfo(FPageInfos[Index]);
end;

constructor TImg2PDF.Create;
begin
  FPageInfos := TFPList.Create;
  Infos.Title := '';
  Infos.Subject := '';
  Infos.Author := '';
  Infos.Creator := '';
  Infos.Producer := 'Img2Pdf';
  Infos.Keywords := '';
  Infos.CreationDate := Now;
  Infos.ModDate := Now;
  CompressionQuality := 100;
end;

destructor TImg2PDF.Destroy;
begin
  while FPageInfos.Count <> 0 do
    DeleteImage(FPageInfos.Count - 1);
  FPageInfos.Free;
  inherited Destroy;
end;

function TImg2PDF.AddImage(const AFileName: String): Integer;
var
  P: TPageInfo;
begin
  Result := -1;
  if not FileExists(AFileName) then Exit;
  P := TPageInfo.Create(Self);
  P.FileName := AFileName;
  P.GetImageInfos;
  if P.Ext <> '' then
    Result := FPageInfos.Add(P)
  else
    p.Free;
end;

procedure TImg2PDF.DeleteImage(const Index: Integer);
begin
  TPageInfo(FPageInfos[Index]).Free;
  FPageInfos.Delete(Index);
end;

procedure TImg2PDF.SaveToStream(const Stream: TStream);
var
  PDFBuffer: String;
  AData, vkids: String;
  AObjs: array of Integer;
  AObjCount, i, vnbpal, vni, vo: Integer;

  procedure PDFFlush;
  begin
    if PDFBuffer = '' then Exit;
    Stream.Write(Pointer(PDFBuffer)^, Length(PDFBuffer));
    PDFBuffer := '';
  end;

  procedure PDFWrite(const S: String);
  begin
    PDFBuffer := PDFBuffer + S + CRLF;
  end;

  procedure CreateNewObj;
  begin
    Inc(AObjCount);
    SetLength(AObjs, Length(AObjs) + 1);
    AObjs[AObjCount] := Stream.Size;
    PDFWrite(IntToStr(AObjCount) + ' 0 obj');
  end;

begin
  if FPageInfos.Count = 0 then Exit;

  PDFBuffer := '';
  AObjCount := 2;
  SetLength(AObjs, 3);

  try
    PDFWrite(PDF_VERSION);

    for i := 0 to FPageInfos.Count - 1 do
    begin
      CreateNewObj;
      PDFWrite('<<');
      PDFWrite('/Type /Page');
      PDFWrite('/Parent 1 0 R');
      PDFWrite('/MediaBox [0 0 ' + IntToStr(PageInfo[i].Width) + ' ' + IntToStr(PageInfo[i].Height) + ']');
      PDFWrite('/Resources 2 0 R');
      PDFWrite('/Contents ' + IntToStr(AObjCount + 1) + ' 0 R');
      PDFWrite('>>');
      PDFWrite('endobj');

      CreateNewObj;
      AData := '1 0 0 1 0 ' + IntToStr(PageInfo[i].Height) + ' cm' + CRLF +
        Format('q %d 0 0 %d 0 -%d cm /I%d Do Q', [PageInfo[i].Width, PageInfo[i].Height,
        PageInfo[i].Height, i + 1]);
      PDFWrite('<<');
      PDFWrite('/Length ' + IntToStr(Length(AData)));
      PDFWrite('>>');
      PDFWrite('stream');
      PDFWrite(AData + CRLF + 'endstream');
      PDFWrite('endobj');
    end;
    PDFFlush;

    vni := AObjCount;
    for i := 0 to FPageInfos.Count - 1 do
      try
        PageInfo[i].LoadImageData;
        CreateNewObj;
        PDFWrite('<<');
        PDFWrite('/Type /XObject');
        PDFWrite('/Subtype /Image');
        PDFWrite('/Width ' + IntToStr(PageInfo[i].Width));
        PDFWrite('/Height ' + IntToStr(PageInfo[i].Height));
        PDFWrite('/ColorSpace [/' + PageInfo[i].ColorSpace + ']');
        PDFWrite('/BitsPerComponent ' + IntToStr(PageInfo[i].BitsPerComponent));
        PDFWrite('/Filter /' + PageInfo[i].Filter);
        PDFWrite('/Length ' + IntToStr(PageInfo[i].Stream.Size));
        PDFWrite('>>');
        PDFWrite('stream');
        PDFFlush;
        if Assigned(PageInfo[i].Stream) then
        begin
          PageInfo[i].Stream.Position := 0;
          Stream.CopyFrom(PageInfo[i].Stream, PageInfo[i].Stream.Size);
        end;
        PDFWrite(CRLF + 'endstream');
        PDFWrite('endobj');
      finally
        PageInfo[i].FlushImageData;
      end;
    PDFFlush;

    AObjs[1] := Stream.Size;
    PDFWrite('1 0 obj');
    PDFWrite('<<');
    PDFWrite('/Type /Pages');
    vkids := '/Kids [';
    for i := 0 to FPageInfos.Count - 1 do
      vkids := vkids + IntToStr(3 + 2 * i) + ' 0 R ';
    PDFWrite(vkids + ']');
    PDFWrite('/Count ' + IntToStr(FPageInfos.Count));
    PDFWrite('/MediaBox [0 0 800 600]');
    PDFWrite('>>');
    PDFWrite('endobj');

    AObjs[2] := Stream.Size;
    PDFWrite('2 0 obj');
    PDFWrite('<<');
    PDFWrite('/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]');
    PDFWrite('/Font');
    PDFWrite('<<');
    PDFWrite('>>');
    PDFWrite('/XObject');
    PDFWrite('<<');
    vnbpal := 0;
    for i := 1 to FPageInfos.Count do
    begin
      PDFWrite('/I' + IntToStr(i) + ' ' + IntToStr(vni + (i) + vnbpal) + ' 0 R');
      if (PageInfo[i - 1].ColorSpace = 'Indexed') then
        vnbpal := vnbpal + 1;
    end;
    PDFWrite('>>');
    PDFWrite('>>');
    PDFWrite('endobj');

    CreateNewObj;
    PDFWrite('<<');
    PDFWrite('/CreationDate (D:' + FormatDateTime('yyyymmddhhnnss', Infos.CreationDate) + ')');
    PDFWrite('/ModDate (D:' + FormatDateTime('yyyymmddhhnnss', Infos.ModDate) + ')');
    if Infos.Title <> '' then
      PDFWrite('/Title (' + PDFString(Infos.Title) + ')');
    if Infos.Subject <> '' then
      PDFWrite('/Subject (' + PDFString(Infos.Subject) + ')');
    if Infos.Author <> '' then
      PDFWrite('/Author (' + PDFString(Infos.Author) + ')');
    if Infos.Creator <> '' then
      PDFWrite('/Creator (' + PDFString(Infos.Creator) + ')');
    if Infos.Producer <> '' then
      PDFWrite('/Producer (' + PDFString(Infos.Producer) + ')');
    if Infos.Keywords <> '' then
      PDFWrite('/Keywords (' + PDFString(Infos.Keywords) + ')');
    PDFWrite('>>');
    PDFWrite('endobj');

    CreateNewObj;
    PDFWrite('<<');
    PDFWrite('/Type /Catalog');
    PDFWrite('/OpenAction [3 0 R /FitH null]');
    PDFWrite('/Pages 1 0 R');
    PDFWrite('>>');
    PDFWrite('endobj');

    vo := Stream.Size;
    PDFWrite('xref');
    PDFWrite('0 ' + IntToStr(AObjCount + 1));
    PDFWrite('0000000000 ' + PDFInt(PDF_MAX_GEN_NUM, 5) + ' f');
    for i := 1 to AObjCount do
      PDFWrite(PDFInt(AObjs[i], 10) + ' 00000 n');
    PDFWrite('trailer');
    PDFWrite('<<');
    PDFWrite('/Size ' + IntToStr(AObjCount + 1));
    PDFWrite('/Root ' + IntToStr(AObjCount) + ' 0 R');
    PDFWrite('/Info ' + IntToStr(AObjCount - 1) + ' 0 R');
    PDFWrite('>>');
    PDFWrite('startxref');
    PDFWrite(IntToStr(vo));

    PDFWrite(PDF_FILE_END);
    PDFFlush;
  finally
    SetLength(AObjs, 0);
  end;
end;

procedure TImg2PDF.SaveToFile(const AFileName: String);
var
  fs: TFileStream;
begin
  if FPageInfos.Count = 0 then Exit;
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

initialization
  FillChar(JPEGError, SizeOf(JPEGError), 0);
  jpeg_std_error(JPEGError);

end.
