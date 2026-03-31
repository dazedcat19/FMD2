{ ImgInfos.pas

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
unit ImgInfos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, FPimage,
  FPReadJPEG, FPReadPNG, FPReadGif, FPReadBMP, FPReadTiff,
  FPWriteJPEG, FPWritePNG, FPWriteBMP, FPWriteTiff;

type
  PImageHandlerRec = ^TImageHandlerRec;
  TCheckImageStreamFunc = function(const Stream: TStream): Boolean;
  TGetImageStreamSizeProc = procedure(const Stream: TStream; out Width, Height: Integer);
  TImageHandlerRec = record
    ReaderClass: TFPCustomImageReaderClass;
    WriterClass: TFPCustomImageWriterClass;
    CheckImageStream: TCheckImageStreamFunc;
    GetImageStreamSize: TGetImageStreamSizeProc;
    Ext: String;
    WExt: String;
  end;


  { TimageHandlerMgr }

  TimageHandlerMgr = class
  private
    FEmptyHandlerRec: TImageHandlerRec;
    function GetCount: Integer;
  public
    List: array of TImageHandlerRec;
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ReaderClass: TFPCustomImageReaderClass;
      const WriterClass: TFPCustomImageWriterClass;
      const CheckImageStreamFunc: TCheckImageStreamFunc;
      const GetImageStreamSizeProc: TGetImageStreamSizeProc;
      const Ext: String;
      const WExt: String = '');
    function GetImageHandlerByStream(const Stream: TStream): PImageHandlerRec;
    function GetImageHandlerByFile(const FileName: String): PImageHandlerRec;
    function GetImageHandlerByExt(const Ext: String): PImageHandlerRec;
    function GetImageStreamExt(const Stream: TStream): String; inline;
    function GetImageFileExt(const FileName: String): String; inline;
    function GetImageStreamSize(const Stream: TStream; out Width, Height: Integer): String;
    function GetImageFileSize(const FileName: String; out Width, Height: Integer): String;
    function GetImageStreamReaderClass(const Stream: TStream): TFPCustomImageReaderClass; inline;
    function GetImageFileReaderClass(const FileName: String): TFPCustomImageReaderClass; inline;
    function GetImageExtReaderClass(const Ext: String): TFPCustomImageReaderClass; inline;
    function GetImageStreamWriterClass(const Stream: TStream): TFPCustomImageWriterClass; inline;
    function GetImageFileWriterClass(const FileName: String): TFPCustomImageWriterClass; inline;
    function GetImageExtWriterClass(const Ext: String): TFPCustomImageWriterClass; inline;
    function GetImageWriterExt(const Ext: String): String; inline;
  published
    property Count: Integer read GetCount;
  end;

function GetImageHandlerByStream(const Stream: TStream): TImageHandlerRec; inline;
function GetImageHandlerByFile(const FileName: String): TImageHandlerRec; inline;
function GetImageHandlerByExt(const Ext: String): TImageHandlerRec; inline;
function GetImageStreamExt(const Stream: TStream): String; inline;
function GetImageFileExt(const FileName: String): String; inline;
function GetImageStreamSize(const Stream: TStream; out Width, Height: Integer): String; inline;
function GetImageFileSize(const FileName: String; out Width, Height: Integer): String; inline;
function GetImageStreamReaderClass(const Stream: TStream): TFPCustomImageReaderClass; inline;
function GetImageFileReaderClass(const FileName: String): TFPCustomImageReaderClass; inline;
function GetImageExtReaderClass(const Ext: String): TFPCustomImageReaderClass; inline;
function GetImageStreamWriterClass(const Stream: TStream): TFPCustomImageWriterClass; inline;
function GetImageFileWriterClass(const FileName: String): TFPCustomImageWriterClass; inline;
function GetImageExtWriterClass(const Ext: String): TFPCustomImageWriterClass; inline;
function GetImageWriterExt(const Ext: String): String; inline;

var
  ImageHandlerMgr: TimageHandlerMgr;

implementation

function GetImageHandlerByStream(const Stream: TStream): TImageHandlerRec;
begin
  Result := ImageHandlerMgr.GetImageHandlerByStream(Stream)^;
end;

function GetImageHandlerByFile(const FileName: String): TImageHandlerRec;
begin
  Result := ImageHandlerMgr.GetImageHandlerByFile(FileName)^;
end;

function GetImageHandlerByExt(const Ext: String): TImageHandlerRec;
begin
  Result := ImageHandlerMgr.GetImageHandlerByExt(Ext)^;
end;

function GetImageStreamExt(const Stream: TStream): String;
begin
  Result := ImageHandlerMgr.GetImageStreamExt(Stream);
end;

function GetImageFileExt(const FileName: String): String;
begin
  Result := ImageHandlerMgr.GetImageFileExt(FileName);
end;

function GetImageStreamSize(const Stream: TStream; out Width, Height: Integer): String;
begin
  Result := ImageHandlerMgr.GetImageStreamSize(Stream, Width, Height);
end;

function GetImageFileSize(const FileName: String; out Width, Height: Integer): String;
begin
  Result := ImageHandlerMgr.GetImageFileSize(FileName, Width, Height);
end;

function GetImageStreamReaderClass(const Stream: TStream): TFPCustomImageReaderClass;
begin
  Result := ImageHandlerMgr.GetImageStreamReaderClass(Stream);
end;

function GetImageFileReaderClass(const FileName: String): TFPCustomImageReaderClass;
begin
  Result := ImageHandlerMgr.GetImageFileReaderClass(FileName);
end;

function GetImageExtReaderClass(const Ext: String): TFPCustomImageReaderClass;
begin
  Result := ImageHandlerMgr.GetImageExtReaderClass(Ext);
end;

function GetImageStreamWriterClass(const Stream: TStream): TFPCustomImageWriterClass;
begin
  Result := ImageHandlerMgr.GetImageStreamWriterClass(Stream);
end;

function GetImageFileWriterClass(const FileName: String): TFPCustomImageWriterClass;
begin
  Result := GetImageFileWriterClass(FileName);
end;

function GetImageExtWriterClass(const Ext: String): TFPCustomImageWriterClass;
begin
  Result := GetImageExtWriterClass(Ext);
end;

function GetImageWriterExt(const Ext: String): String;
begin
  Result := GetImageWriterExt(Ext);
end;

{ TimageHandlerMgr }

function TimageHandlerMgr.GetCount: Integer;
begin
  Result := Length(List);
end;

constructor TimageHandlerMgr.Create;
begin
  FillChar(FEmptyHandlerRec, SizeOf(FEmptyHandlerRec), 0);
end;

destructor TimageHandlerMgr.Destroy;
begin
  SetLength(List, 0);
  inherited Destroy;
end;

procedure TimageHandlerMgr.Add(const ReaderClass: TFPCustomImageReaderClass;
  const WriterClass: TFPCustomImageWriterClass;
  const CheckImageStreamFunc: TCheckImageStreamFunc;
  const GetImageStreamSizeProc: TGetImageStreamSizeProc; const Ext: String;
  const WExt: String);
var
  i: Integer;
begin
  i := Length(List);
  SetLength(List, i + 1);
  List[i].ReaderClass := ReaderClass;
  List[i].WriterClass := WriterClass;
  List[i].CheckImageStream := CheckImageStreamFunc;
  List[i].GetImageStreamSize := GetImageStreamSizeProc;
  List[i].Ext := Ext;
  if WExt <> '' then
    List[i].WExt := WExt
  else
    List[i].WExt := Ext;
end;

function TimageHandlerMgr.GetImageHandlerByStream(const Stream: TStream): PImageHandlerRec;
var
  P: Int64;
  i: Integer;
begin
  Result := @FEmptyHandlerRec;
  if Stream = nil then Exit;
  if Stream.Size = 0 then Exit;
  P := Stream.Position;
  try
    for i := Low(List) to High(List) do
    begin
      Stream.Position := 0;
      if List[i].CheckImageStream(Stream) then
      begin
        Result := @List[i];
        Break;
      end;
    end;
  finally
    Stream.Position := P;
  end;
end;

function TimageHandlerMgr.GetImageHandlerByFile(const FileName: String): PImageHandlerRec;
var
  FS: TFileStream;
begin
  Result := @FEmptyHandlerRec;
  if not FileExists(FileName) then Exit;
  try
    FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Result := GetImageHandlerByStream(FS);
  finally
    FS.Free;
  end;
end;

function TimageHandlerMgr.GetImageHandlerByExt(const Ext: String): PImageHandlerRec;
var
  i: Integer;
begin
  Result := @FEmptyHandlerRec;
  for i := Low(List) to High(List) do
    if Ext = List[i].Ext then
    begin
      Result := @List[i];
      Break;
    end;
end;

function TimageHandlerMgr.GetImageStreamExt(const Stream: TStream): String;
begin
  Result := GetImageHandlerByStream(Stream)^.Ext;
end;

function TimageHandlerMgr.GetImageFileExt(const FileName: String): String;
begin
  Result := GetImageHandlerByFile(FileName)^.Ext;
end;

function TimageHandlerMgr.GetImageStreamSize(const Stream: TStream; out Width,
  Height: Integer): String;
var
  H: PImageHandlerRec;
begin
  Width := 0;
  Height := 0;
  H := GetImageHandlerByStream(Stream);
  Result := H^.Ext;
  if Assigned(H^.GetImageStreamSize) then
    H^.GetImageStreamSize(Stream, Width, Height);
end;

function TimageHandlerMgr.GetImageFileSize(const FileName: String; out Width,
  Height: Integer): String;
var
  FS: TFileStream;
begin
  Result := '';
  Width := 0;
  Height := 0;
  if not FileExists(FileName) then Exit;
  try
    FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Result := GetImageStreamSize(FS, Width, Height);
  finally
    FS.Free;
  end;
end;

function TimageHandlerMgr.GetImageStreamReaderClass(const Stream: TStream
  ): TFPCustomImageReaderClass;
begin
  Result := GetImageHandlerByStream(Stream)^.ReaderClass;
end;

function TimageHandlerMgr.GetImageFileReaderClass(const FileName: String
  ): TFPCustomImageReaderClass;
begin
  Result := GetImageHandlerByFile(FileName)^.ReaderClass;
end;

function TimageHandlerMgr.GetImageExtReaderClass(const Ext: String
  ): TFPCustomImageReaderClass;
begin
  Result := GetImageHandlerByExt(Ext)^.ReaderClass;
end;

function TimageHandlerMgr.GetImageStreamWriterClass(const Stream: TStream
  ): TFPCustomImageWriterClass;
begin
  Result := GetImageHandlerByStream(Stream)^.WriterClass;
end;

function TimageHandlerMgr.GetImageFileWriterClass(const FileName: String
  ): TFPCustomImageWriterClass;
begin
  Result := GetImageHandlerByFile(FileName)^.WriterClass;
end;

function TimageHandlerMgr.GetImageExtWriterClass(const Ext: String
  ): TFPCustomImageWriterClass;
begin
  Result := GetImageHandlerByExt(Ext)^.WriterClass;
end;

function TimageHandlerMgr.GetImageWriterExt(const Ext: String): String;
begin
  Result := GetImageHandlerByExt(Ext)^.WExt;
end;

function JPEGCheckImageStream(const Stream: TStream): Boolean;
var
  Hdr: Word = 0;
begin
  Result := (Stream.Read(Hdr, 2) = 2) and (Hdr = $D8FF);
end;

procedure JPEGGetImageSize(const Stream: TStream; out Width, Height: Integer);
var
  B: Byte = 0;
  W: Word = 0;
begin
  if Stream.Seek(2, soFromBeginning) <> 2 then Exit;
  if Stream.Read(B, 1) <> 1 then Exit;
  while (Stream.Position < Stream.Size) and (B = $FF) do
  begin
    Stream.Read(B, 1);
    case B of
      $C0..$C3:
      begin
        Stream.Seek(3, soFromCurrent);
        Stream.Read(W, 2);
        Height := Swap(W);
        Stream.Read(W, 2);
        Width := Swap(W);
        Stream.Read(B, 1);
        Exit;
      end;
      $FF:
        Stream.Read(B, 1);
      $D0..$D9, $01:
      begin
        Stream.Seek(1, soFromCurrent);
        Stream.Read(B, 1);
      end;
      else
      begin
        Stream.Read(W, 2);
        Stream.Seek(Swap(W) - 2, soFromCurrent);
        Stream.Read(B, 1);
      end;
    end;
  end;
end;

function PNGCheckImageStream(const Stream: TStream): Boolean;
var
  Hdr: array[0..7] of Char = (' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ');
begin
  Result := (Stream.Read(Hdr, 8) = 8) and (Hdr = #137'PNG'#13#10#26#10);
end;

procedure PNGGetImageSize(const Stream: TStream; out Width, Height: Integer);
var
  W: Word = 0;
  H: Word = 0;
begin
  if not ((Stream.Seek(18, soFromBeginning) = 18)
    and (Stream.Read(W, 2) = 2)
    and (Stream.Seek(2, soFromCurrent) = 22)
    and (Stream.Read(H, 2) = 2)) then Exit;
  {$IFDEF ENDIAN_LITTLE}
  W := Swap(W);
  H := Swap(H);
  {$ENDIF}
  Width := W;
  Height := H;
end;

function GIFCheckImageStream(const Stream: TStream): Boolean;
var
  Hdr: array[0..5] of Char = (' ', ' ', ' ', ' ', ' ', ' ');
begin
  Result := (Stream.Read(Hdr, 6) = 6) and ((Hdr = 'GIF87a') or (Hdr = 'GIF89a'));
end;

procedure GIFGetImageSize(const Stream: TStream; out Width, Height: Integer);
type
  TGifHeader = packed record
    Sig: array[0..5] of Char;
    ScreenWidth,
    ScreenHeight: Word;
    PackedBit,
    BackgroundColor,
    AspectRatio: Byte;
  end;
  TGifImageDescriptor = packed record
    Left,
    Top,
    Width,
    Height: Word;
    PackedBit: Byte;
  end;
var
  Hdr: TGifHeader;
  Des: TGifImageDescriptor;
  PalleteSize: Integer = 0;
  C: Byte = 0;
begin
  FillChar(Hdr{%H-}, SizeOf(Hdr), 0);
  if Stream.Read(Hdr, SizeOf(TGifHeader)) <> SizeOf(TGifHeader) then Exit;
  if (Hdr.PackedBit and $80) <> 0 then
  begin
    PalleteSize := 3 * (1 shl (Hdr.Packedbit and 7 + 1));
    if Stream.Seek(PalleteSize, soFromCurrent) <> SizeOf(TGifHeader) + PalleteSize then Exit;
  end;
  FillChar(Des{%H-}, SizeOf(TGifImageDescriptor), 0);
  while Stream.Position < Stream.Size do
  begin
    Stream.Read(C, 1);
    if C = $2C then
    begin
      if Stream.Read(Des, SizeOf(TGifImageDescriptor)) <> SizeOf(TGifImageDescriptor) then Exit;
      {$IFDEF ENDIAN_BIG}
      Des.Width := LEtoN(Width);
      Des.Height := LEtoN(Height);
      {$ENDIF}
      Width := Des.Width;
      Height := Des.Height;
      Break;
    end;
  end;
end;

function BMPCheckImageStream(const Stream: TStream): Boolean;
var
  Hdr: Word = 0;
begin
  Result := (Stream.Read(Hdr, 2) = 2) and (Hdr = 19778);
end;

procedure BMPGetImageSize(const Stream: TStream; out Width, Height: Integer);
var
  W: LongInt = 0;
  H: LongInt = 0;
begin
  if not ((Stream.Seek(18, soFromBeginning) = 18)
    and (Stream.Read(W, 4) = 4)
    and (Stream.Read(H, 4) = 4)) then Exit;
  Width := LEtoN(W);
  Height := abs(LEtoN(H));
end;

function FixEndian(const W: Word; const BE: Boolean): Word; overload;
begin
  if BE then
    Result := BEtoN(W)
  else
    Result := LEtoN(W);
end;

function TIFFCheckImageStream(const Stream: TStream): Boolean;
var
  Hdr: array[0..1] of Char = (' ', ' ');
  Sig: Word = 0;
  BE: Boolean = False;

  function CheckHdr: Boolean;
  begin
    Result := True;
    if Hdr = 'II' then BE := False
    else if Hdr = 'MM' then BE := True
    else Result := False;
  end;

begin
  Result := (Stream.Read(Hdr, 2) = 2) and CheckHdr
    and (Stream.Read(Sig, 2) = 2) and (FixEndian(Sig, BE) = 42);
end;

function FixEndian(const W: DWord; const BE: Boolean): DWord; overload;
begin
  if BE then
    Result := BEtoN(W)
  else
    Result := LEtoN(W);
end;

procedure TIFFGetImageSize(const Stream: TStream; out Width, Height: Integer);
type
  TIDF_Field = packed record
    Tag,
    FieldType: Word;
    ValCount,
    ValOffset: DWord;
  end;
var
  Hdr: array[0..1] of Char = (' ', ' ');
  BE: Boolean = False;
  Imgs: Word = 0;
  Field: TIDF_Field;
  i: Word;

  function CheckHdr: Boolean;
  begin
    Result := True;
    if Hdr = 'II' then BE := False
    else if Hdr = 'MM' then BE := True
    else Result := False;
  end;

  function ReadDir: Boolean;
  begin
    Result := Stream.Read(Field, SizeOf(TIDF_Field)) = SizeOf(TIDF_Field);
    if not Result then Exit;
    Field.Tag := FixEndian(Field.Tag, BE);
    Field.ValOffset := FixEndian(Field.ValOffset, BE);
  end;

begin
  if not ((Stream.Read(Hdr, 2) = 2) and CheckHdr) then Exit;
  if Stream.Seek(6, soFromCurrent) <> 8 then Exit;
  if Stream.Read(Imgs, 2) <> 2 then Exit;
  Imgs := FixEndian(Imgs, BE);
  FillChar(Field{%H-}, SizeOf(TIDF_Field), 0);
  for i := 1 to Imgs do
  begin
    if not ReadDir then Exit;
    case Field.Tag of
      $0100: Width := Field.ValOffset;
      $0101: Height := Field.ValOffset;
    end;
  end;
end;

function WEBPCheckImageStream(const Stream: TStream): Boolean;
var
  Hdr: array[0..3] of Char = (#0, #0, #0, #0);
begin
  Result := (Stream.Read(Hdr, 4) = 4) and (Hdr = 'RIFF') and
    (Stream.Seek(4, soFromCurrent) = 8) and
    (Stream.Read(Hdr, 4) = 4) and (Hdr = 'WEBP');
end;

procedure WEBPGetImageSize(const Stream: TStream; out Width, Height: Integer);
var
  Hdr: array[0..3] of Byte = (0, 0, 0, 0);
begin
  Width := 0;
  Height := 0;
  if (Stream.Seek(12, soFromBeginning) <> 12) or
    (stream.Read(Hdr, 4) <> 4) then Exit;
  if not ((Hdr[0] = $56) and (Hdr[1] = $50) and (Hdr[2] = $38)) then Exit;
  // "VP8 "
  if Hdr[3] = $20 then
  begin
    // https://tools.ietf.org/html/rfc6386#page-30
    // 7 byte + 3 byte signature 9D 01 2A
    Stream.Seek(7, soFromCurrent);
    Stream.Read(Hdr, 3);
    if not ((Hdr[0] = $9D) and (Hdr[1] = $01) and (Hdr[2] = $2A)) then Exit;
    Stream.Read(Width, 2);
    Stream.Read(Height, 2);
    Width := LEtoN(Width);
    Height := LEtoN(Height);
  end
  else
  // "VP8L"
  if Hdr[3] = $4C then
  begin
    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification
    // 4 byte + 1 byte signature 2F
    Stream.Seek(4, soFromCurrent);
    Stream.Read(Hdr, 1);
    if Hdr[0] <> $2F then Exit;
    Stream.Read(Hdr, 4);
    Width := (((Hdr[1] and $3F) shl 8) or Hdr[0]) + 1;
    Height := (((Hdr[3] and $F) shl 10) or (Hdr[2] shl 2) or ((Hdr[1] and $C0) shr 6)) + 1;
  end
  else
  // "VP8X"
  if Hdr[3] = $58 then
  begin
    // https://developers.google.com/speed/webp/docs/riff_container#extended_file_format
    Stream.Seek(8, soFromCurrent);
    Stream.Read(Width, 3);
    Stream.Read(Height, 3);
    Width := LEtoN(Width) + 1;
    Height := LEtoN(Height) + 1;
  end;
end;

function AVIFCheckImageStream(const Stream: TStream): Boolean;
var
  Hdr: array[0..11] of Char = (#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0,#0);
begin
  // AVIF files start with an ISOBMFF ftyp box.
  // Bytes 4-7 must be 'ftyp', and bytes 8-11 typically 'avif' or 'avis'.
  Result := (Stream.Read(Hdr, 12) = 12) and
            (Hdr[4] = 'f') and (Hdr[5] = 't') and (Hdr[6] = 'y') and (Hdr[7] = 'p') and
            (Hdr[8] = 'a') and (Hdr[9] = 'v') and (Hdr[10] = 'i') and ((Hdr[11] = 'f') or (Hdr[11] = 's'));
end;

procedure AVIFGetImageSize(const Stream: TStream; out Width, Height: Integer);
var
  Buffer: array[0..4095] of Byte;
  BytesRead, i: Integer;
begin
  Width := 0;
  Height := 0;
  Stream.Seek(0, soFromBeginning);
  
  // Read the first 4KB to scan for the 'ispe' (Image Spatial Extent) box.
  // This avoids building a full ISOBMFF tree parser just to get dimensions.
  BytesRead := Stream.Read(Buffer{%H-}, SizeOf(Buffer));
  for i := 0 to BytesRead - 16 do
  begin
    if (Buffer[i] = Ord('i')) and (Buffer[i+1] = Ord('s')) and 
       (Buffer[i+2] = Ord('p')) and (Buffer[i+3] = Ord('e')) then
    begin
      // 'ispe' found. The next 4 bytes are version(1) and flags(3).
      // The 4 bytes after that are the Width, followed by 4 bytes for Height (Big Endian).
      Width := (Buffer[i+8] shl 24) or (Buffer[i+9] shl 16) or (Buffer[i+10] shl 8) or Buffer[i+11];
      Height := (Buffer[i+12] shl 24) or (Buffer[i+13] shl 16) or (Buffer[i+14] shl 8) or Buffer[i+15];
      Break;
    end;
  end;
end;

initialization
  ImageHandlerMgr := TimageHandlerMgr.Create;
  ImageHandlerMgr.Add(TFPReaderJPEG, TFPWriterJPEG, @JPEGCheckImageStream, @JPEGGetImageSize, 'jpg');
  ImageHandlerMgr.Add(TFPReaderPNG, TFPWriterPNG, @PNGCheckImageStream, @PNGGetImageSize, 'png');
  ImageHandlerMgr.Add(nil, nil, @WEBPCheckImageStream, @WEBPGetImageSize, 'webp');
  ImageHandlerMgr.Add(nil, nil, @AVIFCheckImageStream, @AVIFGetImageSize, 'avif');
  ImageHandlerMgr.Add(TFPReaderGif, TFPWriterPNG, @GIFCheckImageStream, @GIFGetImageSize, 'gif', 'png');
  ImageHandlerMgr.Add(TFPReaderBMP, TFPWriterBMP, @BMPCheckImageStream, @BMPGetImageSize, 'bmp');
  ImageHandlerMgr.Add(TFPReaderTiff, TFPWriterTiff, @TIFFCheckImageStream, @TIFFGetImageSize, 'tif');

finalization
  ImageHandlerMgr.Free;

end.
