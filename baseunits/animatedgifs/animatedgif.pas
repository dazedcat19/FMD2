unit AnimatedGif;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, FPImage, MemBitmap;

type
    TDisposeMode = (dmNone, dmKeep, dmErase, dmRestore);
    TGifSubImage = record
        Image: TMemBitmap;
        Position: TPoint;
        Delay: integer;
        disposeMode: TDisposeMode;
        TransparentColor: TMemPixel;
    end;
    TGifSubImageArray = array of TGifSubImage;

    TGifBackgroundMode = (gbmSimplePaint, gbmEraseBackground, gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously);

    { TAnimatedGif }

    TAnimatedGif = class(TGraphic)
      private
        FWidth,FHeight: integer;
        FBackgroundColor: TColor;

        FPrevDate: TDateTime;
        FPaused: boolean;
        FTimeAccumulator: double;
        FCurrentImage, FWantedImage: integer;
        FPreviousDisposeMode : TDisposeMode;

        FBackgroundImage, FPreviousVirtualScreen,
        FStretchedVirtualScreen, FInternalVirtualScreen, FRestoreImage: TMemBitmap;
        FImageChanged: boolean;

        function GetCount: integer;
        procedure Render(StretchWidth,StretchHeight: integer);
        procedure UpdateSimple(Canvas: TCanvas; ARect: TRect; DrawOnlyIfChanged: Boolean = true);
        procedure UpdateEraseBackground(Canvas: TCanvas; ARect: TRect; DrawOnlyIfChanged: Boolean = true);
        procedure Init;
        function GetBitmap: TBitmap;
        function GetMemBitmap: TMemBitmap;
        procedure SaveBackgroundOnce(Canvas: TCanvas; ARect: TRect);
        procedure SetCurrentImage(Index: integer);

      protected
        FImages: TGifSubImageArray;
        procedure LoadImages(stream: TStream);

        {TGraphic}
        procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
        function GetEmpty: Boolean; override;
        function GetHeight: Integer; override;
        function GetTransparent: Boolean; override;
        function GetWidth: Integer; override;
        procedure SetHeight(Value: Integer); override;
        procedure SetTransparent(Value: Boolean); override;
        procedure SetWidth(Value: Integer); override;

      public
        EraseColor: TColor;
        BackgroundMode: TGifBackgroundMode;

        constructor Create(filename: string);
        constructor Create(stream: TStream);
        constructor Create; override;
        function Duplicate: TAnimatedGif;

        {TGraphic}
        procedure LoadFromStream(Stream: TStream); override;
        procedure SaveToStream(Stream: TStream); override;
        class function GetFileExtensions: string; override;

        procedure Clear; override;
        destructor Destroy; override;
        procedure Pause;
        procedure Resume;

        procedure Show(Canvas: TCanvas; ARect: TRect); overload;
        procedure Update(Canvas: TCanvas; ARect: TRect); overload;
        procedure Hide(Canvas: TCanvas; ARect: TRect); overload;

        property BackgroundColor : TColor read FBackgroundColor;
        property Count: Integer read GetCount;
        property Width: integer read FWidth;
        property Height: integer read FHeight;
        property Paused: Boolean read FPaused;
        property Bitmap: TBitmap read GetBitmap;
        property MemBitmap: TMemBitmap read GetMemBitmap;
        property CurrentImage: integer read FCurrentImage write SetCurrentImage;
    end;

    { TFPReaderGIF }

    TFPReaderGIF = class (TFPCustomImageReader)
    protected
      procedure InternalRead  (Str:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Str:TStream) : boolean; override;
    end;

const
   GifBackgroundModeStr : array[TGifBackgroundMode] of string =
     ('gbmSimplePaint', 'gbmEraseBackground', 'gbmSaveBackgroundOnce', 'gbmUpdateBackgroundContinuously');

implementation

Const AlphaMask = $FF000000;

TYPE TGIFSignature=packed array[1..6] of char;

     TGIFScreenDescriptor=packed RECORD
       width,height:word;
       flags,background,map:byte;
     END;

     TGIFImageDescriptor=packed RECORD
       x,y,width,height:word;
       flags:byte;
     END;

     TGIFExtensionBlock=packed RECORD
       functioncode:byte;
     END;

     TGIFGraphicControlExtension=packed RECORD
       flags:byte;
       delaytime:word;
       transcolor:byte;
     END;

{ TAnimatedGif }

class function TAnimatedGif.GetFileExtensions: string;
begin
  Result := 'gif';
end;

procedure TAnimatedGif.Render(StretchWidth,StretchHeight: integer);
var curDate: TDateTime;
    previousImage, nextImage: integer;

begin
  if FInternalVirtualScreen = nil then
  begin
      FInternalVirtualScreen := TMemBitmap.Create(FWidth,FHeight);
      if Count = 0 then FInternalVirtualScreen.Fill(BackgroundColor) else FInternalVirtualScreen.Fill(MemPixelTransparent);
      FImageChanged := true;
  end;

  if Count = 0 then exit;

  previousImage := FCurrentImage;

  curDate := Now;
  if FWantedImage <> - 1 then
  begin
     nextImage := FWantedImage;
     FTimeAccumulator:= 0;
     FWantedImage := -1;
  end else
  if FCurrentImage = -1 then
  begin
     nextImage := 0;
     FTimeAccumulator := 0;
     FPreviousDisposeMode := dmNone;
  end else
  begin
     if not FPaused then FTimeAccumulator += (curDate-FPrevDate)*24*60*60*1000;
     nextImage := FCurrentImage;
     while FTimeAccumulator> FImages[nextImage].Delay do
     begin
       FTimeAccumulator -= FImages[nextImage].Delay;
       inc(nextImage);
       if nextImage >= Count then nextImage := 0;

       if nextImage = previousImage then
       begin
         inc(nextImage);
         if nextImage >= Count then nextImage := 0;
         break;
       end;
     end;
  end;
  FPrevDate := curDate;

  while FCurrentImage<>nextImage do
  begin
    inc(FCurrentImage);
    if FCurrentImage >= Count then
    begin
      FCurrentImage := 0;
      FPreviousDisposeMode := dmErase;
    end;

    case FPreviousDisposeMode of
    dmErase: FInternalVirtualScreen.Fill(MemPixelTransparent);
    dmRestore: if FRestoreImage <> nil then FInternalVirtualScreen.PutImage(0,0,FRestoreImage,dmSet);
    end;

    with FImages[FCurrentImage] do
    begin
      If disposeMode = dmRestore then
      begin
        if FRestoreImage = nil then FRestoreImage := TMemBitmap.Create(FWidth,FHeight);
        FRestoreImage.PutImage(0,0,FInternalVirtualScreen,dmSet);
      end;

      if Image <> nil then
          FInternalVirtualScreen.PutImage(Position.X,Position.Y,Image,dmSetExceptTransparent);
      FPreviousDisposeMode := disposeMode;
    end;

    FImageChanged := true;
    previousImage := FCurrentImage;
    FInternalVirtualScreen.InvalidateBitmap;
  end;

  if FStretchedVirtualScreen <> nil then FStretchedVirtualScreen.FreeReference;
  if (FInternalVirtualScreen.Width = StretchWidth) and (FInternalVirtualScreen.Height = StretchHeight) then
    FStretchedVirtualScreen := FInternalVirtualScreen.NewReference else
      FStretchedVirtualScreen := FInternalVirtualScreen.Resample(StretchWidth,StretchHeight);
end;

procedure TAnimatedGif.UpdateSimple(Canvas: TCanvas; ARect: TRect; DrawOnlyIfChanged: Boolean = true);
begin
  if FPreviousVirtualScreen <> nil then
  begin
    FPreviousVirtualScreen.FreeReference;
    FPreviousVirtualScreen := nil;
  end;

  Render(ARect.Right-ARect.Left,ARect.Bottom-ARect.Top);
  if FImageChanged then
  begin
    FStretchedVirtualScreen.Draw(Canvas,ARect.Left,ARect.Top);
    FImageChanged := false;
  end else
    if not DrawOnlyIfChanged then FStretchedVirtualScreen.Draw(Canvas,ARect.Left,ARect.Top);

  FPreviousVirtualScreen := FStretchedVirtualScreen.NewReference;
end;

function TAnimatedGif.GetCount: integer;
begin
  result := length(FImages);
end;

constructor TAnimatedGif.Create(filename: string);
var Stream: TFileStream;
begin
  inherited Create;
  Init;
  Stream := TFileStream.Create(filename,fmOpenRead);
  LoadFromStream(Stream);
  Stream.Free;
end;

constructor TAnimatedGif.Create(stream: TStream);
begin
  inherited Create;
  Init;
  LoadFromStream(stream);
end;

constructor TAnimatedGif.Create;
begin
  inherited Create;
  Init;
  LoadFromStream(nil);
end;

function TAnimatedGif.Duplicate: TAnimatedGif;
var i: integer;
begin
     result := TAnimatedGif.Create;
     setlength(result.FImages, length(FImages));
     for i := 0 to high(FImages) do
     begin
       result.FImages[i] := FImages[i];
       FImages[i].Image.NewReference;
     end;
     result.FWidth := FWidth;
     result.FHeight := FHeight;
     result.FBackgroundColor := FBackgroundColor;
end;

procedure TAnimatedGif.LoadFromStream(Stream: TStream);
begin
  FCurrentImage := -1;
  FWantedImage := -1;
  FTimeAccumulator := 0;

  if FStretchedVirtualScreen <> nil then FStretchedVirtualScreen.FreeReference;
  if FPreviousVirtualScreen <> nil then FPreviousVirtualScreen.FreeReference;
  FInternalVirtualScreen.Free;
  FRestoreImage.Free;
  FBackgroundImage.Free;

  FInternalVirtualScreen := nil;
  FStretchedVirtualScreen := nil;
  FRestoreImage := nil;
  FBackgroundImage := nil;
  FPreviousVirtualScreen := nil;

  EraseColor := clBlack;
  FPreviousDisposeMode := dmNone;

  FWidth := 0;
  FHeight := 0;

  if Stream<>nil then LoadImages(Stream);
end;

procedure TAnimatedGif.SaveToStream(Stream: TStream);
begin
     //not implemented
end;

{$HINTS OFF}
procedure TAnimatedGif.LoadImages(stream: TStream);

  PROCEDURE DumpData;
  VAR count:byte;
  BEGIN
    REPEAT
      stream.read(count,1);
      stream.position := stream.position + count;
    UNTIL (count=0) OR (stream.position>=stream.size);
  END;

type
     TRGB=packed RECORD
       r,g,b:byte;
     END;

     TPalette= array of TMemPixel;

  function rgbToColor(rgb: TRGB): TMemPixel;
  begin
       result.red := rgb.r;
       result.green := rgb.g;
       result.blue := rgb.b;
       result.alpha := 255;
  end;

const GIFScreenDescriptor_GlobalColorTableFlag = $80;
      GIFImageDescriptor_LocalColorTableFlag = $80;
      GIFImageDescriptor_InterlacedFlag = $40;
      GIFGraphicControlExtension_TransparentFlag = $01;

CONST ilstart:array[1..4] of longint=(0,4,2,1);
      ilstep:array[1..4] of longint=(8,8,4,2);

var NewImages: array of TGifSubImage;
    NbImages: integer;

    GIFSignature:TGIFSignature;
    GIFScreenDescriptor:TGIFScreenDescriptor;
    GIFBlockID:char;
    GIFImageDescriptor:TGIFImageDescriptor;

    globalPalette: TPalette;
    localPalette: TPalette;

    transcolorIndex: integer;
    delay: integer;
    disposeMode: TDisposeMode;

  procedure LoadGlobalPalette;
  var NbEntries,i: integer;
      rgb: TRGB;
  begin
      NbEntries := 1 SHL (GIFScreenDescriptor.flags AND $07+1);
      setlength(globalPalette, NbEntries);
      FOR i:=0 TO NbEntries-1 DO
         BEGIN
           stream.read(rgb,3);
           globalPalette[i]:=rgbToColor(rgb);
         END;
  end;

  procedure LoadLocalPalette;
  var NbEntries,i: integer;
      rgb: TRGB;
  begin
      NbEntries := 1 SHL (GIFImageDescriptor.flags AND $07+1);
      setlength(localPalette, NbEntries);
      FOR i:=0 TO NbEntries-1 DO
         BEGIN
           stream.read(rgb,3);
           localPalette[i]:=rgbToColor(rgb);
         END;
  end;

  PROCEDURE decodeGIFLZW(image:TMemBitmap;const pal:TPalette;interlaced:boolean);
  VAR xd,yd:longint;
  CONST tablen=4095;
  TYPE Pstr=^Tstr;
       Tstr=RECORD
         prefix:Pstr;
         suffix:longint;
       END;
       Pstrtab=^Tstrtab;
       Tstrtab=array[0..tablen] of Tstr;

  VAR strtab:Pstrtab;
      oldcode,curcode,clearcode,endcode:longint;
      codesize,codelen,codemask:longint;
      stridx:longint;
      bitbuf,bitsinbuf:longint;
      bytbuf:array[0..255] of byte;
      bytinbuf,bytbufidx:byte;
      endofsrc:boolean;
      xcnt,ycnt,ystep,pass:longint;

    PROCEDURE InitStringTable;
    VAR i:longint;
    BEGIN
      new(strtab);
      clearcode:=1 SHL codesize;
      endcode:=clearcode+1;
      stridx:=endcode+1;
      codelen:=codesize+1;
      codemask:=(1 SHL codelen)-1;
      FOR i:=0 TO clearcode-1 DO
        BEGIN
          strtab^[i].prefix:=nil;
          strtab^[i].suffix:=i;
        END;
      FOR i:=clearcode TO tablen DO
        BEGIN
          strtab^[i].prefix:=nil;
          strtab^[i].suffix:=0;
        END;
    END;

    PROCEDURE ClearStringTable;
    VAR i:longint;
    BEGIN
      clearcode:=1 SHL codesize;
      endcode:=clearcode+1;
      stridx:=endcode+1;
      codelen:=codesize+1;
      codemask:=(1 SHL codelen)-1;
      FOR i:=clearcode TO tablen DO
        BEGIN
          strtab^[i].prefix:=nil;
          strtab^[i].suffix:=0;
        END;
    END;

    PROCEDURE DoneStringTable;
    BEGIN
      dispose(strtab);
    END;

    FUNCTION GetNextCode:longint;
    BEGIN
      WHILE (bitsinbuf<codelen) DO
        BEGIN
          IF (bytinbuf=0) THEN
            BEGIN
              stream.read(bytinbuf,1);
              IF (bytinbuf=0) THEN endofsrc:=TRUE;
              stream.read(bytbuf,bytinbuf);
              bytbufidx:=0;
            END;
          bitbuf:=bitbuf OR (longint(byte(bytbuf[bytbufidx])) SHL bitsinbuf);
          inc(bytbufidx);
          dec(bytinbuf);
          inc(bitsinbuf,8);
        END;
      getnextcode:=bitbuf AND codemask;
{DBG(bitbuf AND codemask);}
      bitbuf:=bitbuf SHR codelen;
      dec(bitsinbuf,codelen);
    END;

    PROCEDURE AddStr2Tab(prefix:Pstr;suffix:longint);
    BEGIN
      strtab^[stridx].prefix:=prefix;
      strtab^[stridx].suffix:=suffix;
      inc(stridx);
      CASE stridx OF
      0..1:codelen:=1;
      2..3:codelen:=2;
      4..7:codelen:=3;
      8..15:codelen:=4;
      16..31:codelen:=5;
      32..63:codelen:=6;
      64..127:codelen:=7;
      128..255:codelen:=8;
      256..511:codelen:=9;
      512..1023:codelen:=10;
      1024..2047:codelen:=11;
      2048..4096:codelen:=12;
      END;
      codemask:=(1 SHL codelen)-1;
    END;

    FUNCTION Code2Str(code:longint):Pstr;
    BEGIN
      Code2Str:=addr(strtab^[code]);
    END;

    PROCEDURE WriteStr(s:Pstr);
    var colorIndex: integer;
    BEGIN
      IF (s^.prefix<>nil) THEN WriteStr(s^.prefix);
      IF (ycnt>=yd) THEN
        BEGIN
          IF interlaced THEN
            BEGIN
              WHILE (ycnt>=yd) AND (pass<5) DO
                BEGIN
                  inc(pass);
                  ycnt:=ilstart[pass];
                  ystep:=ilstep[pass];
                END;
            END;
        END;

      colorIndex := s^.suffix;
      if (colorIndex <> transcolorIndex) and (colorIndex >=0) and (colorIndex < length(pal)) then
        image.setpixel(xcnt,ycnt,pal[colorIndex]);

      inc(xcnt);
      IF (xcnt>=xd) THEN
        BEGIN
          xcnt:=0;
          inc(ycnt,ystep);

          IF NOT interlaced THEN
            IF (ycnt>=yd) THEN
              BEGIN
                inc(pass);
              END;

        END;
    END;

    FUNCTION firstchar(s:Pstr):byte;
    BEGIN
      WHILE (s^.prefix<>nil) DO s:=s^.prefix;
      firstchar:=s^.suffix;
    END;

  BEGIN
{DBG('lzw start');}
    endofsrc:=FALSE;
    xd:=image.width;
    yd:=image.height;
    xcnt:=0;
    IF interlaced THEN
      BEGIN
        pass:=1;
        ycnt:=ilstart[pass];
        ystep:=ilstep[pass];
      END
    ELSE
      BEGIN
        pass:=4;
        ycnt:=0;
        ystep:=1;
      END;
    oldcode:=0;
    bitbuf:=0;
    bitsinbuf:=0;
    bytinbuf:=0;
    bytbufidx:=0;
    codesize:=0;
    stream.read(codesize,1);
{DBG(codesize);}
    InitStringTable;
    curcode:=getnextcode;
{DBG(curcode);}
    WHILE (curcode<>endcode) AND (pass<5) AND NOT endofsrc{ AND NOT finished} DO
      BEGIN
{DBG('-----');
DBG(curcode);
DBGw(stridx);}
        IF (curcode=clearcode) THEN
          BEGIN
            ClearStringTable;
            REPEAT
              curcode:=getnextcode;
{DBG('lzw clear');}
            UNTIL (curcode<>clearcode);
            IF (curcode=endcode) THEN break;
            WriteStr(code2str(curcode));
            oldcode:=curcode;
          END
        ELSE
          BEGIN
            IF (curcode<stridx) THEN
              BEGIN
                WriteStr(Code2Str(curcode));
                AddStr2Tab(Code2Str(oldcode),firstchar(Code2Str(curcode)));
                oldcode:=curcode;
              END
            ELSE
              BEGIN
                IF (curcode>stridx) THEN break;
                AddStr2Tab(Code2Str(oldcode),firstchar(Code2Str(oldcode)));
                WriteStr(Code2Str(stridx-1));
                oldcode:=curcode;
              END;
          END;
        curcode:=getnextcode;
      END;
    DoneStringTable;
{putimage(0,0,image);}
{DBG('lzw end');
DBG(bytinbuf);}
    IF NOT endofsrc THEN DumpData;
{DBG('lzw finished');}
  END;

  procedure LoadImage;
  var imgWidth,imgHeight: integer;
      img: TMemBitmap;
      Interlaced: boolean;
      palette: TPalette;
  BEGIN
       stream.read(GIFImageDescriptor,sizeof(GIFImageDescriptor));
       IF (GIFImageDescriptor.flags AND GIFImageDescriptor_LocalColorTableFlag=GIFImageDescriptor_LocalColorTableFlag) THEN LoadLocalPalette else
         localPalette := nil;

       if localPalette <> nil then palette := localPalette else palette := globalPalette;
       imgWidth:=GIFImageDescriptor.width;
       imgHeight:=GIFImageDescriptor.height;

       if length(NewImages) <= NbImages  then setlength(NewImages, length(NewImages)*2+1);
       img := TMemBitmap.Create(imgWidth,imgHeight);
       img.Fill(MemPixelTransparent);
       NewImages[NbImages].Image := img;
       NewImages[NbImages].Position := point(GIFImageDescriptor.x,GIFImageDescriptor.y);
       NewImages[NbImages].Delay:= Delay;
       NewImages[NbImages].disposeMode:= disposeMode;

       if (transcolorIndex >= 0) and (transcolorIndex < length(palette)) then
         NewImages[nbImages].TransparentColor:= palette[transcolorIndex] else
           NewImages[nbImages].TransparentColor := MemPixelTransparent;

       inc(NbImages);

       Interlaced := GIFImageDescriptor.flags AND GIFImageDescriptor_InterlacedFlag=GIFImageDescriptor_InterlacedFlag;
       DecodeGIFLZW(img,palette,Interlaced);
  END;

  procedure ChangeImages;
  var i: integer;
  begin
     Clear;
     SetLength(FImages,NbImages);
     for i:= 0 to Count-1 do
       FImages[i] := NewImages[i];
  end;

  procedure ReadExtension;
  var
    GIFExtensionBlock:TGIFExtensionBlock;
    GIFGraphicControlExtension:TGIFGraphicControlExtension;
    mincount, count: byte;

  begin
    stream.read(GIFExtensionBlock,sizeof(GIFExtensionBlock));
    CASE GIFExtensionBlock.functioncode OF
    $F9:BEGIN
          stream.read(count,1);
          if count < sizeof(GIFGraphicControlExtension) then
            mincount := 0 else
          begin
            mincount := sizeof(GIFGraphicControlExtension);
            stream.read(GIFGraphicControlExtension,mincount);

            if GIFGraphicControlExtension.flags and GIFGraphicControlExtension_TransparentFlag = GIFGraphicControlExtension_TransparentFlag then
              transcolorIndex:=GIFGraphicControlExtension.transcolor else
                transcolorIndex := -1;
            if GIFGraphicControlExtension.delaytime <> 0 then Delay := GIFGraphicControlExtension.delaytime*10;
            disposeMode := TDisposeMode((GIFGraphicControlExtension.flags shr 2) and 7);
          end;
          stream.Position:= Stream.Position+count-mincount;
          DumpData;
        END;
    ELSE
      BEGIN
        DumpData;
      END;
    END;
  end;

begin
     NewImages := nil;
     NbImages := 0;
     transcolorIndex := -1;
     Delay := 100;
     FBackgroundColor:= clBlack;
     FWidth := 0;
     FHeight := 0;
     disposeMode := dmErase;

     stream.Read(GIFSignature,sizeof(GIFSignature));
     IF (GIFSignature[1]='G') AND (GIFSignature[2]='I') AND (GIFSignature[3]='F') THEN
     BEGIN
      stream.read(GIFScreenDescriptor,sizeof(GIFScreenDescriptor));
      FWidth := GIFScreenDescriptor.width;
      FHeight := GIFScreenDescriptor.Height;
      IF (GIFScreenDescriptor.flags AND GIFScreenDescriptor_GlobalColorTableFlag=GIFScreenDescriptor_GlobalColorTableFlag) THEN
      begin
         LoadGlobalPalette;
         if GIFScreenDescriptor.background < length(globalPalette) then
           FBackgroundColor := MemPixelToColor(globalPalette[GIFScreenDescriptor.background]);
      end;
      REPEAT
        stream.read(GIFBlockID,sizeof(GIFBlockID));
        CASE GIFBlockID OF
        ';':;
        ',': LoadImage;
        '!': ReadExtension;
        ELSE
        begin
         raise Exception.Create('TAnimatedGif: unexpected block type');
         break;
        end;
        END;
      UNTIL (GIFBlockID=';') OR (stream.Position>=stream.size);
     END else
       raise Exception.Create('TAnimatedGif: invalid header');
     ChangeImages;
end;

procedure TAnimatedGif.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if FBackgroundImage <> nil then FreeAndNil(FBackgroundImage);
  SaveBackgroundOnce(ACanvas, Rect);

  if FPreviousVirtualScreen <> nil then
  begin
    FPreviousVirtualScreen.FreeReference;
    FPreviousVirtualScreen := nil;
  end;

  Render(Rect.Right-Rect.Left,Rect.Bottom-Rect.Top);
  FStretchedVirtualScreen.Draw(ACanvas,Rect.Left,Rect.Top);
  FImageChanged := false;

  FPreviousVirtualScreen := FStretchedVirtualScreen.Duplicate;
end;

function TAnimatedGif.GetEmpty: Boolean;
begin
  Result:= (length(FImages)=0);
end;

function TAnimatedGif.GetHeight: Integer;
begin
  Result:= FHeight;
end;

function TAnimatedGif.GetTransparent: Boolean;
begin
  Result:= true;
end;

function TAnimatedGif.GetWidth: Integer;
begin
  Result:= FWidth;
end;

procedure TAnimatedGif.SetHeight(Value: Integer);
begin
  //not implemented
end;

procedure TAnimatedGif.SetTransparent(Value: Boolean);
begin
  //not implemented
end;

procedure TAnimatedGif.SetWidth(Value: Integer);
begin
  //not implemented
end;

procedure TAnimatedGif.SaveBackgroundOnce(Canvas: TCanvas; ARect: TRect);
begin
     if (FBackgroundImage <> nil) and ((FBackgroundImage.Width <> ARect.Right-ARect.Left) or
         (FBackgroundImage.Height <> ARect.Bottom-ARect.Top)) then FreeAndNil(FBackgroundImage);

     if (BackgroundMode in [gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously]) and (FBackgroundImage = nil) then
     begin
       FBackgroundImage := TMemBitmap.Create(ARect.Right-ARect.Left,ARect.Bottom-ARect.Top);
       FBackgroundImage.GetImage(Canvas,ARect.Left,ARect.Top,EraseColor);
     end;
end;

procedure TAnimatedGif.SetCurrentImage(Index: integer);
begin
  if (Index >= 0) and (Index < Length(FImages)) then FWantedImage := Index;
end;

{$HINTS ON}

procedure TAnimatedGif.Clear;
var i: integer;
begin
  inherited Clear;

  for i := 0 to Count-1 do
    FImages[i].Image.FreeReference;
  FImages := nil;
end;

destructor TAnimatedGif.Destroy;
begin
  Clear;

  if FStretchedVirtualScreen <> nil then FStretchedVirtualScreen.FreeReference;
  if FPreviousVirtualScreen <> nil then FPreviousVirtualScreen.FreeReference;
  FInternalVirtualScreen.Free;
  FRestoreImage.Free;
  FBackgroundImage.Free;
  inherited Destroy;
end;

procedure TAnimatedGif.Pause;
begin
  FPaused := true;
end;

procedure TAnimatedGif.Resume;
begin
  FPaused := false;
end;

procedure TAnimatedGif.Show(Canvas: TCanvas; ARect: TRect);
begin
  Canvas.StretchDraw(ARect,self);
end;

procedure TAnimatedGif.Update(Canvas: TCanvas; ARect: TRect);
var
   n: integer;
   PChangePix,PNewPix, PBackground, PNewBackground: PLongWord;
   oldpix,newpix,newbackpix: LongWord;
   NewBackgroundImage: TMemBitmap;
begin
     if (BackgroundMode = gbmUpdateBackgroundContinuously) and (FBackgroundImage = nil) then BackgroundMode := gbmSaveBackgroundOnce;

     SaveBackgroundOnce(Canvas, ARect);

     case BackgroundMode of
     gbmSimplePaint:
       begin
         UpdateSimple(Canvas, ARect);
         exit;
       end;
     gbmEraseBackground:
       begin
         UpdateEraseBackground(Canvas, ARect);
         exit;
       end;
     gbmSaveBackgroundOnce, gbmUpdateBackgroundContinuously:
       begin
         if FPreviousVirtualScreen <> nil then
         begin
           if (FPreviousVirtualScreen.Width <> ARect.Right-ARect.Left) or
             (FPreviousVirtualScreen.Height <> ARect.Bottom-ARect.Top) then
           begin
             FPreviousVirtualScreen.FreeReference;
             FPreviousVirtualScreen := nil;
           end
             else
               FPreviousVirtualScreen := FPreviousVirtualScreen.GetUnique;
         end;

         Render(ARect.Right-ARect.Left,ARect.Bottom-ARect.Top);

         if FImageChanged then
         begin
           if BackgroundMode = gbmUpdateBackgroundContinuously then
           begin
             NewBackgroundImage := TMemBitmap.Create(FStretchedVirtualScreen.Width,FStretchedVirtualScreen.Height);
             NewBackgroundImage.GetImage(Canvas,ARect.Left,ARect.Top,EraseColor);

             if FPreviousVirtualScreen=nil then
             begin
               FPreviousVirtualScreen := TMemBitmap.Create(FWidth,FHeight);
               FPreviousVirtualScreen.Fill(MemPixelTransparent);
             end;

             PChangePix := PLongWord(FPreviousVirtualScreen.ScanLine[0]);
             PNewPix := PLongWord(FStretchedVirtualScreen.ScanLine[0]);
             PBackground := PLongWord(FBackgroundImage.ScanLine[0]);
             PNewBackground := PLongWord(NewBackgroundImage.ScanLine[0]);
             for n := FStretchedVirtualScreen.NbPixels-1 downto 0 do
             begin
               oldpix := PChangePix^;

               if (oldpix and AlphaMask = AlphaMask) then //pixel opaque précédent
               begin
                 newbackpix := PNewBackground^;
                 if (newbackpix <> oldpix) then //stocke nouveau fond
                   PBackground^ := newbackpix;
               end;

               newpix := PNewPix^;

               if newpix and AlphaMask = AlphaMask then
               begin
                  PChangePix^ := newpix; //pixel opaque
                  if oldpix and AlphaMask = 0 then //pixel transparent précédent
                    PBackground^ := PNewBackground^;
               end
               else if newpix and AlphaMask > 0 then
               begin
                 PChangePix^ := PBackground^;
                 TMemBitmap.DrawPixel(PMemPixel(PChangePix),PMemPixel(@newpix)^);
               end
               else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := PBackground^; //efface précédent

{               if newpix and AlphaMask > AlphaLimit then PChangePix^ := newpix or AlphaMask //pixel opaque
               else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := PBackground^; //efface précédent}

               inc(PNewPix);
               inc(PChangePix);
               inc(PBackground);
               inc(PNewBackground);
             end;
             NewBackgroundImage.Free;
             FPreviousVirtualScreen.InvalidateBitmap;
             FPreviousVirtualScreen.Draw(Canvas,ARect.Left,ARect.Top);
             FPreviousVirtualScreen.PutImage(0,0,FStretchedVirtualScreen,dmSet);
           end else
           begin
             if FPreviousVirtualScreen =nil then
             begin
               FStretchedVirtualScreen.Draw(Canvas,ARect.Left,ARect.Top);
               FPreviousVirtualScreen := FStretchedVirtualScreen.NewReference;
             end else
             begin
               PChangePix := PLongWord(FPreviousVirtualScreen.ScanLine[0]);
               PNewPix := PLongWord(FStretchedVirtualScreen.ScanLine[0]);
               PBackground := PLongWord(FBackgroundImage.ScanLine[0]);
               for n := FStretchedVirtualScreen.NbPixels-1 downto 0 do
               begin
                 newpix := PNewPix^;

                 if newpix and AlphaMask = AlphaMask then PChangePix^ := newpix //pixel opaque
                 else if newpix and AlphaMask > 0 then
                 begin
                   PChangePix^ := PBackground^;
                   TMemBitmap.DrawPixel(PMemPixel(PChangePix),PMemPixel(@newpix)^);
                 end
                 else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := PBackground^; //efface précédent

{                 if newpix and AlphaMask > AlphaLimit then PChangePix^ := newpix or AlphaMask //pixel opaque
                 else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := PBackground^; //efface précédent}

                 inc(PNewPix);
                 inc(PChangePix);
                 inc(PBackground);
               end;
               FPreviousVirtualScreen.InvalidateBitmap;
               FPreviousVirtualScreen.Draw(Canvas,ARect.Left,ARect.Top);
               FPreviousVirtualScreen.PutImage(0,0,FStretchedVirtualScreen,dmSet);
             end;
           end;
           FImageChanged := false;
         end;
       end;
     end;
end;

procedure TAnimatedGif.Hide(Canvas: TCanvas; ARect: TRect);
var shape: TMemBitmap; p,pback: PMemPixel;
    MemEraseColor : TMemPixel; n: integer;
begin
     MemEraseColor := MemPixel(EraseColor);
     if FPreviousVirtualScreen <> nil then
     begin
       if (FPreviousVirtualScreen.Width <> ARect.Right-ARect.Left) or
         (FPreviousVirtualScreen.Height <> ARect.Bottom-ARect.Top) then
         begin
             FPreviousVirtualScreen.FreeReference;
             FPreviousVirtualScreen := nil;
         end
     end;

     case BackgroundMode of
     gbmEraseBackground, gbmSimplePaint:
       begin
         if FPreviousVirtualScreen <> nil then
         begin
              shape := FPreviousVirtualScreen.Duplicate;
              p := shape.ScanLine[0];
              for n := shape.NbPixels-1 downto 0 do
              begin
                if p^.alpha <> 0 then p^ := MemEraseColor else
                   p^:= MemPixelTransparent;
                inc(p);
              end;
              shape.Draw(Canvas,ARect.Left,ARect.Top);
              shape.FreeReference;
         end;
       end;
     gbmSaveBackgroundOnce,gbmUpdateBackgroundContinuously:
       begin
         if (FPreviousVirtualScreen <> nil) and (FBackgroundImage <> nil) then
         begin
              shape := FPreviousVirtualScreen.Duplicate;
              p := shape.ScanLine[0];
              pback := FBackgroundImage.ScanLine[0];
              for n := shape.NbPixels-1 downto 0 do
              begin
                if p^.alpha <> 0 then p^ := pback^ else
                   p^:= MemPixelTransparent;
                inc(p);
                inc(pback);
              end;
              shape.Draw(Canvas,ARect.Left,ARect.Top);
              shape.FreeReference;
         end;
       end;
     end;
end;

procedure TAnimatedGif.UpdateEraseBackground(Canvas: TCanvas; ARect: TRect; DrawOnlyIfChanged: Boolean);
var
   n: integer;
   PChangePix,PNewPix: PLongWord;
   newpix: LongWord;
   MemPixEraseColor : LongWord;
begin
     if EraseColor = clNone then
     begin
         UpdateSimple(Canvas, ARect, DrawOnlyIfChanged);
         exit;
     end;

     if FPreviousVirtualScreen <> nil then
     begin
       if (FPreviousVirtualScreen.Width <> ARect.Right-ARect.Left) or
         (FPreviousVirtualScreen.Height <> ARect.Bottom-ARect.Top) then
         begin
             FPreviousVirtualScreen.FreeReference;
             FPreviousVirtualScreen := nil;
         end
          else
           FPreviousVirtualScreen := FPreviousVirtualScreen.GetUnique;
     end;

     Render(ARect.Right-ARect.Left,ARect.Bottom-ARect.Top);
     if FImageChanged then
     begin
       PMemPixel(@MemPixEraseColor)^ := MemPixel(EraseColor);
       if FPreviousVirtualScreen =nil then
       begin
         FStretchedVirtualScreen.Draw(Canvas,ARect.Left,ARect.Top);
         FPreviousVirtualScreen := FStretchedVirtualScreen.NewReference;
       end else
       begin
         PChangePix := PLongWord(FPreviousVirtualScreen.ScanLine[0]);
         PNewPix := PLongWord(FStretchedVirtualScreen.ScanLine[0]);
         for n := FStretchedVirtualScreen.NbPixels-1 downto 0 do
         begin
           newpix := PNewPix^;

               if newpix and AlphaMask = AlphaMask then PChangePix^ := newpix //pixel opaque
               else if newpix and AlphaMask > 0 then
               begin
                 PChangePix^ := MemPixEraseColor;
                 TMemBitmap.DrawPixel(PMemPixel(PChangePix),PMemPixel(@newpix)^);
               end
               else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := MemPixEraseColor; //efface précédent
{           if newpix and AlphaMask > AlphaLimit then PChangePix^ := newpix or AlphaMask //pixel opaque
           else if PChangePix^ and AlphaMask <> 0 then PChangePix^ := MemPixEraseColor; //efface précédent}

           inc(PNewPix);
           inc(PChangePix);
         end;
         FPreviousVirtualScreen.InvalidateBitmap;
         FPreviousVirtualScreen.Draw(Canvas,ARect.Left,ARect.Top);
         FPreviousVirtualScreen.PutImage(0,0,FStretchedVirtualScreen,dmSet);
       end;

       FImageChanged := false;
     end;
end;

procedure TAnimatedGif.Init;
begin
  BackgroundMode := gbmSaveBackgroundOnce;
end;

function TAnimatedGif.GetBitmap: TBitmap;
begin
  Render(FWidth,FHeight);
  result := FStretchedVirtualScreen.Bitmap;
end;

function TAnimatedGif.GetMemBitmap: TMemBitmap;
begin
  Render(FWidth,FHeight);
  result := FStretchedVirtualScreen;
end;

{ TFPReaderGIF }

procedure TFPReaderGIF.InternalRead(Str: TStream; Img: TFPCustomImage);
var gif: TAnimatedGif; x,y: integer; Mem: TMemBitmap;
begin
     gif := TAnimatedGif.Create(Str);
     Mem := gif.MemBitmap;
     if Img is TMemBitmap then
     begin
        TMemBitmap(Img).Assign(Mem);
     end else
     begin
       Img.SetSize(gif.Width,gif.Height);
       for y := 0 to gif.height-1 do
         for x := 0 to gif.width-1 do
          with Mem.GetPixel(x,y) do
           Img.Colors[x,y] := FPColor(red*$101,green*$101,blue*$101,alpha*$101);
     end;
     gif.Free;
end;

{$HINTS OFF}
function TFPReaderGIF.InternalCheck(Str: TStream): boolean;
var
    GIFSignature:TGIFSignature;
    savepos: int64;
begin
     savepos := str.Position;
     try
       str.Read(GIFSignature,sizeof(GIFSignature));
       IF (GIFSignature[1]='G') AND (GIFSignature[2]='I') AND (GIFSignature[3]='F') THEN
       BEGIN
         result := true;
       end else
         result := false;
     except on ex:exception do result := false;
     end;
     str.Position := savepos;
end;
{$HINTS ON}

initialization

  //Free Pascal Image
  ImageHandlers.RegisterImageReader ('Animated GIF', TAnimatedGif.GetFileExtensions, TFPReaderGIF);

  //Lazarus Picture
  TPicture.RegisterFileFormat(TAnimatedGif.GetFileExtensions,'Animated GIF', TAnimatedGif);

end.

