{
        File: uGetMangaInfosThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader

        -----------------

        This class allows us to get infomation on certain site and shows it
        in FMD's Manga infos tab.
}

unit uGetMangaInfosThread;

{$mode delphi}

interface

uses
  SysUtils, Graphics, Dialogs, Classes, Windows, uBaseUnit, uData, FMDOptions, BaseThread,
  ImgInfos, webp, MultiLog, MemBitmap, VirtualTrees, BGRAAnimatedGif,
  BGRABitmap, BGRABitmapTypes, ImageMagickManager;

type

  { TGetMangaInfosThread }

  TGetMangaInfosThread = class(TBaseThread)
  private
    FNode: PVirtualNode;
    FCover: TPicture;
    FTitle,
    FLink: String;
    FInfo: TMangaInformation;
    FNumChapter: Cardinal;
    FIsHasMangaCover: Boolean;
  protected
    procedure Execute; override;
    procedure MainThreadSyncInfos;
    procedure MainThreadShowInfos;
    procedure MainThreadShowCover;
    procedure MainThreadShowCannotGetInfo;
    procedure LoadCover;
  public
    constructor Create(const AModule: Pointer; const ALink: String; const ANode: PVirtualNode);
    destructor Destroy; override;
    property Title: String read FTitle write FTitle;
  end;

implementation

uses
  frmMain, frmCustomMessageDlg, WebsiteModules, FMDVars;

procedure TGetMangaInfosThread.MainThreadSyncInfos;
begin
  FInfo.SyncInfoToData(dataProcess);
  dataProcess.Commit;
end;

procedure TGetMangaInfosThread.Execute;
var
  m: TModuleContainer;

  function GetMangaInfo: Boolean;
  var
    infob: byte;
    data: PMangaInfoData;
  begin
    Result := False;
    try
      FInfo.MangaInfo.Link := FLink;
      FInfo.MangaInfo.Title := FTitle;
      data := MainForm.vtMangaList.GetNodeData(FNode);
      if Assigned(data) and (MainForm.cbSelectManga.ItemIndex<>-1) and
        (m = TModuleContainer(MainForm.cbSelectManga.Items.Objects[MainForm.cbSelectManga.ItemIndex])) then
      begin
        if FInfo.MangaInfo.Title = '' then
          FInfo.MangaInfo.Title := data^.Title;
        FInfo.MangaInfo.AltTitles := data^.AltTitles;
        FInfo.MangaInfo.Link := data^.Link;
        FInfo.MangaInfo.Authors := data^.Authors;
        FInfo.MangaInfo.Artists := data^.Artists;
        FInfo.MangaInfo.Status := data^.Status;
        FInfo.MangaInfo.Summary := data^.Summary;
        FInfo.MangaInfo.NumChapter := data^.NumChapter;
        FInfo.MangaInfo.Genres := data^.Genres;
        FNumChapter := data^.NumChapter;
      end;
      FInfo.isGenerateFolderChapterName := OptionGenerateMangaFolder;
      FInfo.isRemoveUnicode := OptionChangeUnicodeCharacter;

      infob := INFORMATION_NOT_FOUND;

      infob := FInfo.GetInfoFromURL(FLink);

      if Terminated or isExiting then Exit;
      if infob <> NO_ERROR then Exit;

      //set back if title changed
      if (FInfo.MangaInfo.Title <> '') and (FInfo.MangaInfo.Title <> FTitle) then
        FTitle := FInfo.MangaInfo.Title;

      data := MainForm.vtMangaList.GetNodeData(FNode);
      if Assigned(data) and dataProcess.WebsiteLoaded(m.ID) then //todo: use tmodulecontainer
        begin
          if not(m.InformationAvailable) then
          begin
            if FInfo.MangaInfo.AltTitles = '' then
              FInfo.MangaInfo.AltTitles := data^.AltTitles;
            if FInfo.MangaInfo.Authors = '' then
              FInfo.MangaInfo.Authors := data^.Authors;
            if FInfo.MangaInfo.Artists = '' then
              FInfo.MangaInfo.Artists := data^.Artists;
            if FInfo.MangaInfo.Genres = '' then
              FInfo.MangaInfo.Genres := data^.Genres;
            if FInfo.MangaInfo.Summary = '' then
              FInfo.MangaInfo.Summary := data^.Summary;
          end;

          if not (Terminated or isExiting) then
            Synchronize(MainThreadSyncInfos);
        end;
      Result := True;
    except
      on E: Exception do
        MainForm.ExceptionHandler(Self, E);
    end;
  end;

begin
  m := TModuleContainer(FInfo.Module);
  try
    if not GetMangaInfo then
    begin
      if not (Terminated or isExiting) then
        Synchronize(MainThreadShowCannotGetInfo);
    end
    else
    begin
      if Terminated or isExiting then Exit;
      Synchronize(MainThreadShowInfos);
      FCover.Clear;
      // If there's cover then we will load it to the TPicture component.
      if OptionEnableLoadCover and (Trim(FInfo.MangaInfo.CoverLink) <> '') then
        try
          FInfo.HTTP.Document.Clear;
          if FInfo.HTTP.GET(FInfo.MangaInfo.CoverLink) then
            LoadCover;
        except
        end;
      if not (Terminated or isExiting) then
        Synchronize(MainThreadShowCover);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowCannotGetInfo;
begin
  CenteredMessageDlg(MainForm, RS_DlgCannotGetMangaInfo, mtInformation, [mbOk], 0);
  MainForm.rmInformation.Clear;
  MainForm.tmAnimateMangaInfo.Enabled := False;
  MainForm.pbWait.Visible := False;
  MainForm.imCover.Picture.Assign(nil);
end;

procedure TGetMangaInfosThread.LoadCover;
var
  bmp: TMemBitmap;
  ext: String;
  animGif: TBGRAAnimatedGif;
  tempFile: String;
  imgMagick: TImageMagickManager;
  Output: TMemoryStream;
  identifyResult: String;
  fmtPos, commaPos: Integer;
begin
  FIsHasMangaCover := False;
  with FInfo.HTTP do
  begin
    ext := GetImageStreamExt(Document);
    Logger.Send('GetImageStreamExt: ' + ext);
    
    if (ext = '') and TImageMagickManager.Instance.PathFound then
    begin
      Logger.Send('Ext is blank, using Identify as fallback');
      Document.Position := 0;
      imgMagick := TImageMagickManager.Instance;
      identifyResult := imgMagick.Identify(Document);
      Logger.Send('Identify result: ' + identifyResult);
      if identifyResult <> '' then
      begin
        fmtPos := Pos('Format: ', identifyResult);
        if fmtPos > 0 then
        begin
          ext := Copy(identifyResult, fmtPos + Length('Format: '), Length(identifyResult));
          commaPos := Pos(',', ext);
          if commaPos > 0 then
            ext := Copy(ext, 1, commaPos - 1);
          ext := LowerCase(Trim(ext));
          Logger.Send('Identified ext: ' + ext);
        end;
      end;
    end;
    
    if (ext = 'gif') or (ext = 'png') then
    begin
      Document.Position := 0;
      animGif := TBGRAAnimatedGif.Create;
      try
        animGif.LoadFromStream(Document);
        FCover.Graphic := animGif;
        FIsHasMangaCover := True;
      except
        animGif.Free;
        Document.Position := 0;
        FCover.LoadFromStream(Document);
        FIsHasMangaCover := True;
      end;
    end
    else if (ext = 'tif') or (ext = 'tiff') or (ext = 'webp') or (ext = 'avif') then
    begin
      Document.Position := 0;
      Logger.Send(ext + ': Starting conversion. Size=' + IntToStr(Document.Size));
      imgMagick := TImageMagickManager.Instance;
      if imgMagick.PathFound then
      begin
        Logger.Send(ext + ': Calling ConvertStream to GIF...');
        Output := imgMagick.ConvertStream(Document, 'gif', True, ext);
        Logger.Send(ext + ': ConvertStream returned. Assigned=' + BoolToStr(Assigned(Output), True));
        if Assigned(Output) and (Output.Size > 0) then
        begin
          Output.Position := 0;
          Logger.Send(ext + ': Loading GIF from stream...');
          animGif := TBGRAAnimatedGif.Create;
          try
            animGif.LoadFromStream(Output);
            FCover.Graphic := animGif;
            FIsHasMangaCover := True;
            Logger.Send(ext + ': Cover loaded successfully');
          except
            on E: Exception do
            begin
              Logger.Send(ext + ': LoadFromStream failed: ' + E.Message);
              animGif.Free;
              raise;
            end;
          end;
        end
        else
          Logger.Send(ext + ': ConvertStream failed: ' + imgMagick.LastError);
        Output.Free;
      end
      else
        Logger.Send(ext + ': ImageMagick not available');
    end
    else
    begin
      try
        Document.Position := 0;
        FCover.LoadFromStream(Document);
        FIsHasMangaCover := True;
      except
        
      end;
    end;
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowInfos;
var node: PVirtualNode;
begin
  TransferMangaInfo(mangaInfo, FInfo.MangaInfo);
  with MainForm do
  try
    if Assigned(FNode) and dataProcess.WebsiteLoaded(TModuleContainer(FInfo.Module).ID) then   //todo: use tmodulecontainer
      begin
        vtMangaList.BeginUpdate;
        dataProcess.Refresh(dataProcess.Filtered);
        vtMangaList.ReinitNode(FNode, False);
        if dataProcess.Filtered then begin
          node := vtMangaList.GetNextVisible(FNode, False);
          while Assigned(node) do begin
            vtMangaList.ReinitNode(node, False);
            node := vtMangaList.GetNextVisible(node, False);
          end;
          vtMangaList.RootNodeCount := dataProcess.RecordCount;
          MainForm.UpdateVtMangaListFilterStatus;
        end;
        vtMangaList.EndUpdate;
      end;
    ShowInformation;
  except
    on E: Exception do
      Logger.SendException(Self.ClassName+'.MainThreadShowInfos error!', E);
  end;
end;

procedure TGetMangaInfosThread.MainThreadShowCover;
begin
  MainForm.tmAnimateMangaInfo.Enabled := False;
  MainForm.pbWait.Visible := False;
  if FIsHasMangaCover then
  begin
    try
      MainForm.imCover.Picture.Assign(FCover);
    except
      on E: Exception do ;
    end;
    FCover.Clear;
  end;
end;

constructor TGetMangaInfosThread.Create(const AModule: Pointer;
  const ALink: String; const ANode: PVirtualNode);
begin
  inherited Create(True);
  FCover := MainForm.mangaCover;
  FIsHasMangaCover := False;
  FInfo := TMangaInformation.Create(Self);
  FInfo.Module := AModule;
  FLink := ALink;
  FNode := ANode;
end;

destructor TGetMangaInfosThread.Destroy;
begin
  GetInfosThread := nil;
  FCover := nil;
  FInfo.Free;
  inherited Destroy;
end;

end.
