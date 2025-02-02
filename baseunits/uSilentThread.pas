{
        File: uSilentThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
        ---------------------------------------------
        As the name "silent" suggests, the job of theses classes is to get
        manga information from the site and add them to download list or
        favorites silently.
}

unit uSilentThread;

{$mode delphi}

interface

uses
  SysUtils, fgl, uBaseUnit, uData, uDownloadsManager,
  WebsiteModules, FMDOptions, httpsendthread, BaseThread, LazFileUtils, MultiLog;

type

  TMetaDataType = (MD_DownloadAll, MD_AddToFavorites, MD_ImportToFavorites);

  { TSilentThreadMetaData }

  TSilentThreadMetaData = class
  public
    MetaDataType: TMetaDataType;
    Module: TModuleContainer;
    Title: String;
    URL: String;
    SaveTo: String;
    Status: String;
    NumChapter: String;
    ChapterLinks: String;
    Enabled: Boolean;
    constructor Create(const AType: TMetaDataType;
      const AModule: TModuleContainer; const ATitle, AURL,
      ASaveTo, AStatus, ANumChapter, AChapterLinks: String; AEnabled: Boolean);
  end;

  TSilentThreadManager = class;

  { TSilentThread }

  TSilentThread = class(TBaseThread)
  private
    FManager: TSilentThreadManager;
    FInfo: TMangaInformation;
    // manga information from main thread
    FType: TMetaDataType;
    FModule: TModuleContainer;
    FTitle: String;
    FURL: String;
    FSaveTo: String;
    FStatus: String;
    FNumChapter: String;
    FChapterLinks: String;
    FEnabled: Boolean;
  protected
    procedure SyncDownloadAll;
    procedure SyncAddToFavorite;
    procedure SyncImportToFavorite;
    procedure Execute; override;
  public
    constructor Create(const AManager: TSilentThreadManager);
    destructor Destroy; override;
  end;

  TSilentThreadMetaDatas = TFPGList<TSilentThreadMetaData>;
  TSilentThreads = TFPGList<TSilentThread>;

  { TSilentThreadManager }

  TSilentThreadManager = class
  private
    FMetaDatasGuardian: TRTLCriticalSection;
    FThreadsGuardian: TRTLCriticalSection;
    FMetaDatas: TSilentThreadMetaDatas;
    FThreads: TSilentThreads;
    FLockAdd: Boolean;
    function GetCount: Integer; inline;
    procedure StartThread;
  protected
    procedure AddThread(const T: TSilentThread);
    procedure RemoveThread(const T: TSilentThread);
    function GetMetaData(const T: TSilentThread): Boolean;
  public
    procedure Add(const AType: TMetaDataType; const AModule: TModuleContainer; const ATitle, AURL: String); overload;
    procedure Add(const AType: TMetaDataType; const AModule: TModuleContainer; const ATitle, AURL: String;
      const ASaveTo: String); overload;
    procedure Add(const AType: TMetaDataType; const AModule: TModuleContainer; const ATitle, AURL: String;
      const ASaveTo, AStatus, ANumChapter, AChapterLinks: String; AEnabled: Boolean); overload;
    procedure StopAll(const WaitFor: Boolean = True);
    procedure UpdateLoadStatus;
    procedure BeginAdd;
    procedure EndAdd;
    property Count: Integer read GetCount;
    constructor Create;
    destructor Destroy; override;
  end;

resourcestring
  RS_SilentThreadLoadStatus = 'Loading: %d/%d';

implementation

uses
  frmMain, FMDVars;

{ TSilentThreadManager }

function TSilentThreadManager.GetCount: Integer;
begin
  Result := FMetaDatas.Count + FThreads.Count;
end;

procedure TSilentThreadManager.StartThread;
begin
  if FThreads.Count=0 then
    TSilentThread.Create(Self);
end;

procedure TSilentThreadManager.Add(const AType: TMetaDataType;
  const AModule: TModuleContainer; const ATitle, AURL: String);
begin
  Add(AType, AModule, ATitle, AURL, '', '', '', '', True);
end;

procedure TSilentThreadManager.Add(const AType: TMetaDataType;
  const AModule: TModuleContainer; const ATitle, AURL: String;
  const ASaveTo: String);
begin
  Add(AType, AModule, ATitle, AURL, ASaveTo, '', '', '', True);
end;

procedure TSilentThreadManager.Add(const AType: TMetaDataType;
  const AModule: TModuleContainer; const ATitle, AURL: String;
  const ASaveTo, AStatus, ANumChapter, AChapterLinks: String; AEnabled: Boolean);
begin
  if ((AType=MD_AddToFavorites) or (AType=MD_ImportToFavorites)) and (FavoriteManager.IsMangaExist(ATitle,AModule.ID)) then Exit;
  EnterCriticalsection(FMetaDatasGuardian);
  try
    FMetaDatas.Add(TSilentThreadMetaData.Create(
      AType, AModule, ATitle, AURL, ASaveTo,
      AStatus, ANumChapter, AChapterLinks, AEnabled));
    if not FLockAdd then
      StartThread;
  finally
    LeaveCriticalsection(FMetaDatasGuardian);
  end;
end;

procedure TSilentThreadManager.AddThread(const T: TSilentThread);
begin
  EnterCriticalSection(FThreadsGuardian);
  try
    FThreads.Add(T);
  finally
    LeaveCriticalSection(FThreadsGuardian);
  end;
end;

procedure TSilentThreadManager.RemoveThread(const T: TSilentThread);
begin
  EnterCriticalSection(FThreadsGuardian);
  try
    FThreads.Remove(T);
  finally
    LeaveCriticalSection(FThreadsGuardian);
  end;
end;

function TSilentThreadManager.GetMetaData(const T: TSilentThread): Boolean;
var
  M: TSilentThreadMetaData;
begin
  Result:=False;
  EnterCriticalSection(FMetaDatasGuardian);
  try
    if FMetaDatas.Count=0 then
    begin
      Exit;
    end;
    if FThreads.Count>OptionMaxBackgroundLoadThreads then
    begin
      Exit;
    end;

    M := FMetaDatas.Items[0];
    FMetaDatas.Delete(0);
    T.FType := M.MetaDataType;
    T.FModule := M.Module;
    T.FTitle := M.Title;
    T.FURL := M.URL;
    T.FSaveTo := M.SaveTo;
    T.FStatus := M.Status;
    T.FNumChapter := M.NumChapter;
    T.FChapterLinks := M.ChapterLinks;
    T.FEnabled := M.Enabled;
    M.Free;
    Result := True;

    if (FMetaDatas.Count>0) and (FThreads.Count<OptionMaxBackgroundLoadThreads) then
    begin
      TSilentThread.Create(Self);
    end;
  finally
    LeaveCriticalSection(FMetaDatasGuardian);
  end;
end;

procedure TSilentThreadManager.StopAll(const WaitFor: Boolean);
var
  i: Integer;
begin
  if Count = 0 then Exit;

  EnterCriticalsection(FThreadsGuardian);
  try
    if FThreads.Count > 0 then
      for i := 0 to FThreads.Count - 1 do
        FThreads[i].Terminate;
  finally
    LeaveCriticalsection(FThreadsGuardian);
  end;

  EnterCriticalsection(FMetaDatasGuardian);
  try
    if FMetaDatas.Count > 0 then
    begin
      for i := 0 to FMetaDatas.Count - 1 do
        FMetaDatas[i].Free;
      FMetaDatas.Clear;
    end;
  finally
    LeaveCriticalsection(FMetaDatasGuardian);
  end;

  if WaitFor then
    while FThreads.Count>0 do
      Sleep(HeartBeatRate);
end;

procedure TSilentThreadManager.UpdateLoadStatus;
begin
  if Count > 0 then
  begin
    MainForm.sbMain.Panels[1].Text := Format(RS_SilentThreadLoadStatus, [FThreads.Count, Count])
  end
  else
  begin
    MainForm.sbMain.Panels[1].Text := '';
  end;
end;

procedure TSilentThreadManager.BeginAdd;
begin
  FLockAdd := True;
end;

procedure TSilentThreadManager.EndAdd;
begin
  FLockAdd := False;
  if FMetaDatas.Count > 0 then
    StartThread;
end;

constructor TSilentThreadManager.Create;
begin
  inherited Create;
  InitCriticalSection(FMetaDatasGuardian);
  InitCriticalSection(FThreadsGuardian);
  FLockAdd := False;
  FMetaDatas := TSilentThreadMetaDatas.Create;
  FThreads := TSilentThreads.Create;
end;

destructor TSilentThreadManager.Destroy;
begin
  StopAll(True);
  FMetaDatas.Free;
  FThreads.Free;
  DoneCriticalsection(FThreadsGuardian);
  DoneCriticalsection(FMetaDatasGuardian);
  inherited Destroy;
end;

{ TSilentThreadMetaData }

constructor TSilentThreadMetaData.Create(const AType: TMetaDataType;
  const AModule: TModuleContainer; const ATitle, AURL, ASaveTo,
  AStatus, ANumChapter, AChapterLinks: String; AEnabled: Boolean);
begin
  inherited Create;
  MetaDataType := AType;
  Module := AModule;
  Title := ATitle;
  URL := AURL;
  SaveTo := ASaveTo;
  Status := AStatus;
  NumChapter := ANumChapter;
  ChapterLinks := AChapterLinks;
  Enabled := AEnabled;
end;

{ TSilentThread }

procedure TSilentThread.SyncDownloadAll;
var
  d: TTaskContainer;
  s: String;
  i: Integer;
begin
  if FInfo.MangaInfo.NumChapter = 0 then
    Exit;
  try
    with MainForm do
    begin
      // add a new download task
      DLManager.Lock;
      try
        d := DLManager.AddTask;
        d.DownloadInfo.Module := FModule;

        if Trim(FTitle) = '' then
          FTitle := FInfo.MangaInfo.Title;
        for i := 0 to FInfo.MangaInfo.NumChapter - 1 do
        begin
          // generate folder name
          s := CustomRename(OptionChapterCustomRename,
                            FModule.Name,
                            FTitle,
                            FInfo.MangaInfo.Authors,
                            FInfo.MangaInfo.Artists,
                            FInfo.MangaInfo.ChapterNames.Strings[i],
                            Format('%.4d', [i + 1]),
                            OptionChangeUnicodeCharacter,
                            OptionChangeUnicodeCharacterStr);
          d.ChapterNames.Add(s);
          d.chapterLinks.Add(
            FInfo.MangaInfo.ChapterLinks.Strings[i]);
        end;

        if cbAddAsStopped.Checked then
        begin
          d.downloadInfo.Status := Format('[%d/%d] %s',[0,d.ChapterLinks.Count,RS_Stopped]);
          d.Status := STATUS_STOP;
        end
        else
        begin
          d.downloadInfo.Status := Format('[%d/%d] %s',[0,d.ChapterLinks.Count,RS_Waiting]);
          d.Status := STATUS_WAIT;
        end;

        d.currentDownloadChapterPtr := 0;
        d.downloadInfo.Link := FURL;
        d.downloadInfo.Title := FTitle;
        d.downloadInfo.DateAdded := Now;
        d.downloadInfo.DateLastDownloaded := Now;

        if FSaveTo = '' then
        begin
          FillSaveTo;
          OverrideSaveTo(d.DownloadInfo.Module);
          FSaveTo := edSaveTo.Text;
          // save to
          if OptionGenerateMangaFolder then
            FSaveTo := AppendPathDelim(FSaveTo) + CustomRename(
              OptionMangaCustomRename,
              FModule.Name,
              FTitle,
              FInfo.MangaInfo.Authors,
              FInfo.MangaInfo.Artists,
              '',
              '',
              OptionChangeUnicodeCharacter,
              OptionChangeUnicodeCharacterStr);
        end;
        d.downloadInfo.SaveTo := FSaveTo;
        d.DBInsert;
      finally
        DLManager.UnLock;
      end;

      UpdateVtDownload;
      DLManager.CheckAndActiveTask(False);

      // save downloaded chapters
      if FInfo.MangaInfo.ChapterLinks.Count > 0 then
      begin
        DLManager.DownloadedChapters.Chapters[FInfo.MangaInfo.ModuleID, FURL]:=
          FInfo.MangaInfo.ChapterLinks.Text;
        FavoriteManager.AddToDownloadedChaptersList(FInfo.MangaInfo.ModuleID,
          FURL, FInfo.MangaInfo.ChapterLinks);
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TSilentThread.SyncAddToFavorite;
var
  s: String;
begin
  try
    with MainForm do
    begin
      if Trim(FTitle) = '' then
        FTitle := FInfo.MangaInfo.Title;
      if FSaveTo = '' then
      begin
        FillSaveTo;
	OverrideSaveTo(FModule);
        s := edSaveTo.Text;
      end
      else
        s := FSaveTo;
      if OptionGenerateMangaFolder then
        s := AppendPathDelim(s) + CustomRename(
          OptionMangaCustomRename,
          FModule.Name,
          FTitle,
          FInfo.MangaInfo.Authors,
          FInfo.MangaInfo.Artists,
          '',
          '',
          OptionChangeUnicodeCharacter,
          OptionChangeUnicodeCharacterStr);

      FavoriteManager.Add(
        FModule,
        FTitle,
        FInfo.MangaInfo.Status,
        IntToStr(FInfo.MangaInfo.NumChapter),
        FInfo.MangaInfo.ChapterLinks.Text,
        s,
        FURL);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;
 
procedure TSilentThread.SyncImportToFavorite;
var
  s: String;
begin
  try
    with MainForm do
    begin
      if Trim(FTitle) = '' then
      begin
        FTitle := FInfo.MangaInfo.Title;
      end;
      if FSaveTo = '' then
      begin
        FillSaveTo;
	OverrideSaveTo(FModule);
        FSaveTo := edSaveTo.Text;
      end;

      FavoriteManager.Add(
        FModule,
        FTitle,
        FInfo.MangaInfo.Status,
        IntToStr(FInfo.MangaInfo.NumChapter),
        FInfo.MangaInfo.ChapterLinks.Text,
        FSaveTo,
        FURL,
        FEnabled);
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TSilentThread.Execute;

begin
  while FManager.GetMetaData(Self) do
  begin
    Synchronize(FManager.UpdateLoadStatus);

    try
      FInfo.HTTP.Reset;
      FInfo.MangaInfo.Clear;
      FInfo.Module := FModule;
      FInfo.MangaInfo.Title := FTitle;
      case FType of
        MD_DownloadAll:
          begin
            if (FInfo.GetInfoFromURL(FURL)=NO_ERROR) and not(Terminated) then
            begin
              Synchronize(SyncDownloadAll);
            end;
          end;
        MD_AddToFavorites:
          begin
            if (FInfo.GetInfoFromURL(FURL)=NO_ERROR) and not(Terminated) then
            begin
              Synchronize(SyncAddToFavorite);
            end;
          end;
        MD_ImportToFavorites:
          begin
            FInfo.MangaInfo.Status := FStatus;
            FInfo.MangaInfo.NumChapter := StrToIntDef(FNumChapter, 0);
            FInfo.MangaInfo.ChapterLinks.Text := FChapterLinks;
            FEnabled := FEnabled;
            Synchronize(SyncImportToFavorite);
          end;
      end;
    except
      on E: Exception do
        MainForm.ExceptionHandler(Self, E);
    end;
  end;

  if OptionSortDownloadsOnNewTasks then
  begin
    DLManager.Sort(DLManager.SortColumn);
  end;
  MainForm.UpdateVtFavorites;
  MainForm.vtFavoritesFilterCountChange;
end;

constructor TSilentThread.Create(const AManager: TSilentThreadManager);
begin
  inherited Create(False);
  FInfo:=TMangaInformation.Create(Self);
  FManager:=AManager;
  FManager.AddThread(Self);
end;

destructor TSilentThread.Destroy;
begin
  FManager.RemoveThread(Self);
  if not isExiting then
    Synchronize(FManager.UpdateLoadStatus);
  FInfo.Free;
  inherited Destroy;
end;

end.
