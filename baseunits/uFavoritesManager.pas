{
        File: uFavoritesManager.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uFavoritesManager;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, Dialogs, ExtCtrls,
  LazFileUtils, uBaseUnit, uData, uDownloadsManager, WebsiteModules, FMDOptions,
  httpsendthread, FavoritesDB, BaseThread, SQLiteData, MultiLog, SimpleException, VirtualTrees;

type
  TFavoriteManager = class;
  TFavoriteTask = class;
  TfavoriteContainer = class;

  { TFavoriteThread }

  TFavoriteThread = class(TBaseThread)
  private
    FTask: TFavoriteTask;
    FMangaInformation: TMangaInformation;
  protected
    procedure Execute; override;
    procedure DoCheck;
  public
    FContainer: TfavoriteContainer;
    constructor Create(const ATask: TFavoriteTask);
    destructor Destroy; override;
  end;

  TFavoriteThreads = TFPGList<TFavoriteThread>;

  { TFavoriteTask }

  TFavoriteTask = class(TBaseThread)
  private
    FBtnCaption: String;
    FPendingCount: Cardinal;
    FNeedRepaint: Integer;
    FTimerRepaint: TTimer;
    FThreads: TFavoriteThreads;
    FManager: TFavoriteManager;
    FCS_Threads: TRTLCriticalSection;
    FCS_GetNext: TRTLCriticalSection;
  protected
    procedure DoCustomTerminate(Sender: TObject);
    procedure TimerRepaintOnTimer(Sender: TObject);
    procedure SyncStartChecking;
    procedure SyncFinishChecking;
    procedure SyncUpdateBtnCaption;
    procedure Execute; override;
    procedure UpdateStatus;
  protected
    function GetNext(var C: TfavoriteContainer): Boolean;
    procedure AddThread(const T: TFavoriteThread);
    procedure RemoveThread(const T: TFavoriteThread);
  public
    procedure UpdateBtnCaption(Cap: String);
    constructor Create(const AManager: TFavoriteManager);
    destructor Destroy; override;
  end;

  { TFavoriteContainer }

  TFavoriteContainer = class
  private
    Fid: String;
    FOrder: Integer;
    FEnabled: Boolean;
    FManager: TFavoriteManager;
    procedure SetEnabled(AValue: Boolean);
  public
    Tag: Integer;
    FavoriteInfo: TFavoriteInfo;
    NewMangaInfo: TMangaInfo;
    NewMangaInfoChaptersPos: TCardinalList;
    Thread: TFavoriteThread;
    Status: TFavoriteStatusType;
    constructor Create(const M: TFavoriteManager);
    destructor Destroy; override;
  public
    procedure DBInsert; inline;
    procedure DBReplace(const OldId: String); inline;
    procedure DBUpdateTitle; inline;
    procedure DBUpdateEnabled; inline;
    procedure DBUpdateDateLastChecked; inline;
    procedure DBUpdateSaveTo; inline;
    procedure DBUpdateLastUpdated; inline;
  published
    property ID: String read Fid;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TFavoriteContainers = TFPGList<TFavoriteContainer>;

  { TFavoriteManager }

  TFavoriteManager = class
  private
    FGuardian: TRTLCriticalSection;
    FFavoritesDB: TFavoritesDB;
    FSortColumn: Integer;
    FSortDirection, FIsAuto, FIsRunning: Boolean;
    FEnabledCount: Integer;
    FUpdateOrderCount: Integer;
    function GetDisabledCount: Integer; inline;
    function GetFavoritesCount: Integer; inline;
    function GetFavorite(const Index: Integer): TFavoriteContainer;
  protected
    procedure DBUpdateOrder;
  public
    isRunningRestore: Boolean;
    Items: TFavoriteContainers;
    TaskThread: TFavoriteTask;
    DLManager: TDownloadManager;
    OnUpdateFavorite: procedure of object;
    OnUpdateDownload: procedure of object;

    constructor Create;
    destructor Destroy; override;

    //Check favorites
    procedure CheckForNewChapter(FavoriteIndex: Integer = -1);
    procedure StopChekForNewChapter(WaitFor: Boolean = True; FavoriteIndex: Integer = -1);

    // Show notification form after checking completed
    procedure ShowResult;
    // Return true if a manga exist in favorites
    function LocateManga(const ATitle, AWebsite: String): TFavoriteContainer;
    function IsMangaExist(const ATitle, AWebsite: String): Boolean; inline;
    function LocateMangaByLink(const AModuleID, ALink: String): TFavoriteContainer;
    function IsMangaExistByLink(const AModuleID, ALink: String): Boolean; inline;
    // Add new manga to the list
    procedure Add(const AModule: Pointer; const ATitle, AStatus, ACurrentChapter, ADownloadedChapterList, ASaveTo, ALink: String; AEnabled: Boolean = True);
    // Replace manga from same site
    procedure Replace(const OldId: String; const AModule: Pointer; const ATitle, AStatus, ACurrentChapter, ADownloadedChapterList, ASaveTo, ALink: String);
    // Delete directly, must be inside lock/unlock
    procedure Delete(const Pos: Integer);
    procedure Remove(const T: TFavoriteContainer);
    // Restore information from favorites.db
    procedure Restore;
    // Backup to favorites.db
    procedure Backup;
    // Add FFavorites downloadedchapterlist
    procedure AddToDownloadedChaptersList(const AWebsite, ALink: String; const AValue: TStrings);
    // sorting
    procedure Sort(const AColumn: Integer);
    // critical section
    procedure Lock; inline;
    procedure UnLock; inline;
    procedure UpdateOrder; inline;

    property Count: Integer read GetFavoritesCount;
    property EnabledCount: Integer read FEnabledCount;
    property DisabledCount: Integer read GetDisabledCount;
    property SortDirection: Boolean read FSortDirection write FSortDirection;
    property SortColumn: Integer read FSortColumn write FSortColumn;
    property isAuto: Boolean read FIsAuto write FIsAuto;
    property isRunning: Boolean read FIsRunning write FIsRunning;
    property Favorite[const Index: Integer]: TFavoriteContainer read GetFavorite; default;
    property DB: TFavoritesDB read FFavoritesDB;
  end;

resourcestring
  RS_DlgFavoritesCheckIsRunning = 'Favorites check is running!';
  RS_DlgNewChapterCaption = 'Found new chapter(s)';
  RS_LblNewChapterFound = 'Found %d new chapter from %d manga(s):';
  RS_FavoriteHasNewChapter = '%s <%s> has %d new chapter(s).';
  RS_BtnDownload = '&Download';
  RS_BtnAddToQueue = '&Add to queue';
  RS_BtnCancel = '&Cancel';
  RS_DlgCompletedMangaCaption = 'Found completed manga(s)';
  RS_LblMangaWillBeRemoved = '%d completed manga(s) will be removed:';
  RS_BtnRemove = '&Remove';
  RS_BtnCheckFavorites = 'Check for new chapter';

implementation

uses
  frmMain, frmNewChapter, FMDVars, DateUtils;

{ TFavoriteContainer }

procedure TFavoriteContainer.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if FEnabled then
    Inc(FManager.FEnabledCount)
  else
    Dec(FManager.FEnabledCount);
  if not FManager.isRunningRestore then
    DBUpdateEnabled;
end;

constructor TFavoriteContainer.Create(const M: TFavoriteManager);
begin
  FOrder:=-1;
  Tag := 0;
  FManager := M;
  FEnabled := True;
  Inc(FManager.FEnabledCount);
end;

destructor TFavoriteContainer.Destroy;
begin
  if Assigned(Thread) then
  begin
    Thread.Terminate;
    Thread.WaitFor;
    Thread := nil;
  end;
  if Assigned(NewMangaInfo) then
  begin
    NewMangaInfo.Free;
    NewMangaInfoChaptersPos.Free;
  end;
  inherited Destroy;
end;

procedure TFavoriteContainer.DBInsert;
begin
  Fid:=LowerCase(FavoriteInfo.ModuleID+FavoriteInfo.Link);
  with FavoriteInfo do
    FManager.FFavoritesDB.Add(
      Fid,
      FOrder,
      FEnabled,
      ModuleID,
      Link,
      Title,
      Status,
      CurrentChapter,
      DownloadedChapterList,
      SaveTo,
      DateAdded
      );
end;

procedure TFavoriteContainer.DBReplace(const OldId: String);
begin
  Fid := LowerCase(FavoriteInfo.ModuleID+FavoriteInfo.Link);
  with FavoriteInfo do
    FManager.FFavoritesDB.Replace(
      OldId,
      Fid,
      FOrder,
      FEnabled,
      ModuleID,
      Link,
      Title,
      Status,
      CurrentChapter,
      DownloadedChapterList,
      SaveTo,
      DateAdded
      );
end;

procedure TFavoriteContainer.DBUpdateTitle;
begin
  FManager.FFavoritesDB.UpdateTitle(Fid,FavoriteInfo.Title);
end;

procedure TFavoriteContainer.DBUpdateEnabled;
begin
  FManager.FFavoritesDB.UpdateEnabled(Fid,FEnabled);
end;

procedure TFavoriteContainer.DBUpdateDateLastChecked;
begin
  with FavoriteInfo do
    FManager.FFavoritesDB.UpdateLastChecked(Fid,Status,CurrentChapter,DateLastChecked);
end;

procedure TFavoriteContainer.DBUpdateSaveTo;
begin
  FManager.FFavoritesDB.UpdateSaveTo(Fid,FavoriteInfo.SaveTo);
end;

procedure TFavoriteContainer.DBUpdateLastUpdated;
begin
  with FavoriteInfo do
    FManager.FFavoritesDB.UpdateLastUpdated(Fid,DownloadedChapterList,DateLastUpdated);
end;

{ TFavoriteThread }

procedure TFavoriteThread.Execute;
begin
  while FTask.GetNext(FContainer) do
  begin
    DoCheck;

    if Terminated then
    begin
      FContainer.Status := STATUS_IDLE;
      // free unused objects
      if Assigned(FContainer.NewMangaInfo) then
      begin
        FreeAndNil(FContainer.NewMangaInfo);
        FreeAndNil(FContainer.NewMangaInfoChaptersPos);
      end;
    end
    else
      FContainer.Status := STATUS_CHECKED;
    FContainer.Thread := nil;
    if not Terminated then
      FTask.UpdateStatus;
  end;
end;

procedure TFavoriteThread.DoCheck;
var
  DLChapters: TStringList;
  i: Integer;
begin
  if (FContainer.FavoriteInfo.Link) = '' then Exit;

  FContainer.Status := STATUS_CHECKING;
  FTask.UpdateStatus;

  with FContainer do
    try
      FMangaInformation.HTTP.Reset;
      FMangaInformation.MangaInfo.Clear;
      FMangaInformation.Module := FavoriteInfo.Module;
      FMangaInformation.isGetByUpdater := False;
      // get new manga info
      FMangaInformation.GetInfoFromURL(FavoriteInfo.Link);
      if not Terminated then
      begin
        NewMangaInfo := FMangaInformation.MangaInfo.Clone;
        NewMangaInfoChaptersPos := TCardinalList.Create;
        // update current chapters count immedietly
        FavoriteInfo.CurrentChapter := IntToStr(NewMangaInfo.ChapterLinks.Count);
        FavoriteInfo.Status := NewMangaInfo.Status;
        if NewMangaInfo.ChapterLinks.Count > 0 then
        begin
          // tag 100 for transfer favorite, add all chapter to downloaded chapter list
          if FContainer.Tag = 100 then
          begin
            FavoriteInfo.DownloadedChapterList := NewMangaInfo.ChapterLinks.Text;
            FContainer.Tag := 0;
          end
          else
          try
            DLChapters := TStringList.Create;
            DLChapters.Sorted := False;
            DLChapters.Text := FavoriteInfo.DownloadedChapterList;
            DLChapters.Sorted := True;
            for i := 0 to NewMangaInfo.ChapterLinks.Count - 1 do
              if DLChapters.IndexOf(NewMangaInfo.ChapterLinks[i]) = -1 then
                NewMangaInfoChaptersPos.Add(i);
          finally
            DLChapters.Free;
          end;
        end;

        if not Terminated then
        begin
          FContainer.FavoriteInfo.DateLastChecked := Now;
          if (NewMangaInfoChaptersPos.Count <> 0) then
            FContainer.FavoriteInfo.DateLastUpdated := Now;
        end;

        // free unneeded objects
        if (NewMangaInfoChaptersPos.Count = 0) and
          (NewMangaInfo.Status <> MangaInfo_StatusCompleted) then
        begin
          FreeAndNil(NewMangaInfo);
          FreeAndNil(NewMangaInfoChaptersPos);
        end;
      end;
    except
      on E: Exception do
        ExceptionHandle(Self, E);
    end;
end;

constructor TFavoriteThread.Create(const ATask: TFavoriteTask);
begin
  inherited Create(False);
  FTask := ATask;
  FTask.AddThread(Self);
  FMangaInformation := TMangaInformation.Create(Self);
end;

destructor TFavoriteThread.Destroy;
begin
  FMangaInformation.Free;
  FTask.RemoveThread(Self);
  inherited Destroy;
end;

{ TFavoriteTask }

procedure TFavoriteTask.DoCustomTerminate(Sender: TObject);
var
  i: Integer;
begin
  // terminate all FThreads
  EnterCriticalsection(FCS_Threads);
  try
    for i := 0 to FThreads.Count - 1 do
      FThreads[i].Terminate;
  finally
    LeaveCriticalsection(FCS_Threads);
  end;
end;

procedure TFavoriteTask.TimerRepaintOnTimer(Sender: TObject);
begin
  if FNeedRepaint > 0 then
  begin
    InterlockedExchange(FNeedRepaint, 0);
    FormMain.vtFavorites.Repaint;
  end;
end;

procedure TFavoriteTask.SyncStartChecking;
begin
  with MainForm do begin
    btCancelFavoritesCheck.Visible := True;
    btFavoritesCheckNewChapter.Width :=
      btFavoritesCheckNewChapter.Width - btCancelFavoritesCheck.Width - 6;
    btFavoritesCheckNewChapter.Caption := RS_Checking;
    rbFavoritesShowAll.Enabled := False;
    rbFavoritesShowDisabled.Enabled := False;
    rbFavoritesShowEnabled.Enabled := False;
  end;
end;

procedure TFavoriteTask.SyncFinishChecking;
begin
  with MainForm do
  begin
    btCancelFavoritesCheck.Visible := False;
    btFavoritesCheckNewChapter.Width := btFavoritesCheckNewChapter.Width +
      btCancelFavoritesCheck.Width + 6;
    btFavoritesCheckNewChapter.Caption := RS_BtnCheckFavorites;
    rbFavoritesShowAll.Enabled := True;
    rbFavoritesShowDisabled.Enabled := True;
    rbFavoritesShowEnabled.Enabled := True;
    vtFavorites.Repaint;
    if OptionAutoCheckFavInterval and (not tmCheckFavorites.Enabled) then
      tmCheckFavorites.Enabled := True;
  end;
end;

procedure TFavoriteTask.SyncUpdateBtnCaption;
begin
  MainForm.btFavoritesCheckNewChapter.Caption := FBtnCaption;
end;

procedure TFavoriteTask.Execute;
var
  i: Integer;
begin
  FManager.isRunning := True;
  Synchronize(SyncStartChecking);

  // create the first thread
  TFavoriteThread.Create(Self);

  // wait for FThreads to finish
  while FThreads.Count > 0 do
    Sleep(HeartBeatRate);

  // reset all status
  FManager.Lock;
  try
    for i := 0 to FManager.Items.Count - 1 do
      with FManager.Items[i] do
      begin
        if Status=STATUS_CHECKED then
          DBUpdateDateLastChecked;
        if Status<>STATUS_IDLE then
          Status := STATUS_IDLE;
      end;
  finally
    FManager.UnLock;
  end;

  if (not Terminated) and (not isDlgCounter) then
    Synchronize(FManager.ShowResult)
  else
  // free unused unit
  begin
    EnterCriticalsection(FManager.FGuardian);
    try
      for i := 0 to FManager.Items.Count - 1 do
        with FManager.Items[i] do
        begin
          if Assigned(NewMangaInfo) then
            FreeAndNil(NewMangaInfo);
          if Assigned(NewMangaInfoChaptersPos) then
            FreeAndNil(NewMangaInfoChaptersPos);
        end;
    finally
      LeaveCriticalsection(FManager.FGuardian);
    end;
  end;

  EnterCriticalsection(FManager.FGuardian);
  try
    FManager.isRunning := False;
    FManager.TaskThread := nil;
  finally
    LeaveCriticalsection(FManager.FGuardian);
  end;

  // reset the ui
  if not isExiting then
  begin
    FManager.Backup;
    Synchronize(SyncFinishChecking);
  end;
end;

procedure TFavoriteTask.UpdateBtnCaption(Cap: String);
begin
  FBtnCaption := Cap;
  Synchronize(SyncUpdateBtnCaption);
end;

constructor TFavoriteTask.Create(const AManager: TFavoriteManager);
begin
  inherited Create(False);
  FManager := AManager;
  OnCustomTerminate := DoCustomTerminate;
  InitCriticalSection(FCS_Threads);
  InitCriticalSection(FCS_GetNext);
  FThreads := TFavoriteThreads.Create;
  FPendingCount := 1;

  FNeedRepaint := 0;
  FTimerRepaint := TTimer.Create(nil);
  FTimerRepaint.Interval := 500;
  FTimerRepaint.OnTimer := TimerRepaintOnTimer;
  FTimerRepaint.Enabled := True;
end;

destructor TFavoriteTask.Destroy;
begin
  FThreads.Free;
  FTimerRepaint.Free;
  DoneCriticalSection(FCS_GetNext);
  DoneCriticalSection(FCS_Threads);
  inherited Destroy;
end;

procedure TFavoriteTask.UpdateStatus;
begin
  InterlockedIncrement(FNeedRepaint);
end;

function TFavoriteTask.GetNext(var C: TfavoriteContainer): Boolean;
var
  i: TFavoriteContainer;
begin
  Result := False;
  C := nil;

  if FThreads.Count > OptionMaxFavoriteThreads then Exit;

  EnterCriticalSection(FCS_GetNext);
  try
    FPendingCount := 0;
    for i in FManager.Items do
    begin
      if Terminated then Break;
      if (i.Status = STATUS_CHECK) then
      begin
        if i.FavoriteInfo.Module = nil then
          i.Status := STATUS_IDLE
        else
        if C = nil then
          C := i
        else
          Inc(FPendingCount);
      end;
    end;

    if (FPendingCount > 0) and (FThreads.Count < OptionMaxFavoriteThreads) then
      TFavoriteThread.Create(Self);
  finally
    LeaveCriticalSection(FCS_GetNext);
  end;

  Result := C <> nil;
end;

procedure TFavoriteTask.AddThread(const T: TFavoriteThread);
begin
  EnterCriticalSection(FCS_Threads);
  try
    FThreads.Add(T);
  finally
    LeaveCriticalSection(FCS_Threads);
  end;
end;

procedure TFavoriteTask.RemoveThread(const T: TFavoriteThread);
begin
  EnterCriticalSection(FCS_Threads);
  try
    FThreads.Remove(T);
  finally
    LeaveCriticalSection(FCS_Threads);
  end;
end;

{ TFavoriteManager }

function TFavoriteManager.GetFavoritesCount: Integer;
begin
  Result := Items.Count;
end;

function TFavoriteManager.GetDisabledCount: Integer;
begin
  Result := Items.Count - FEnabledCount;
end;

function TFavoriteManager.GetFavorite(const Index: Integer): TFavoriteContainer;
begin
  Result := Items[Index];
end;

procedure TFavoriteManager.DBUpdateOrder;
var
  i: Integer;
begin
  if FUpdateOrderCount=0 then Exit;
  for i := 0 to Items.Count-1 do
  with Items[i] do begin
    if i<>FOrder then
    begin
      FOrder:=i;
      FFavoritesDB.tempSQL+='UPDATE "favorites" SET "order"='+PrepSQLValue(FOrder)+' WHERE "id"='+PrepSQLValue(Fid)+';';
      Inc(FFavoritesDB.tempSQLcount);
      if FFavoritesDB.tempSQLcount>=MAX_BIG_SQL_FLUSH_QUEUE then
        FFavoritesDB.FlushSQL(False);
    end;
  end;
  FUpdateOrderCount:=0;
end;

constructor TFavoriteManager.Create;
begin
  inherited Create;
  InitCriticalSection(FGuardian);
  isRunningRestore:=False;
  FUpdateOrderCount:=0;
  FEnabledCount:=0;
  ForceDirectories(USERDATA_FOLDER);
  isRunning := False;
  Items := TFavoriteContainers.Create;;
  FFavoritesDB := TFavoritesDB.Create(FAVORITESDB_FILE);
  FFavoritesDB.Open;
end;

destructor TFavoriteManager.Destroy;
var
  i: Integer;
begin
  if Items.Count > 0 then
  begin
    StopChekForNewChapter;
    for i := 0 to Items.Count - 1 do
      Items[i].Free;
  end;
  Items.Free;
  FFavoritesDB.Free;
  DoneCriticalsection(FGuardian);
  inherited Destroy;
end;

procedure TFavoriteManager.CheckForNewChapter(FavoriteIndex: Integer);
var
  i: Integer;
  toCheckCount: Integer;
begin
  if isDlgCounter then Exit;
  if Items.Count = 0 then Exit;
  try
    toCheckCount := 0;
    if FavoriteIndex > -1 then
    begin
      with Items[FavoriteIndex] do
        if Assigned(FavoriteInfo.Module) and FEnabled and (Status = STATUS_IDLE) then
        begin
          Status := STATUS_CHECK;
          Inc(toCheckCount);
          if Assigned(TaskThread) then
            InterLockedIncrement(TaskThread.FPendingCount);
        end;
    end
    else
    if isRunning then
    begin
      if not isAuto then
        MessageDlg('', RS_DlgFavoritesCheckIsRunning, mtInformation, [mbOK], 0);
    end
    else
    begin
      EnterCriticalsection(FGuardian);
      try
        for i := 0 to Items.Count - 1 do
          with Items[i] do
            if Assigned(FavoriteInfo.Module) and FEnabled and (Status = STATUS_IDLE) and (Trim(FavoriteInfo.Link) <> '') then
            begin
              Status := STATUS_CHECK;
              Inc(toCheckCount);
            end;
      finally
        LeaveCriticalsection(FGuardian);
      end;
    end;
    if (toCheckCount > 0) and (TaskThread = nil) then
      TaskThread := TFavoriteTask.Create(Self);
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
end;

procedure TFavoriteManager.StopChekForNewChapter(WaitFor: Boolean; FavoriteIndex: Integer);
begin
  if not isRunning then Exit;
  if FavoriteIndex > -1 then
  begin
    with Items[FavoriteIndex] do begin
      if Thread <> nil then
      begin
        Thread.Terminate;
        if WaitFor then
          Thread.WaitFor;
      end;
      if Status <> STATUS_IDLE then
        Status := STATUS_IDLE;
    end;
  end
  else
  if Assigned(TaskThread) then
  begin
    TaskThread.Terminate;
    if WaitFor then
      TaskThread.WaitFor;
  end;
end;

procedure TFavoriteManager.ShowResult;
var
  i, j,
  numOfNewChapters,
  numOfMangaNewChapters,
  numOfCompleted: Integer;
  LNCResult: TNewChapterResult = ncrCancel;
  newChapterListStr: String = '';
  removeListStr: String = '';
begin
  if isDlgCounter then Exit;
  if (Self.DLManager = nil) and Assigned(DLManager) then
    Self.DLManager := DLManager;
  if Self.DLManager = nil then Exit;

  Self.Sort(Self.FSortColumn);
  Lock;
  try
    numOfNewChapters := 0;
    numOfMangaNewChapters := 0;
    numOfCompleted := 0;

    try
      // check for all favorites
      for i := 0 to Items.Count - 1 do
        with Items[i] do
        begin
          if Assigned(NewMangaInfo) then
          begin
            // new chapters add to notification
            if NewMangaInfoChaptersPos.Count > 0 then
            begin
              newChapterListStr += LineEnding + '- ' + Format(
                RS_FavoriteHasNewChapter, [FavoriteInfo.Title, FavoriteInfo.Website,
                NewMangaInfoChaptersPos.Count]);
              Inc(numOfMangaNewChapters);
              Inc(numOfNewChapters, NewMangaInfoChaptersPos.Count);
            end
            else
            // completed series add to notification
            if OptionAutoCheckFavRemoveCompletedManga and
              (NewMangaInfo.Status = MangaInfo_StatusCompleted) then
            begin
              removeListStr += LineEnding + Format('- %s <%s>',
                [FavoriteInfo.Title, FavoriteInfo.Website]);
              Inc(numOfCompleted);
            end;
          end;
        end;

      // if there is completed mangas, show dialog
      if numOfCompleted > 0 then
      begin
        with TNewChapter.Create(MainForm) do
          try
            Caption := RS_DlgCompletedMangaCaption;
            lbNotification.Caption := Format(RS_LblMangaWillBeRemoved, [numOfCompleted]);
            mmMemo.Lines.Text := Trim(removeListStr);
            btDownload.Caption := RS_BtnRemove;
            btCancel.Caption := RS_BtnCancel;
            btDownload.Show;
            btCancel.Show;
            btQueue.Hide;
            ShowModal;
            LNCResult := FormResult;
          finally
            Free;
          end;

        //delete complete FFavorites
        if LNCResult = ncrDownload then
        begin
          i := 0;
          while i < Items.Count do
            with Items[i] do
            begin
              if Assigned(NewMangaInfo) and
                (NewMangaInfoChaptersPos.Count = 0) and
                (NewMangaInfo.Status = MangaInfo_StatusCompleted) then
                Delete(i)
              else
                Inc(i);
            end;
        end;
      end;

      // if there is new chapters
      if numOfNewChapters > 0 then
      begin
        if OptionAutoCheckFavDownload then
          LNCResult := ncrDownload
        else
          with TNewChapter.Create(MainForm) do
            try
              Caption := RS_DlgNewChapterCaption;
              lbNotification.Caption :=
                Format(RS_LblNewChapterFound, [numOfNewChapters, numOfMangaNewChapters]);
              mmMemo.Lines.Text := Trim(newChapterListStr);
              btDownload.Caption := RS_BtnDownload;
              btQueue.Caption := RS_BtnAddToQueue;
              btCancel.Caption := RS_BtnCancel;
              btDownload.Show;
              btQueue.Show;
              btCancel.Show;
              ShowModal;
              LNCResult := FormResult;
            finally
              Free;
            end;

        // generate download task
        if LNCResult <> ncrCancel then
        begin
          while DLManager.isRunningBackup do
            Sleep(100);

          DLManager.Lock;
          try
            for i := 0 to Items.Count - 1 do
            with Items[i] do
              if Assigned(NewMangaInfo) and
                (NewMangaInfoChaptersPos.Count > 0) then
              begin
                with DLManager.AddTask do
                begin
                  Manager := DLManager;
                  CurrentDownloadChapterPtr := 0;
                  DownloadInfo.Module := FavoriteInfo.Module;
                  DownloadInfo.Link := FavoriteInfo.Link;
                  DownloadInfo.Title := FavoriteInfo.Title;
                  DownloadInfo.SaveTo := FavoriteInfo.SaveTo;
                  DownloadInfo.DateAdded := Now;
                  DownloadInfo.DateLastDownloaded := Now;

                  for j := 0 to NewMangaInfoChaptersPos.Count - 1 do
                  begin
                    ChapterLinks.Add(NewMangaInfo.ChapterLinks[NewMangaInfoChaptersPos[j]]);
                    ChapterNames.Add(CustomRename(
                      OptionChapterCustomRename,
                      FavoriteInfo.Website,
                      FavoriteInfo.Title,
                      NewMangaInfo.Authors,
                      NewMangaInfo.Artists,
                      NewMangaInfo.ChapterNames[NewMangaInfoChaptersPos[j]],
                      Format('%.4d', [NewMangaInfoChaptersPos[j] + 1]),
                      OptionChangeUnicodeCharacter,
                      OptionChangeUnicodeCharacterStr));
                  end;

                  if LNCResult = ncrDownload then
                  begin
                    DownloadInfo.Status := Format('[%d/%d] %s',[0,ChapterLinks.Count,RS_Waiting]);
                    Status := STATUS_WAIT;
                  end
                  else
                  begin
                    DownloadInfo.Status := Format('[%d/%d] %s',[0,ChapterLinks.Count,RS_Stopped]);
                    Status := STATUS_STOP;
                  end;
                  DBInsert;
                  // add to downloaded chapter list
                  FavoriteInfo.downloadedChapterList := MergeCaseInsensitive([FavoriteInfo.DownloadedChapterList, chapterLinks.Text]);
                  // add to downloaded chapter list in downloadmanager
                  DLManager.DownloadedChapters.Chapters[FavoriteInfo.ModuleID, FavoriteInfo.Link] := chapterLinks.Text;
                end;
                DBUpdateLastUpdated;
                // free unused objects
                FreeAndNil(NewMangaInfo);
                FreeAndNil(NewMangaInfoChaptersPos);
              end;
          finally
            DLManager.UnLock;
          end;

          if LNCResult in [ncrDownload, ncrQueue] then
          begin
            if OptionSortDownloadsOnNewTasks then
              DLManager.Sort(DLManager.SortColumn);
            if LNCResult = ncrDownload then
              DLManager.CheckAndActiveTask;
            if OptionShowDownloadsTabOnNewTasks then
              MainForm.pcMain.ActivePage := MainForm.tsDownload;
          end;

          if Assigned(OnUpdateDownload) then
            OnUpdateDownload;
          if Assigned(OnUpdateFavorite) then
            OnUpdateFavorite;
        end;
      end;

    except
      on E: Exception do
        ExceptionHandle(Self, E);
    end;

    // check again for unused objects and free them
    for i := 0 to Items.Count - 1 do
      with Items[i] do
        if Assigned(NewMangaInfo) then
        begin
          FreeAndNil(NewMangaInfo);
          FreeAndNil(NewMangaInfoChaptersPos);
        end;
  finally
    UnLock;
  end;
end;

function TFavoriteManager.LocateManga(const ATitle, AWebsite: String): TFavoriteContainer;
var
  i: Integer;
begin
  Result := nil;
  if Items.Count <> 0 then
    for i := 0 to Items.Count - 1 do
      with Items[i].FavoriteInfo do
        if SameText(ATitle, Title) and SameText(AWebsite, ModuleID) then
          Exit(Items[i]);
end;

function TFavoriteManager.IsMangaExist(const ATitle, AWebsite: String): Boolean;
begin
  Result := LocateManga(ATitle, AWebsite) <> nil;
end;

function TFavoriteManager.LocateMangaByLink(const AModuleID, ALink: String): TFavoriteContainer;
var
  i: Integer;
begin
  Result := nil;
  if Items.Count <> 0 then
    for i := 0 to Items.Count - 1 do
      with Items[i].FavoriteInfo do
        if SameText(AModuleID, ModuleID) and SameText(ALink, Link) then
          Exit(Items[i]);
end;

function TFavoriteManager.IsMangaExistByLink(const AModuleID, ALink: String): Boolean;
begin
  Result := LocateMangaByLink(AModuleID, ALink) <> nil;
end;

procedure TFavoriteManager.Add(const AModule: Pointer; const ATitle, AStatus, ACurrentChapter,
  ADownloadedChapterList, ASaveTo, ALink: String; AEnabled: Boolean);
var
  F: TFavoriteContainer;
begin
  if AModule = nil then Exit;
  Lock;
  try
    if IsMangaExist(ATitle, TModuleContainer(AModule).ID) then Exit;
    F:=TFavoriteContainer.Create(Self);
    F.Enabled := AEnabled;
    F.FOrder:=Items.Add(F);
    with F.FavoriteInfo do begin
      Module := AModule;
      Title := ATitle;
      Status := AStatus;
      CurrentChapter := ACurrentChapter;
      SaveTo := ASaveTo;
      Link := ALink;
      DownloadedChapterList := ADownloadedChapterList;
      DateAdded := Now;
      DateLastChecked := Now;
      DateLastUpdated := Now;
    end;
    F.Status:=STATUS_IDLE;
    F.DBInsert;
  finally
    UnLock;
  end;
  if not isRunning then
    Sort(SortColumn);
end;

procedure TFavoriteManager.Replace(const OldId: String; const AModule: Pointer; const ATitle, AStatus, ACurrentChapter,
  ADownloadedChapterList, ASaveTo, ALink: String);
var
  F: TFavoriteContainer;
begin
  if AModule = nil then Exit;
  Lock;
  try
    F:=TFavoriteContainer.Create(Self);
    F.FOrder:=Items.Add(F);
    with F.FavoriteInfo do begin
      Module := AModule;
      Title := ATitle;
      Status := AStatus;
      CurrentChapter := ACurrentChapter;
      SaveTo := ASaveTo;
      Link := ALink;
      DownloadedChapterList := ADownloadedChapterList;
      DateAdded := Now;
      DateLastChecked := Now;
      DateLastUpdated := Now;
    end;
    F.Status:=STATUS_IDLE;
    F.DBReplace(OldId);
  finally
    UnLock;
  end;
  if not isRunning then
    Sort(SortColumn);
end;

procedure TFavoriteManager.Delete(const Pos: Integer);
begin
  if Items[Pos].FEnabled then
  begin
    Dec(FEnabledCount);
  end;
  FFavoritesDB.Delete(Items[Pos].Fid);
  Items[Pos].Free;
  Items.Delete(Pos);
  UpdateOrder;
end;

procedure TFavoriteManager.Remove(const T: TFavoriteContainer);
begin
  if T.FEnabled then
  begin
    Dec(FEnabledCount);
  end;
  FFavoritesDB.Delete(T.Fid);
  T.Free;
  Items.Remove(T);
  UpdateOrder;
end;

procedure TFavoriteManager.Restore;
var
  F: TFavoriteContainer;
begin
  if not FFavoritesDB.Connection.Connected then Exit;
  if not FFavoritesDB.OpenTable(False) then Exit;
  try
    if FFavoritesDB.Table.RecordCount = 0 then Exit;
    Lock;
    isRunningRestore:=True;
    try
      //FFavoritesDB.Table.Last; //load all to memory
      FFavoritesDB.Table.First;
      while not FFavoritesDB.Table.EOF do
      begin
        F := TFavoriteContainer.Create(Self);
        F.FOrder:=Items.Add(F);
        with F.FavoriteInfo, FFavoritesDB.Table do
        begin
          F.Fid                 := Fields[f_id].AsString;
          F.Status              := STATUS_IDLE;
          F.Enabled             := Fields[f_enabled].AsBoolean;
          ModuleID              := Fields[f_moduleid].AsString;
          Link                  := Fields[f_link].AsString;
          Title                 := Fields[f_title].AsString;
          Status                := Fields[f_status].AsString;
          CurrentChapter        := Fields[f_currentchapter].AsString;
          DownloadedChapterList := Fields[f_downloadedchapterlist].AsString;
          SaveTo                := Fields[f_saveto].AsString;
          DateAdded             := Fields[f_dateadded].AsDateTime;
          DateLastChecked       := Fields[f_datelastchecked].AsDateTime;
          DateLastUpdated       := Fields[f_datelastupdated].AsDateTime;
        end;
        FFavoritesDB.Table.Next;
      end;
    finally
      UnLock;
    end;
    isRunningRestore:=False;
  finally
    FFavoritesDB.CloseTable;
  end;
end;

procedure TFavoriteManager.Backup;
begin
  Lock;
  try
    DBUpdateOrder;
    FFavoritesDB.Commit(False);
  finally
    UnLock;
  end;
end;

procedure TFavoriteManager.AddToDownloadedChaptersList(const AWebsite,
  ALink: String; const AValue: TStrings);
var
  i: Integer;
begin
  if (Items.Count = 0) or (AWebsite = '') or (ALink = '') or (AValue.Count = 0) then Exit;
  try
    EnterCriticalsection(FGuardian);
    for i := 0 to Items.Count - 1 do
      with Items[i].FavoriteInfo do
        if SameText(AWebsite, ModuleID) and SameText(ALink, Link) then
        begin
          DownloadedChapterList := MergeCaseInsensitive([DownloadedChapterList, AValue.Text]);
          Break;
        end;
  finally
    LeaveCriticalsection(FGuardian);
  end;
end;

function CompareFavoriteContainer(const Item1, Item2: TFavoriteContainer): Integer;

  function GetStr(ARow: TFavoriteContainer): String;
  begin
    with ARow.FavoriteInfo do
      case ARow.FManager.SortColumn of
        1: Result := Title;
        2: Result := currentChapter;
        3: Result := Website;
        4: Result := SaveTo;
        else
          Result := '';
      end;
  end;

  function GetDateTime(ARow: TFavoriteContainer): TDateTime;
  begin
    with ARow.FavoriteInfo do
      case ARow.FManager.SortColumn of
        5: Result := DateAdded;
        6: Result := DateLastChecked;
        7: Result := DateLastUpdated;
        else
          Result := Now;
      end;
  end;

begin
  if (Item1.FManager.SortColumn >= 5) and (Item1.FManager.SortColumn <= 7) then
  begin
    if Item1.FManager.SortDirection then
      Result := CompareDateTime(GetDateTime(Item2), GetDateTime(Item1))
    else
      Result := CompareDateTime(GetDateTime(Item1), GetDateTime(Item2));
  end
  else
  begin
    if Item1.FManager.SortDirection then
      Result := NaturalCompareStr(GetStr(Item2), GetStr(Item1))
    else
      Result := NaturalCompareStr(GetStr(Item1), GetStr(Item2));
  end;
  if Result = 0 then Result := NaturalCompareStr(Item1.FavoriteInfo.Title, Item2.FavoriteInfo.Title);
end;

procedure TFavoriteManager.Sort(const AColumn: Integer);
begin
  if Items.Count < 2 then Exit;
  EnterCriticalSection(FGuardian);
  try
    SortColumn := AColumn;
    Items.Sort(CompareFavoriteContainer);
    UpdateOrder;
  finally
    LeaveCriticalSection(FGuardian);
  end;
end;

procedure TFavoriteManager.Lock;
begin
  EnterCriticalsection(FGuardian);
  EnterCriticalSection(FFavoritesDB.Guardian);
  FFavoritesDB.BeginUpdate;
end;

procedure TFavoriteManager.UnLock;
begin
  FFavoritesDB.EndUpdate;
  LeaveCriticalSection(FFavoritesDB.Guardian);
  LeaveCriticalsection(FGuardian);
end;

procedure TFavoriteManager.UpdateOrder;
begin
  Inc(FUpdateOrderCount);
end;

end.
