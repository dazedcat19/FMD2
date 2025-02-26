{
        File: uUpdateThread.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uUpdateThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, fgl, uData, LazFileUtils, uBaseUnit, uMisc,
  WebsiteModules, DBDataProcess, FMDOptions, httpsendthread, uCustomControls,
  BaseThread, MultiLog, ExtCtrls, Forms, Controls, Buttons, Graphics;

type
  TUpdateListManagerThread = class;

  { TUpdateListThread }

  TUpdateListThread = class(TBaseThread)
  private
    FOwner: TUpdateListManagerThread;
    FWorkPtr: Integer;
    FMangaInfo: TMangaInformation;
  protected
    procedure Execute; override;
    procedure GetDirectoryPageNumber;
    procedure GetNamesAndLinks;
    procedure GetInfo;
  public
    constructor Create(const AOwner: TUpdateListManagerThread);
    destructor Destroy; override;
  end;

const
  DefaultCommitCount = 32;

type
  TUpdateListThreads = specialize TFPGList<TUpdateListThread>;

  { TUpdateListManagerThread }

  TUpdateListManagerThread = class(TBaseThread)
  private
    FCommitCount: Integer;
    FThreadAborted,
    FThreadEndNormally,
    FIsPreListAvailable: Boolean;

    FStatusBar: TPanel;
    FButtonCancel: TSpeedButton;

    FControlMargin,
    FProgressBarHeight: Integer;
    FResized: Boolean;
    FProgressBarRect,
    FProgressBarPercentsRect,
    FStatusTextRect: TRect;
    FStatusText: String;

    FTimerRepaint: TTimer;
    FNeedRepaint: Boolean;

    FGuardian: TRTLCriticalSection;
    FWorkPtr: Integer;
    FTotalPtr: Integer;
    FCurrentGetInfoLimit: Integer;
    FCurrentCS: TCheckStyleType;
  protected
    procedure SyncCreate;
    procedure SyncDestroy;
    procedure TimerRepaintTimer(Sender: TObject);
    procedure StatusBarPaint(Sender: TObject);
    procedure StatusBarRezise(Sender: TObject);
    procedure StatusBarShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure ButtonCancelClick(Sender: TObject);
    procedure UpdateStatusText(AStatusText: String);
    procedure SetCurrentDirectoryPageNumber(AValue: Integer);
    procedure MainThreadEndGetting;
    procedure MainThreadRemoveFilter;
    procedure ExtractFile;
    procedure RefreshList;
    procedure DlgReport;
    procedure CheckOut(const alimit: Integer; const acs: TCheckStyleType);
    procedure Execute; override;
    procedure TerminateThreads;
    procedure WaitForThreads; inline;
    procedure TerminateCurrent(Sender: TObject);
    procedure ThreadAdd(const T: TUpdateListThread);
    procedure ThreadRemove(const T: TUpdateListThread);
  protected
    procedure GetCurrentLimit;
    function GetNext(const T: TUpdateListThread): Boolean;
  public
    procedure Lock; inline;
    procedure Unlock; inline;
    procedure UpdateStatusFormatted(AStatusText: String);
  public
    ThreadsGuardian: TRTLCriticalSection;
    isFinishSearchingForNewManga, isDoneUpdateNecessary: Boolean;
    mainDataProcess: TDBDataProcess;
    tempDataProcess: TDBDataProcess;
    websites: TStringList;
    website, twebsite, twebsitetemp: String;
    module: TModuleContainer;
    directoryCount,
    workPtr,
    websitePtr,
    numberOfThreads: Integer;
    Threads: TUpdateListThreads;
    InvertedList: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure CheckCommit(const CommitCount: Integer = DefaultCommitCount);
    property CurrentDirectoryPageNumber: Integer read FCurrentGetInfoLimit write SetCurrentDirectoryPageNumber;
  end;
  
resourcestring
  RS_DlgUpdateListCheckIsRunning = 'Update list(s) is running!';
  RS_UpdatingList = 'Updating list';
  RS_GettingDirectory = 'Getting directory';
  RS_LookingForNewTitle = 'Looking for new title(s)';
  RS_LookingForNewTitleFromAnotherDirectory = 'Looking for new title(s) from another directory';
  RS_GettingInfo = 'Getting info';
  RS_GettingListFor = 'Getting list for';
  RS_Preparing = 'Preparing';
  RS_IndexingNewTitle = 'Indexing new title(s)';
  RS_RemovingDuplicateFromNewTitle = 'Removing duplicate from new title(s)';
  RS_RemovingDuplicateFromCurrentData = 'Removing duplicate from current data';
  RS_RemovingDuplicateFromLocalData = 'Removing duplicate from local data';
  RS_SynchronizingData = 'Synchronizing data';
  RS_SavingData = 'Saving data';
  RS_DlgHasNewManga = '%s has %d new manga(s)';

implementation

uses
  frmMain, FMDVars, LuaWebsiteModuleHandler, Dialogs;

const
  CL_ProgressBarBaseLine = clBtnFace;
  CL_ProgressBarBase     = clWindow;
  CL_ProgressBarLine     = $25b006;
  CL_ProgressBar         = $42d932;

{ TUpdateListThread }

constructor TUpdateListThread.Create(const AOwner: TUpdateListManagerThread);
begin
  inherited Create(False);
  FOwner := AOwner;
  FOwner.ThreadAdd(Self);
  if FOwner.FCurrentCS = CS_INFO then
  begin
    FMangaInfo := TMangaInformation.Create(Self);
    FMangaInfo.isGetByUpdater := True;
    FMangaInfo.Module := FOwner.module;
  end;
  FWorkPtr := -1;
end;

destructor TUpdateListThread.Destroy;
begin
  FOwner.ThreadRemove(Self);
  if FMangaInfo <> nil then
    FMangaInfo.Free;
  inherited Destroy;
end;

procedure TUpdateListThread.Execute;
begin
  case FOwner.FCurrentCS of
    CS_DIRECTORY_COUNT : GetDirectoryPageNumber;
    CS_DIRECTORY_PAGE  : GetNamesAndLinks;
    CS_INFO            : GetInfo;
  end;
end;

procedure TUpdateListThread.GetDirectoryPageNumber;
var
  http: THTTPSendThread;
  {R: Byte;}
begin
  http := FOwner.module.CreateHTTP(Self);
  try
    while FOwner.GetNext(Self) do
    begin
      try
        {R := INFORMATION_NOT_FOUND;}
        FOwner.module.TotalDirectoryPage[FWorkPtr] := 1;

        //load pagenumber_config if available
        if Assigned(FOwner.module.OnGetDirectoryPageNumber) then
        begin
          // TODO: GetDirectoryPageNumber.Result(Byte), do something with the result
          {R := }FOwner.module.OnGetDirectoryPageNumber(FOwner, http, FOwner.module.TotalDirectoryPage[FWorkPtr], FWorkPtr, FOwner.module);
          if FOwner.module.TotalDirectoryPage[FWorkPtr] < 1 then
            FOwner.module.TotalDirectoryPage[FWorkPtr] := 1;
        end;

        http.Reset;
      except
        on E: Exception do
          SendLogException(Self.ClassName+'.GetDirectoryPage()>ERROR ', E);
      end;
    end;
  finally
    http.Free;
  end;
end;

procedure TUpdateListThread.GetNamesAndLinks;
var
  http: THTTPSendThread;
  names, links: TStringList;
  R: Byte;
  i: Integer;
begin
  http := FOwner.module.CreateHTTP(Self);
  names := TStringList.Create;
  links := TStringList.Create;
  try
    while FOwner.GetNext(Self) do
    begin
      try
        R := INFORMATION_NOT_FOUND;
        if FOwner.InvertedList then
          FWorkPtr := FOwner.module.TotalDirectoryPage[FOwner.module.CurrentDirectoryIndex] - FWorkPtr -1;

        if Assigned(FOwner.module.OnGetNameAndLink) then
          R := FOwner.module.OnGetNameAndLink(FOwner, http, names, links, IntToStr(FWorkPtr), FOwner.module);

        if (R <> INFORMATION_NOT_FOUND) and (links.Count <> 0) then
        begin
          //remove host from URLs
          RemoveHostFromURLsPair(links, names);

          //if website has sorted list by latest added
          //we will stop at first found against current db
          FOwner.tempDataProcess.Lock;
          try
            if FOwner.FIsPreListAvailable then
            begin
              for i:=0 to links.Count-1 do
              begin
                if FOwner.mainDataProcess.AddData(names[i],'',links[i],'','','','','',0,0) then
                  FOwner.tempDataProcess.AddData(names[i],'',links[i],'','','','','',0,0)
                else if (FOwner.isFinishSearchingForNewManga=False) and FOwner.module.SortedList and (not FOwner.InvertedList) then
                  FOwner.isFinishSearchingForNewManga:=True;
              end;
              FOwner.mainDataProcess.Rollback;
            end
            else
            begin
              for i:=0 to links.Count-1 do
                FOwner.tempDataProcess.AddData(names[i],'',links[i],'','','','','',0,0);
            end;
            FOwner.tempDataProcess.Commit;
          finally
            FOwner.tempDataProcess.Unlock;
          end;
        end;

        http.Reset;
        names.Clear;
        links.Clear;
      except
        on E: Exception do
          SendLogException(Self.ClassName+'.GetNamesAndLinks()>ERROR ', E);
      end;
    end;
  finally
    http.Free;
    names.Free;
    links.Free;
  end;
end;

procedure TUpdateListThread.GetInfo;
begin
  while FOwner.GetNext(Self) do
  begin
    try
      if (FMangaInfo.MangaInfo.Link <> '') and
        (FMangaInfo.GetInfoFromURL(FMangaInfo.MangaInfo.Link) <> INFORMATION_NOT_FOUND) and
        (not Terminated) and (FMangaInfo.MangaInfo.Status <> '-1') then
      begin
        // status = '-1' mean it's not exist and shouldn't be saved to database
        FOwner.mainDataProcess.Lock;
        try
          FMangaInfo.AddInfoToData(FMangaInfo.MangaInfo.Link,FMangaInfo.MangaInfo.Link,FOwner.mainDataProcess);
          //FOwner.CheckCommit(FOwner.numberOfThreads);
          // todo: test if CheckCommit(32) is sufficient.
          FOwner.CheckCommit;
        finally
          FOwner.mainDataProcess.Unlock;
        end;
      end;

      FMangaInfo.HTTP.Reset;
      FMangaInfo.MangaInfo.Clear;
    except
      on E: Exception do
        SendLogException(Self.ClassName+'.GetNamesAndLinks()>ERROR ', E);
    end;
  end;
end;

{ TUpdateListManagerThread }

procedure TUpdateListManagerThread.MainThreadEndGetting;
begin
  isUpdating:=False;
  if isPendingExitCounter then
    MainForm.DoExitWaitCounter;
end;

procedure TUpdateListManagerThread.MainThreadRemoveFilter;
begin
  MainForm.btRemoveFilterClick(MainForm.btRemoveFilter);
end;

procedure TUpdateListManagerThread.ExtractFile;
var
  Sza, datapath, filepath: String;
begin
  Sza := FMD_DIRECTORY + ZIP_EXE;
  if not FileExists(Sza) then Exit;

  datapath := DATA_FOLDER;
  filepath := datapath + website;
  if FileExists(filepath + '.7z') then
     filepath += '.7z'
  else
  if FileExists(filepath + '.zip') then
    filepath += '.zip';

  if FileExists(filepath) then
  begin
    if FileExists(datapath + website + DBDATA_EXT) then
      DeleteFile(datapath + website + DBDATA_EXT);
    if FileExists(datapath + website + DATA_EXT) then
      DeleteFile(datapath + website + DATA_EXT);
    RunExternalProcess(Sza, ['x', filepath, '-o' +
      AnsiQuotedStr(datapath, '"'), '-aoa'], False, True);
    DeleteFile(filepath);
  end
end;

constructor TUpdateListManagerThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  OnCustomTerminate := @TerminateCurrent;

  InitCriticalSection(FGuardian);
  InitCriticalSection(ThreadsGuardian);

  websites := TStringList.Create;
  mainDataProcess := TDBDataProcess.Create;
  tempDataProcess := TDBDataProcess.Create;
  Threads := TUpdateListThreads.Create;
  FThreadEndNormally:=False;
  FThreadAborted:=False;
  FIsPreListAvailable:=False;
  FCurrentGetInfoLimit := 1;
  InvertedList := False;

  Synchronize(@SyncCreate);
end;

destructor TUpdateListManagerThread.Destroy;
begin
  TerminateThreads;
  WaitForThreads;
  Synchronize(@SyncDestroy);
  if FThreadAborted then Logger.SendWarning(Self.ClassName+', thread aborted by user?');
  if not FThreadEndNormally then Logger.SendWarning(Self.ClassName+', thread doesn''t end normally, ended by user?');
  websites.Free;
  mainDataProcess.Close;
  tempDataProcess.Close;
  DeleteDBDataProcess(twebsite);
  DeleteDBDataProcess(twebsitetemp);
  mainDataProcess.Free;
  tempDataProcess.Free;
  Threads.Free;
  isUpdating := False;
  DoneCriticalsection(ThreadsGuardian);
  DoneCriticalsection(FGuardian);
  inherited Destroy;
end;

procedure TUpdateListManagerThread.CheckCommit(const CommitCount: Integer);
begin
  Inc(FCommitCount);
  if FCommitCount >= CommitCount then
  begin
    FCommitCount := 0;
    if Assigned(mainDataProcess) then
      mainDataProcess.Commit;
  end;
end;

procedure TUpdateListManagerThread.RefreshList;
begin
  try
    with MainForm do
    begin
      if TModuleContainer(cbSelectManga.Items.Objects[cbSelectManga.ItemIndex]).ID = website then
      begin
        vtMangaList.Clear;
        if dataProcess = nil then
          dataProcess := TDBDataProcess.Create
        else
          dataProcess.Close;
        OverwriteDBDataProcess(website, twebsite);
        OpenDataDB(website);
      end
      else
      begin
        if dataProcess.WebsiteLoaded(website) then
          dataProcess.RemoveFilter;
        OverwriteDBDataProcess(website, twebsite);
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TUpdateListManagerThread.DlgReport;
begin
  CenteredMessageDlg(MainForm, Format(RS_DlgHasNewManga, [module.Name, tempDataProcess.RecordCount]),
    mtInformation, [mbYes], 0);
end;

procedure TUpdateListManagerThread.CheckOut(const alimit: Integer; const acs: TCheckStyleType);
begin
  FCurrentGetInfoLimit := alimit;
  FCurrentCS := acs;

  TUpdateListThread.Create(Self);

  WaitForThreads;
end;

procedure TUpdateListManagerThread.SetCurrentDirectoryPageNumber(AValue: Integer);
begin
  if AValue < FCurrentGetInfoLimit then Exit;
  Lock;
  try
    FCurrentGetInfoLimit := AValue;
  finally
    Unlock;
  end;
end;

procedure TUpdateListManagerThread.SyncCreate;
var
  txtHeight: Integer;
begin
  FControlMargin := FormMain.ScaleFontTo96(2);
  FProgressBarHeight := FormMain.ScaleFontTo96(7);
  FStatusBar := TPanel.Create(nil);
  with FStatusBar do begin
    Parent := MainForm;
    DoubleBuffered := True;
    Align := alBottom;
    AutoSize := False;
    txtHeight := Canvas.GetTextHeight('A');
    Height := txtHeight + FProgressBarHeight + (FControlMargin * 6);
    Caption := '';
    Color := clBtnFace;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    BorderStyle := bsNone;
    BorderSpacing.Top := FControlMargin;
    OnPaint := @StatusBarPaint;
    OnResize := @StatusBarRezise;
    OnShowHint := @StatusBarShowHint;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Style := psSolid;
    FResized := True;
  end;

  FButtonCancel := TSpeedButton.Create(FStatusBar);
  with FButtonCancel do begin
    Parent := FStatusBar;
    Align := alNone;
    AutoSize := False;
    Flat := True;
    Anchors := [akTop, akRight, akBottom];
    AnchorSideTop.Control := FStatusBar;
    AnchorSideTop.Side := asrTop;
    BorderSpacing.Top := FControlMargin;
    AnchorSideRight.Control := FStatusBar;
    AnchorSideRight.Side := asrRight;
    BorderSpacing.Right := FControlMargin;
    AnchorSideBottom.Control := FStatusBar;
    AnchorSideBottom.Side := asrBottom;
    BorderSpacing.Bottom := FControlMargin;
    Width := Height;
    OnClick := @ButtonCancelClick;
    Images := FormMain.IconList;
    ImageIndex := 24;
  end;

  StatusBarRezise(FStatusBar);

  FTimerRepaint := TTimer.Create(FStatusBar);
  FTimerRepaint.Interval := 1000;
  FTimerRepaint.OnTimer := @TimerRepaintTimer;
  FTimerRepaint.Enabled := True;
  FNeedRepaint := True;
end;

procedure TUpdateListManagerThread.SyncDestroy;
begin
  FStatusBar.Free;
end;

procedure TUpdateListManagerThread.TimerRepaintTimer(Sender: TObject);
begin
  if FNeedRepaint then
  begin
    FNeedRepaint := False;
    FStatusBar.Repaint;
  end;
end;

procedure TUpdateListManagerThread.StatusBarPaint(Sender: TObject);
var
  txtHeight: integer;
  barPercents: Double;
begin
  with FStatusBar.Canvas do
  begin
    Pen.Color := clActiveBorder;
    Line(0,0,FStatusBar.ClientRect.Right,0);

    if FResized then
    begin
      FStatusTextRect := FStatusBar.ClientRect;
      FStatusTextRect.Right := FButtonCancel.Left;
      FStatusTextRect.Inflate(-(FControlMargin * 2), -(FControlMargin * 2));
      FProgressBarRect := FStatusTextRect;
      FProgressBarRect.Top := FProgressBarRect.Bottom - FProgressBarHeight;
      FStatusTextRect.Bottom := FProgressBarRect.Top - FControlMargin;
      FResized := False;
    end;

    Brush.Style := bsSolid;
    Pen.Style := psSolid;

    Pen.Color := CL_ProgressBarBaseLine;
    Brush.Color := CL_ProgressBarBase;
    Rectangle(FProgressBarRect);

    if FTotalPtr = 0 then
      FTotalPtr := 100;
    if FWorkPtr > FTotalPtr then
      FWorkPtr := FTotalPtr;
    barPercents := FWorkPtr / FTotalPtr;
    if barPercents > 0 then
    begin
      FProgressBarPercentsRect := FProgressBarRect;
      FProgressBarPercentsRect.Right :=
        Round((FProgressBarPercentsRect.Right - FProgressBarPercentsRect.Left) * barPercents) + FProgressBarPercentsRect.Left;

      Pen.Color   := CL_ProgressBarLine;
      Brush.Color := CL_ProgressBar;

      Frame(FProgressBarPercentsRect);
      FProgressBarPercentsRect.Inflate(-2, -2);
      GradientFill(FProgressBarPercentsRect, BlendColor(Brush.Color, CL_ProgressBarBase, 128), Brush.Color, gdHorizontal);
    end;
    Font.Color := clWindowText;
    Brush.Style := bsClear;
    txtHeight := GetTextHeight(FStatusText);
    TextRect(FStatusTextRect, FStatusTextRect.Left, FStatusTextRect.Top + ((FStatusTextRect.Bottom - FStatusTextRect.Top - txtHeight) div 2), FStatusText);
  end;
end;

procedure TUpdateListManagerThread.StatusBarRezise(Sender: TObject);
begin
  FResized := True;
end;

procedure TUpdateListManagerThread.StatusBarShowHint(Sender: TObject;
  HintInfo: PHintInfo);
begin
  HintInfo^.HintWindowClass := TCustomHintWindow;
  HintInfo^.HintStr := Trim(websites.Text);
end;

procedure TUpdateListManagerThread.ButtonCancelClick(Sender: TObject);
begin
  Self.Terminate;
end;

procedure TUpdateListManagerThread.UpdateStatusText(AStatusText: String);
begin
  if AStatusText <> FStatusText then
  begin
    FStatusText := AStatusText;
    FNeedRepaint := True;
  end;
end;

procedure TUpdateListManagerThread.UpdateStatusFormatted(AStatusText: String);
begin
  Lock;
  try
    FStatusText := RS_UpdatingList + Format(' [%d/%d] %s | [T:%d] [%d/%d] ',
      [websitePtr, websites.Count, module.Name, Threads.Count, workPtr, FCurrentGetInfoLimit]) + AStatusText;
    FNeedRepaint := True;
  finally
    Unlock;
  end;
end;

procedure TUpdateListManagerThread.Execute;
var
  j, k: Integer;
  cloghead: String;
begin
  if websites.Count = 0 then
    Exit;
  try
    websitePtr := 0;
    while websitePtr < websites.Count do
    begin
      FThreadAborted:=True;
      module := TModuleContainer(websites.Objects[websitePtr]);
      if Assigned(module) then
      begin
        website := module.ID;
        Inc(websitePtr);

        cloghead:=Self.ClassName+', '+Module.Name+': ';
        UpdateStatusText(RS_UpdatingList + Format(' [%d/%d] %s',
          [websitePtr, websites.Count, Module.Name]) + ' | ' + RS_Preparing + '...');

        twebsite:='__'+website;
        twebsitetemp:=twebsite+'_templist';
        try
          DeleteDBDataProcess(twebsite);
          DeleteDBDataProcess(twebsitetemp);
          if (dataProcess.Website = website) and
            (dataProcess.Connected) then
            dataProcess.Backup(twebsite)
          else
          begin
            if dataProcess.WebsiteLoaded(website) then
              Synchronize(@MainThreadRemoveFilter);
            CopyDBDataProcess(website, twebsite);
          end;

          if not mainDataProcess.Connect(twebsite) then
            mainDataProcess.CreateDatabase(twebsite);
          tempDataProcess.CreateDatabase(twebsitetemp);

          // get directory page count
          InvertedList := False;
          directoryCount := 0;
          workPtr := 0;
          if Assigned(module.OnAfterUpdateList) then
            module.OnAfterUpdateList(module);
          if Assigned(module.OnBeforeUpdateList) then
            module.OnBeforeUpdateList(module);
          if module.Settings.Enabled and (module.Settings.UpdateListDirectoryPageNumber > 0) then
          begin
            module.TotalDirectoryPage[FWorkPtr] := module.Settings.UpdateListDirectoryPageNumber;
            InvertedList := True;
          end
          else
            CheckOut(module.TotalDirectory, CS_DIRECTORY_COUNT);

          if Terminated then
          begin
            if Assigned(module.OnAfterUpdateList) then
              module.OnAfterUpdateList(module);
            Break;
          end;

          mainDataProcess.OpenTable('',True);
          FIsPreListAvailable:=mainDataProcess.RecordCount>0;
          mainDataProcess.CloseTable;

          // get names and links
          workPtr := 0;
          isFinishSearchingForNewManga := False;
          j := Low(module.TotalDirectoryPage);
          while j <= High(module.TotalDirectoryPage) do
          begin
            workPtr := 0;
            isFinishSearchingForNewManga := False;
            module.CurrentDirectoryIndex := j;
            CheckOut(module.TotalDirectoryPage[j], CS_DIRECTORY_PAGE);
            Inc(j);
            if Terminated then Break;
          end;

          if Assigned(module.OnBeforeUpdateList) then
            module.OnBeforeUpdateList(module);
          if Terminated then
            if not (OptionUpdateListNoMangaInfo and not(module.SortedList)) then
              Break;

          UpdateStatusText(RS_UpdatingList + Format(' [%d/%d] %s',
            [websitePtr, websites.Count, Module.Name]) + ' | ' + RS_IndexingNewTitle + '...');

          tempDataProcess.OpenTable('', True);
          // get manga info
          if tempDataProcess.RecordCount>0 then
          begin
            workPtr := 0;
            FCommitCount := 0;
            if not module.InformationAvailable or
              OptionUpdateListNoMangaInfo then
            begin
              Inc(workPtr);
              for k:=0 to tempDataProcess.RecordCount-1 do
              begin
                mainDataProcess.AddData(
                  tempDataProcess.Value[k,DATA_PARAM_TITLE],
                  '',
                  tempDataProcess.Value[k,DATA_PARAM_LINK],
                  '',
                  '',
                  '',
                  '',
                  '',
                  0,
                  Now
                  );
                CheckCommit(5000);
              end;
            end
            else
              CheckOut(tempDataProcess.RecordCount, CS_INFO);
            mainDataProcess.Commit;

            if (workPtr > 0) and (not (Terminated and module.SortedList)) then
            begin
              UpdateStatusText(RS_UpdatingList + Format(' [%d/%d] %s',
                [websitePtr, websites.Count, Module.Name]) + ' | ' + RS_SavingData + '...');
              mainDataProcess.Sort;
              mainDataProcess.Close;
              Synchronize(@RefreshList);
            end;
          end;
        except
          on E: Exception do
            SendLogException(cloghead + 'error occured!', E);
        end;

        tempDataProcess.Close;
        mainDataProcess.Close;
        DeleteDBDataProcess(twebsite);
        DeleteDBDataProcess(twebsitetemp);

        if Terminated then
          Break;
        websites[websitePtr - 1] := UTF8Encode(#$2714) + websites[websitePtr - 1];
        FThreadAborted:=False;
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
  FThreadEndNormally:=True;
  Synchronize(@MainThreadEndGetting);
end;

procedure TUpdateListManagerThread.TerminateThreads;
var
  t: TUpdateListThread;
begin
  EnterCriticalsection(ThreadsGuardian);
  try
    for t in Threads do
      t.Terminate;
  finally
    LeaveCriticalsection(ThreadsGuardian);
  end;
end;

procedure TUpdateListManagerThread.WaitForThreads;
begin
  while Threads.Count <> 0 do
    Sleep(2000); // this will wait for a loop of high number
end;

procedure TUpdateListManagerThread.TerminateCurrent(Sender: TObject);
begin
  TerminateThreads;
end;

procedure TUpdateListManagerThread.ThreadAdd(const T: TUpdateListThread);
begin
  EnterCriticalSection(ThreadsGuardian);
  try
    Threads.Add(T);
  finally
    LeaveCriticalSection(ThreadsGuardian);
  end;
end;

procedure TUpdateListManagerThread.ThreadRemove(const T: TUpdateListThread);
begin
  EnterCriticalSection(ThreadsGuardian);
  try
    Threads.Remove(T);
  finally
    LeaveCriticalSection(ThreadsGuardian);
  end;
end;

procedure TUpdateListManagerThread.GetCurrentLimit;
begin
  if FTotalPtr <> FCurrentGetInfoLimit then
    FTotalPtr := FCurrentGetInfoLimit;
  if module.Settings.Enabled and (module.Settings.UpdateListNumberOfThread > 0) then
    numberOfThreads := module.Settings.UpdateListNumberOfThread
  else
  if module.MaxThreadPerTaskLimit > 0 then
    numberOfThreads := module.MaxThreadPerTaskLimit
  else
    numberOfThreads := OptionMaxUpdateListThreads;
  if (module.ConnectionsQueue.MaxConnections > 0) and (numberOfThreads > module.ConnectionsQueue.MaxConnections) then
    numberOfThreads := module.ConnectionsQueue.MaxConnections;
  if numberOfThreads < 1 then
    numberOfThreads := 1;  //default
end;

function TUpdateListManagerThread.GetNext(const T: TUpdateListThread): Boolean;
var
  s: String;
begin
  Result := False;
  T.FWorkPtr := -1;
  if Terminated then Exit;
  // Finish searching for new series in sorted mode
  if (FCurrentCS = CS_DIRECTORY_PAGE) and (isFinishSearchingForNewManga) then Exit;

  if workPtr >= FCurrentGetInfoLimit then Exit;

  Lock;
  try
    GetCurrentLimit;
    if Threads.Count > numberOfThreads then Exit;

    T.FWorkPtr := workPtr;
    Result := True;
    InterlockedIncrement(workPtr);

    s := RS_UpdatingList + Format(' [%d/%d] %s | [T:%d] [%d/%d]',
      [websitePtr, websites.Count, module.Name, Threads.Count, workPtr, FCurrentGetInfoLimit]);

    case FCurrentCS of
      CS_DIRECTORY_COUNT:
        begin
          if FCurrentGetInfoLimit = 1 then
            s := RS_UpdatingList + Format(' [%d/%d] ', [websitePtr, websites.Count]) +
              module.Name + ' | ' + RS_GettingDirectory + '...'
          else
            s := s + ' | ' + RS_GettingDirectory + '...';
        end;
      CS_DIRECTORY_PAGE:
        begin
          s += ' | ' + RS_LookingForNewTitle +
            Format(' %d/%d', [module.CurrentDirectoryIndex + 1, module.TotalDirectory]) +
            '...';
        end;
      CS_INFO:
        begin
          tempDataProcess.Lock;
          try
            T.FMangaInfo.MangaInfo.Title := tempDataProcess.Value[T.FWorkPtr,DATA_PARAM_TITLE];
            T.FMangaInfo.MangaInfo.Link := tempDataProcess.Value[T.FWorkPtr,DATA_PARAM_LINK];
            s := Format('%s | %s "%s"', [s, RS_GettingInfo, T.FMangaInfo.MangaInfo.Title]);
          finally
            tempDataProcess.Unlock;
          end;
        end;
    end;
    UpdateStatusText(s);
    FWorkPtr := workPtr + 1;

    // spawn new worker thread
    if (Threads.Count < numberOfThreads) and (workPtr < FCurrentGetInfoLimit) then
      TUpdateListThread.Create(Self);
  finally
    Unlock;
  end;
end;

procedure TUpdateListManagerThread.Lock;
begin
  EnterCriticalSection(FGuardian);
end;

procedure TUpdateListManagerThread.Unlock;
begin
  LeaveCriticalSection(FGuardian);
end;

end.
