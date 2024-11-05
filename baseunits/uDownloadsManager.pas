{
        File: uDownloadsManager.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uDownloadsManager;

{$mode objfpc}{$H+}

interface

uses
  LazFileUtils, Classes, SysUtils, ExtCtrls, typinfo, fgl,
  blcksock, MultiLog, uBaseUnit, uPacker, uMisc, DownloadedChaptersDB, FMDOptions,
  httpsendthread, DownloadsDB, BaseThread, SQLiteData, dateutils, strutils;

type
  TDownloadStatusType = (
    STATUS_STOP,
    STATUS_WAIT,
    STATUS_PREPARE,
    STATUS_DOWNLOAD,
    STATUS_FINISH,
    STATUS_COMPRESS,
    STATUS_PROBLEM,
    STATUS_FAILED,
    STATUS_NONE        // devault value oncreate, don't use
    );
  TDownloadStatusTypes = set of TDownloadStatusType;

  TDownloadManager = class;
  TTaskContainer = class;
  TTaskThread = class;

  { TDownloadThread }

  TDownloadThread = class(TBaseThread)
  private
    // Get real image url
    function GetLinkPageFromURL(const URL: String): Boolean;

    // Download image
    function DownloadImage: Boolean;

    procedure SockOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
    procedure DoPageLink;
    procedure DoDownload;
    procedure DoSuccess;
  protected
    procedure Execute; override;
  public
    Task: TTaskThread;
    HTTP: THTTPSendThread;
    WorkId: Integer;
    constructor Create(const ATask: TTaskThread);
    destructor Destroy; override;
  end;

  TDownloadThreads = specialize TFPGList<TDownloadThread>;

  { TTaskThread }

  TTaskThread = class(TBaseThread)
  private
    ThreadsGuardian,
    GetWorkIdGuardian: TRTLCriticalSection;
    FCheckAndActiveTaskFlag: Boolean;
    FCurrentWorkingDir: String;
    {$IFDEF Windows}
    FCurrentMaxFileNameLength: Integer;
    {$ENDIF}
    FCurrentCustomFileName: String;
    FIsForDelete: Boolean;
    currentMaxThread: Integer;
    procedure SetCurrentWorkingDir(AValue: String);
    procedure SetIsForDelete(AValue: Boolean);
    procedure SyncShowBallonHint;
    procedure TerminateThreads;
    procedure WaitForThreads; inline;
  protected
    procedure SockOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
    procedure DoGetPageNumber;
    procedure CheckOut; inline;
    procedure Execute; override;
    function Compress: Boolean;
    procedure SyncStop;
    procedure StatusFailedToCreateDir;
    function FirstFailedChapters: Integer;
    function FailedChaptersExist: Boolean;
    // show notification when download completed
    procedure ShowBalloonHint;
    // general exception info
    function GetExceptionInfo: String;
  protected
    procedure GetCurrentLimit;
    procedure CreateNewDownloadThread; inline;
    procedure RemoveThread(const T: TDownloadThread); inline;
    function GetWorkId: Integer;
  private
    function InternalGetPageLinkWorkId: Integer;
    function InternalGetDownloadWorkId: Integer;
    procedure TerminateCurrent(Sender: TObject);
  public
    HTTP: THTTPSendThread;
    Flag: TFlagType;
    Manager: TDownloadManager;
    // container (for storing information)
    Container: TTaskContainer;
    // download threads
    Threads: TDownloadThreads;
    constructor Create(const C: TTaskContainer);
    destructor Destroy; override;
    function GetFileName(const AWorkId: Integer): String;
    property CurrentWorkingDir: String read FCurrentWorkingDir write SetCurrentWorkingDir;
    property CurrentMaxFileNameLength: Integer read FCurrentMaxFileNameLength;
    // current custom filename with only %FILENAME% left intact
    property CurrentCustomFileName: String read FCurrentCustomFileName write FCurrentCustomFileName;
    property IsForDelete: Boolean read FIsForDelete write SetIsForDelete;
  end;

  { TTaskContainer }

  TTaskContainer = class
  private
    FStatus: TDownloadStatusType;
    FEnabled,
    FDirty: Boolean;
    function GetRunnning: Boolean; inline;
    procedure SetEnabled(AValue: Boolean);
    procedure SetStatus(AValue: TDownloadStatusType);
  public
    DlId: String;
    Order: Integer;
    // critical section
    CS_Container: TRTLCriticalSection;
    // read count for transfer rate
    ReadCount: Integer;
    // task thread of this container
    TaskThread: TTaskThread;
    // download manager
    Manager: TDownloadManager;
    DownloadInfo: TDownloadInfo;
    // current link index
    CurrentPageNumber,
    // current chapter index
    CurrentDownloadChapterPtr,
    WorkCounter,
    DownCounter,
    PageNumber: Integer;

    ChapterNames,
    ChapterLinks,
    ChaptersStatus,
    PageContainerLinks,
    PageLinks: TStringList;
    FileNames: TStringList;
    // custom filename
    CustomFileName: String;
    constructor Create;
    destructor Destroy; override;
    procedure IncReadCount(const ACount: Integer); inline;
    // manager must be locked
    procedure DBInsert; inline;
    procedure DBUpdateEnabled; inline;
    procedure DBUpdateStatus; inline;
    // safe without locked
    procedure DBUpdate; inline;
  public
    property Status: TDownloadStatusType read FStatus write SetStatus;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Running: Boolean read GetRunnning;
  end;

  TTaskContainers = specialize TFPGList<TTaskContainer>;

  { TDownloadManager }

  TDownloadManager = class
  private
    FSortDirection: Boolean;
    FSortColumn: Integer;
    FDownloadsDB: TDownloadsDB;
    FUpdateOrderCount: Integer;
    procedure AddItemsActiveTask(const Item: TTaskContainer);
    procedure RemoveItemsActiveTask(const Item: TTaskContainer);
    function GetTask(const TaskId: Integer): TTaskContainer;
  protected
    function GetTaskCount: Integer; inline;
    function GetTransferRate: Integer;
    procedure ChangeStatusCount(const OldStatus, NewStatus: TDownloadStatusType);
    procedure DecStatusCount(const Status: TDownloadStatusType);
    procedure IncStatusCount(const Status: TDownloadStatusType);
    // it has no data validation, only StartTask after all data check passed
    procedure StartTask(const taskID: Integer);
    procedure DBUpdateOrder;
  public
    CS_Task,
    CS_ItemsActiveTask: TRTLCriticalSection;
    Items,
    ItemsActiveTask: TTaskContainers;
    isRunningBackup, isRunningRestore, isFinishTaskAccessed, isRunningBackupDownloadedChaptersList,
    isReadyForExit: Boolean;

    // status count
    CS_StatusCount: TRTLCriticalSection;
    StatusCount: array [TDownloadStatusType] of Integer;
    // disabled count
    DisabledCount,
    CompressType,
    RetryConnect: Integer;

    //downloaded chapter list database
    DownloadedChapters: TDownloadedChaptersDB;

    //exit counter
    ExitWaitOK: Boolean;


    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetTaskCount;


    // DB access
    procedure Lock;
    procedure UnLock;
    procedure Backup;
    procedure Restore;
    procedure UpdateOrder; inline;

    // These methods relate to highlight downloaded chapters.
    procedure GetDownloadedChaptersState(const AModuleID, ALink: String;
      var AChapters: array of TChapterStateItem);

    // Add new task to the list.
    function AddTask: TTaskContainer;
    // Check and active previous work-in-progress tasks.
    procedure CheckAndActiveTaskAtStartup;
    // Check and active waiting tasks.
    procedure CheckAndActiveTask(const isCheckForFMDDo: Boolean = False);
    // Active a stopped task.
    procedure SetTaskActive(const taskID: Integer);
    // Redownload a finished task.
    procedure RedownloadTask(const taskID: Integer);
    // Stop a download/wait task.
    procedure StopTask(const taskID: Integer; const isCheckForActive: Boolean =
      True; isWaitFor: Boolean = False);
    // Start all task
    procedure StartAllTasks;
    // Stop all download/wait tasks.
    procedure StopAllTasks;
    // Stop all download task inside a task before terminate the program.
    procedure StopAllDownloadTasksForExit;
    // Delete directly, must inside lock/unlock
    procedure Delete(const TaskId: Integer);
    // Remove all finished tasks.
    procedure RemoveAllFinishedTasks;
    // check status of task
    function TaskStatusPresent(Stats: TDownloadStatusTypes): Boolean;
    // enable task
    procedure EnableTask(const TaskId: Integer);
    procedure DisableTask(const TaskId: Integer);

    // Sort
    procedure Sort(const AColumn: Integer);

    property SortDirection: Boolean read FSortDirection write FSortDirection;
    property SortColumn: Integer read FSortColumn write FSortColumn;
    property TransferRate: Integer read GetTransferRate;
    property Task[const TaskId: Integer]: TTaskContainer read GetTask; default;
    property DB: TDownloadsDB read FDownloadsDB;
  end;

resourcestring
  RS_FailedToCreateDir = 'Failed to create directory!';
  RS_FailedTryResumeTask = 'Failed, try resuming this task!';
  RS_Preparing = 'Preparing';
  RS_Downloading = 'Downloading';
  RS_Stopped = 'Stopped';
  RS_Finish = 'Completed';
  RS_Waiting = 'Waiting...';
  RS_Compressing = 'Compressing...';
  RS_Failed = 'Failed';
  RS_Disabled = 'Disabled';

implementation

uses
  frmMain, WebsiteModules, SimpleException;

function IntToStr(Value: Cardinal): String;
begin
  Result := SysUtils.IntToStr(QWord(Value));
end;

{ TDownloadThread }

constructor TDownloadThread.Create(const ATask: TTaskThread);
begin
  inherited Create(False);
  Task := ATask;
  HTTP := TModuleContainer(Task.Container.DownloadInfo.Module).CreateHTTP(Self);
  HTTP.Sock.OnStatus := @SockOnStatus;
end;

destructor TDownloadThread.Destroy;
begin
  Task.RemoveThread(Self);
  HTTP.Free;
  inherited Destroy;
end;

procedure TDownloadThread.Execute;
begin
  case Task.Flag of
    CS_DOWNLOAD      : DoDownload;
    CS_GETPAGELINK   : DoPageLink;
    CS_GETPAGENUMBER : ;
  end;
end;

function TDownloadThread.GetLinkPageFromURL(const URL: String): Boolean;
begin
  Result := False;
  if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnGetImageURL) then
    Result := TModuleContainer(Task.Container.DownloadInfo.Module).OnGetImageURL(Self, URL, TModuleContainer(Task.Container.DownloadInfo.Module));
end;

function TDownloadThread.DownloadImage: Boolean;
var
  workFilename,
  workURL,
  savedFilename: String;
begin
  Result := False;

  // check download path
  if not ForceDirectories(Task.CurrentWorkingDir) then
  begin
    Task.StatusFailedToCreateDir;
    Exit(False);
  end;

  // check pagelinks url
  workURL := Trim(Task.Container.PageLinks[WorkId]);
  if workURL = '' then
  begin
    Task.Container.PageLinks[WorkId] := 'W';
    Exit;
  end
  else
  if workURL = 'W' then
    Exit
  else
  if workURL = 'D' then
    Exit(True);

  // prepare filename
  workFilename := Task.GetFileName(WorkId);

  // download image
  savedFilename := '';

  HTTP.Reset;
  HTTP.AcceptImage;

  if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnDownloadImage) and
    (Task.Container.PageNumber = Task.Container.PageContainerLinks.Count) and
    (WorkId < Task.Container.PageContainerLinks.Count) then
      workURL := Task.Container.PageContainerLinks[WorkId];

  // OnBeforeDownloadImage
  if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnBeforeDownloadImage) then
    Result := TModuleContainer(Task.Container.DownloadInfo.Module).OnBeforeDownloadImage(Self, workURL, TModuleContainer(Task.Container.DownloadInfo.Module));

  // OnDownloadImage
  if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnDownloadImage) then
    Result := TModuleContainer(Task.Container.DownloadInfo.Module).OnDownloadImage(Self, workURL, TModuleContainer(Task.Container.DownloadInfo.Module))
  else
    Result := HTTP.GET(workURL);

  if Result then
  begin
    savedFilename := FindImageFile(Task.CurrentWorkingDir + workFilename);
    Result := savedFilename <> '';
    if not Result then
    begin
      if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnSaveImage) then
        savedFilename := TModuleContainer(Task.Container.DownloadInfo.Module).OnSaveImage(Self, Task.CurrentWorkingDir, workFilename, TModuleContainer(Task.Container.DownloadInfo.Module))
      else
        savedFilename := SaveImageStreamToFile(HTTP, Task.CurrentWorkingDir, workFilename);
      Result := savedFilename <> '';
    end;
  end;

  if Result then
    Result := FileExists(savedFilename);

  if Terminated then Exit(False);
  if Result then
  begin
    Task.Container.PageLinks[WorkId] := 'D';
    // OnAfterImageSaved
    if Assigned(TModuleContainer(Task.Container.DownloadInfo.Module).OnAfterImageSaved) then
      Result := TModuleContainer(Task.Container.DownloadInfo.Module).OnAfterImageSaved(Self, savedFilename, TModuleContainer(Task.Container.DownloadInfo.Module));
  end;
end;

procedure TDownloadThread.SockOnStatus(Sender: TObject; Reason: THookSocketReason;
  const Value: String);
begin
  if Reason = HR_ReadCount then
    Task.Container.IncReadCount(StrToIntDef(Value, 0));
end;

procedure TDownloadThread.DoPageLink;
begin
  WorkId := Task.GetWorkId;
  while WorkId <> -1 do
  begin
    if GetLinkPageFromURL(Task.Container.ChapterLinks.Strings[Task.Container.CurrentDownloadChapterPtr]) then
      DoSuccess;
    WorkId := Task.GetWorkId;
    if WorkId <> -1 then
      HTTP.Reset;
  end;
end;

procedure TDownloadThread.DoDownload;
begin
  WorkId := Task.GetWorkId;
  while WorkId <> -1 do
  begin
    if DownloadImage then
      DoSuccess;
    WorkId := Task.GetWorkId;
    if WorkId <> -1 then
      HTTP.Reset;
  end;
end;

procedure TDownloadThread.DoSuccess;
begin
  EnterCriticalSection(Task.Container.CS_Container);
  try
    InterLockedIncrement(Task.Container.DownCounter);
    Task.Container.DownloadInfo.Progress :=
      Format('%d/%d', [Task.Container.DownCounter, Task.Container.PageNumber]);
  finally
    LeaveCriticalSection(Task.Container.CS_Container);
  end;
end;

// ----- TTaskThread -----

constructor TTaskThread.Create(const C: TTaskContainer);
begin
  inherited Create(False);
  Container:=C;
  Container.TaskThread:=Self;
  Container.Manager.AddItemsActiveTask(Container);
  TModuleContainer(Container.DownloadInfo.Module).IncActiveTaskCount;

  InitCriticalSection(ThreadsGuardian);
  InitCriticalSection(GetWorkIdGuardian);
  Threads := TDownloadThreads.Create;
  FCheckAndActiveTaskFlag := True;
  FIsForDelete := False;
  FCurrentWorkingDir := '';
  FCurrentCustomFileName := '';
  {$IFDEF WINDOWS}
  FCurrentMaxFileNameLength := 0;
  {$ENDIF}
  OnCustomTerminate := @TerminateCurrent;

  HTTP:=TModuleContainer(Container.DownloadInfo.Module).CreateHTTP(Self);
  HTTP.Sock.OnStatus:=@SockOnStatus;
end;

destructor TTaskThread.Destroy;
begin
  WaitForThreads;
  with Container do
  begin
    DownloadInfo.DateLastDownloaded := Now;
    TModuleContainer(DownloadInfo.Module).DecActiveTaskCount;
    Manager.RemoveItemsActiveTask(Container);
    TaskThread:=nil;
    if not (IsForDelete or Manager.isReadyForExit) then
    begin
      Container.ReadCount := 0;
      DownloadInfo.TransferRate := '';
      if Status <> STATUS_STOP then
      begin
        if (WorkCounter >= PageLinks.Count) and
           (CurrentDownloadChapterPtr >= ChapterLinks.Count) and
           (not FailedChaptersExist) then
        begin
          DownloadInfo.Status := Format('[%d/%d] %s',[Container.ChapterLinks.Count,Container.ChapterLinks.Count,RS_Finish]);
          DownloadInfo.Progress := '';
          Status := STATUS_FINISH;
        end
        else
        if not (Status in [STATUS_FAILED, STATUS_PROBLEM]) then
        begin
          DownloadInfo.Status :=
            Format('[%d/%d] %s', [CurrentDownloadChapterPtr + 1,
            ChapterLinks.Count, RS_Stopped]);
          Status := STATUS_STOP;
          FCheckAndActiveTaskFlag := False;
        end;
        if not isExiting then
          Synchronize(@SyncStop);
      end;
    end;
    DBUpdate;
  end;
  Threads.Free;
  HTTP.Free;
  DoneCriticalsection(ThreadsGuardian);
  DoneCriticalSection(GetWorkIdGuardian);
  inherited Destroy;
end;

function TTaskThread.GetFileName(const AWorkId: Integer): String;
{$IFDEF WINDOWS}
var
  s: UnicodeString;
{$ENDIF}
begin
  Result := '';
  if (Container.FileNames.Count = Container.PageLinks.Count) and
    (AWorkId < Container.FileNames.Count) then
    Result := Container.FileNames[AWorkId];
  if Result = '' then
    Result := Format('%.3d', [AWorkId + 1]);
  Result := StringReplace(CurrentCustomFileName, CR_FILENAME, Result, [rfReplaceAll]);
  {$IFDEF WINDOWS}
  s := UTF8Decode(Result);
  if Length(s) > FCurrentMaxFileNameLength then
  begin
    Delete(s, 1, Length(s) - FCurrentMaxFileNameLength);
    Result := UTF8Encode(s);
  end;
  {$ENDIF}
end;

function TTaskThread.Compress: Boolean;
var
  uPacker: TPacker;
  i: Integer;
  s: String;
begin
  Result := True;
  if (Container.Manager.CompressType >= 1) then
  begin
    Container.DownloadInfo.Status :=
      Format('[%d/%d] %s', [Container.CurrentDownloadChapterPtr + 1,
      Container.ChapterLinks.Count, RS_Compressing]);
    uPacker := TPacker.Create;
    try
      case Container.Manager.CompressType of
        1: uPacker.Format := pfZIP;
        2: uPacker.Format := pfCBZ;
        3: uPacker.Format := pfPDF;
        4: uPacker.Format := pfEPUB;
      end;
      uPacker.CompressionQuality := OptionPDFQuality;
      uPacker.Path := CurrentWorkingDir;
      uPacker.FileName := RemovePathDelim(CorrectPathSys(CorrectPathSys(Container.DownloadInfo.SaveTo) +
        Container.ChapterNames[Container.CurrentDownloadChapterPtr]));
      for i := 0 to Container.PageLinks.Count - 1 do
      begin
        s := FindImageFile(uPacker.Path + GetFileName(i));
        if s <> '' then
          uPacker.FileList.Add(s);
      end;
      Result := uPacker.Execute;
      if not Result then
        Logger.SendWarning(Self.ClassName+', failed to compress. '+uPacker.SavedFileName);
    except
      on E: Exception do
      begin
        E.Message := E.Message + LineEnding + '  In TTaskThread.Compress' + LineEnding + GetExceptionInfo;
        MainForm.ExceptionHandler(Self, E);
      end;
    end;
    uPacker.Free;
  end;
end;

procedure TTaskThread.SyncStop;
begin
  Container.Manager.CheckAndActiveTask(FCheckAndActiveTaskFlag);
end;

procedure TTaskThread.StatusFailedToCreateDir;
begin
  Logger.SendError(Format('Failed to create dir(%d) = %s', [Length(CurrentWorkingDir), CurrentWorkingDir]));
  Container.DownloadInfo.Status := Format('[%d/%d] %s (%d) %s', [
    Container.CurrentDownloadChapterPtr,
    Container.ChapterLinks.Count,
    RS_FailedToCreateDir, Length(CurrentWorkingDir), LineEnding + CurrentWorkingDir]);
  Container.Status := STATUS_FAILED;
end;

function TTaskThread.FirstFailedChapters: Integer;
var
  i: Integer;
begin
  for i := 0 to Container.ChaptersStatus.Count - 1 do
    if Container.ChaptersStatus[i] = 'F' then Exit(i);
  Result := -1;
end;

function TTaskThread.FailedChaptersExist: Boolean;
begin
  Result := FirstFailedChapters <> -1;
end;

procedure TTaskThread.ShowBalloonHint;
begin
  if OptionShowBalloonHint then
    Synchronize(@SyncShowBallonHint);
end;

function TTaskThread.GetExceptionInfo: String;
begin
  Result :=
    '  Flag        : ' + GetEnumName(TypeInfo(TFlagType), Integer(Flag)) + LineEnding +
    '  Website     : ' + Container.DownloadInfo.ModuleID + LineEnding +
    '  Title       : ' + Container.DownloadInfo.title + LineEnding +
    '  Chapterlink : ' + Container.ChapterLinks[Container.CurrentDownloadChapterPtr] + LineEnding +
    '  Chaptername : ' + Container.ChapterNames[Container.CurrentDownloadChapterPtr] + LineEnding;
end;

procedure TTaskThread.SetCurrentWorkingDir(AValue: String);
{$IFDEF WINDOWS}
var
  s: UnicodeString;
{$ENDIF}
begin
  if FCurrentWorkingDir = AValue then Exit;
  FCurrentWorkingDir := CorrectPathSys(AValue);
  {$IFDEF Windows}
  s := UTF8Decode(FCurrentWorkingDir);
  if MainForm.cbOptionEnableLongNamePaths.Checked then
    FCurrentMaxFileNameLength := FMDMaxImageFilePath + Length(s)
  else
    FCurrentMaxFileNameLength := FMDMaxImageFilePath - Length(s);
  {$ENDIF}
end;

procedure TTaskThread.SetIsForDelete(AValue: Boolean);
begin
  if FIsForDelete = AValue then Exit;
  FIsForDelete := AValue;
end;

procedure TTaskThread.SyncShowBallonHint;
begin
  with MainForm.TrayIcon, Container.DownloadInfo do
  begin
    if Container.Status = STATUS_FAILED then
    begin
      BalloonFlags := bfError;
      BalloonHint := QuotedStrd(Title);
      if Status = '' then
        BalloonHint := BalloonHint + ' - ' + RS_Failed
      else
        BalloonHint := BalloonHint + LineEnding + Status;
    end
    else
    if Container.Status = STATUS_FINISH then
    begin
      BalloonFlags := bfInfo;
      BalloonHint :=
        '"' + Container.DownloadInfo.title + '" - ' + RS_Finish;
    end;
    ShowBalloonHint;
  end;
end;

procedure TTaskThread.TerminateThreads;
var
  t: TDownloadThread;
begin
  EnterCriticalsection(ThreadsGuardian);
  try
    for t in Threads do
      t.Terminate;
  finally
    LeaveCriticalsection(ThreadsGuardian);
  end;
end;

procedure TTaskThread.WaitForThreads;
begin
  while Threads.Count <> 0 do
    Sleep(HeartBeatRate);
end;

procedure TTaskThread.SockOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
begin
  if Reason = HR_ReadCount then
    Container.IncReadCount(StrToIntDef(Value, 0));
end;

procedure TTaskThread.DoGetPageNumber;
var
  success: Boolean;
begin
  success := False;
  // Get total number of images/pages per chapter
  Container.PageNumber := 0;
  if Assigned(TModuleContainer(Container.DownloadInfo.Module).OnGetPageNumber) then
  begin
    success := TModuleContainer(Container.DownloadInfo.Module).OnGetPageNumber(
      Self,
      Container.ChapterLinks[Container.CurrentDownloadChapterPtr],
      TModuleContainer(Container.DownloadInfo.Module));
  end;
  if Container.PageLinks.Count > 0 then
    TrimStrings(Container.PageLinks);

  // Prepare 'space' for storing image url.
  if (not Terminated) and
    (Container.PageNumber > 0) then
  begin
    while Container.PageLinks.Count < Container.PageNumber do
      Container.PageLinks.Add('W');
  end
  else
    success := False;

  if success then
  begin
    EnterCriticalSection(Container.CS_Container);
    try
      InterLockedIncrement(Container.DownCounter);
      Container.DownloadInfo.Progress := Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
    finally
      LeaveCriticalSection(Container.CS_Container);
    end;
  end;
end;

procedure TTaskThread.GetCurrentLimit;
begin
  if TModuleContainer(Container.DownloadInfo.Module).MaxThreadPerTaskLimit > 0 then
    currentMaxThread := TModuleContainer(Container.DownloadInfo.Module).MaxThreadPerTaskLimit
  else
    currentMaxThread := OptionMaxThreads;
  if currentMaxThread > OptionMaxThreads then
    currentMaxThread := OptionMaxThreads;
  if (TModuleContainer(Container.DownloadInfo.Module).ConnectionsQueue.MaxConnections > 0) and (currentMaxThread > TModuleContainer(Container.DownloadInfo.Module).ConnectionsQueue.MaxConnections) then
    currentMaxThread := TModuleContainer(Container.DownloadInfo.Module).ConnectionsQueue.MaxConnections;
  if currentMaxThread < 1 then
    currentMaxThread := 1;  //default
end;

procedure TTaskThread.CreateNewDownloadThread;
begin
  EnterCriticalsection(ThreadsGuardian);
  try
    Threads.Add(TDownloadThread.Create(Self));
  finally
    LeaveCriticalsection(ThreadsGuardian);
  end;
end;

procedure TTaskThread.RemoveThread(const T: TDownloadThread);
begin
  EnterCriticalSection(ThreadsGuardian);
  try
    Threads.Remove(T);
  finally
    LeaveCriticalSection(ThreadsGuardian);
  end;
end;

function TTaskThread.GetWorkId: Integer;
begin
  Result := -1;
  if (Container.WorkCounter >= Container.PageNumber) then Exit;
  if Terminated then Exit;

  EnterCriticalSection(GetWorkIdGuardian);
  try
    GetCurrentLimit;
    if Threads.Count > currentMaxThread then Exit;

    case Flag of
      CS_DOWNLOAD: Result := InternalGetDownloadWorkId;
      CS_GETPAGELINK: Result := InternalGetPageLinkWorkId;
      CS_GETPAGENUMBER: ;
    end;

    // spawn new worker thread
    if (Threads.Count < currentMaxThread) and (Container.WorkCounter < Container.PageNumber) then
      CreateNewDownloadThread;
  finally
    LeaveCriticalSection(GetWorkIdGuardian);
  end;
end;

function TTaskThread.InternalGetPageLinkWorkId: Integer;
begin
  Result := -1;

  while Container.WorkCounter < Container.PageLinks.Count do
  begin
    if Container.PageLinks[Container.WorkCounter] = 'W' then
    begin
      Result := Container.WorkCounter;
      InterlockedIncrement(Container.WorkCounter);
      Break;
    end;
    InterlockedIncrement(Container.WorkCounter);
    InterlockedIncrement(Container.DownCounter);
    InterLockedIncrement(Container.CurrentPageNumber);
    Container.DownloadInfo.Progress := Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
  end;
end;

function TTaskThread.InternalGetDownloadWorkId: Integer;
begin
  Result := -1;

  while Container.WorkCounter < Container.PageLinks.Count do
  begin
    if Container.PageLinks[Container.WorkCounter] <> 'D' then
    begin
      Result := Container.WorkCounter;
      InterlockedIncrement(Container.WorkCounter);
      Break;
    end;
    InterlockedIncrement(Container.WorkCounter);
    InterlockedIncrement(Container.DownCounter);
    Container.DownloadInfo.Progress := Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
  end;
end;

procedure TTaskThread.TerminateCurrent(Sender: TObject);
begin
  TerminateThreads;
end;

procedure TTaskThread.CheckOut;
begin
  CreateNewDownloadThread;
  WaitForThreads;
end;

procedure TTaskThread.Execute;
var
  DynamicPageLink: Boolean;
  FailedRetryCount: Integer = 0;

  function CheckForPrepare: Boolean;
  var
    i: Integer;
  begin
    if Container.PageLinks.Count = 0 then
      Exit(True);
    Result := False;
    if Container.PageLinks.Count > 0 then
      for i := 0 to Container.PageLinks.Count - 1 do
        if (Container.PageLinks[i] = 'W') or
          (Container.PageLinks[i] = '') then
          Exit(True);
  end;

  function CheckForExists: Integer;
  var
    i: Integer;
  begin
    Result := 0;
    if Container.PageLinks.Count > 0 then
      begin
        for i := 0 to Container.PageLinks.Count - 1 do
        begin
          if MainForm.cbOptionEnableLongNamePaths.Checked then
          begin
            if Pos('\\?\', CurrentWorkingDir) = 0 then
              CurrentWorkingDir := '\\?\' + CurrentWorkingDir;
          end;
          if ImageFileExists(CurrentWorkingDir + GetFileName(i)) then
          begin
            Container.PageLinks[i] := 'D';
            Inc(Result);
          end
          else
          if Container.PageLinks[i] = 'D' then
          begin
            if DynamicPageLink then
              Container.PageLinks[i] := 'G'
            else
              Container.PageLinks[i] := 'W';
          end;
        end;
      end;
  end;

  function CheckForFinish: Boolean;
  var
    c, i: Integer;
    s: String;
  begin
    Result := False;
    if Container.PageLinks.Count = 0 then
      Exit;

    c := Container.PageLinks.Count - CheckForExists;

    Result := c = 0;
    if Result = False then
    begin
      s:=LineEnding;
      for i:=0 to Container.PageLinks.Count-1 do
        if Container.PageLinks[i]<>'D' then
          s+='['+GetFileName(i)+'] '+Container.PageLinks[i]+LineEnding;
      Logger.SendWarning(Format('%s.CheckForFinish failed %d of %d [%s] "%s" > "%s"',
        [Self.ClassName,
        c,
        Container.PageLinks.Count,
        Container.DownloadInfo.Website,
        Container.DownloadInfo.Title,
        Container.ChapterLinks[Container.CurrentDownloadChapterPtr]])+s);
    end;
  end;

var
  i: Integer;
begin
  Container.DownloadInfo.DateLastDownloaded := Now;
  Container.DownloadInfo.TransferRate := FormatByteSize(Container.ReadCount, true);
  try
    DynamicPageLink := TModuleContainer(Container.DownloadInfo.Module).DynamicPageLink;

    if Trim(Container.CustomFileName) = '' then
      Container.CustomFileName := DEFAULT_FILENAME_CUSTOMRENAME;

    while Container.ChaptersStatus.Count < Container.CurrentDownloadChapterPtr - 1 do
      Container.ChaptersStatus.Add('D');
    while Container.ChaptersStatus.Count < Container.ChapterLinks.Count do
      Container.ChaptersStatus.Add('P');

    if OptionAlwaysStartTaskFromFailedChapters and (Container.CurrentDownloadChapterPtr <> 0) then
      Container.CurrentDownloadChapterPtr := 0;

    while Container.CurrentDownloadChapterPtr < Container.ChapterLinks.Count do
    begin
      while Container.ChaptersStatus[Container.CurrentDownloadChapterPtr] = 'D' do
        Inc(Container.CurrentDownloadChapterPtr);

      WaitForThreads;
      if Terminated then Exit;

      //check path
      if OptionGenerateChapterFolder then
        CurrentWorkingDir := CorrectPathSys(Container.DownloadInfo.SaveTo) +
          Container.ChapterNames[Container.CurrentDownloadChapterPtr]
      else
        CurrentWorkingDir := Container.DownloadInfo.SaveTo;
      if not ForceDirectories(CurrentWorkingDir) then
      begin
        StatusFailedToCreateDir;
        ShowBalloonHint;
        Exit;
      end;

      if Assigned(TModuleContainer(Container.DownloadInfo.Module).OnTaskStart) then
        TModuleContainer(Container.DownloadInfo.Module).OnTaskStart(Container, TModuleContainer(Container.DownloadInfo.Module));

      // set current working custom filename
      CurrentCustomFileName :=  CustomRename(Container.CustomFileName,
        Container.DownloadInfo.Website,
        Container.DownloadInfo.Title,
        '',
        '',
        Container.ChapterNames[Container.CurrentDownloadChapterPtr],
        '',
        OptionChangeUnicodeCharacter,
        OptionChangeUnicodeCharacterStr,
        CR_FILENAME);

      // Get total number of images/pages per chapter
      if Container.PageLinks.Count = 0 then
      begin
        Container.PageNumber := 0;
        Flag := CS_GETPAGENUMBER;
        Container.WorkCounter := 0;
        Container.DownCounter := 0;
        Container.DownloadInfo.Progress := '0/0';
        Container.DownloadInfo.Status :=
          Format('[%d/%d] %s (%s)',
          [Container.CurrentDownloadChapterPtr + 1,
          Container.ChapterLinks.Count,
          RS_Preparing,
          Container.ChapterNames[Container.CurrentDownloadChapterPtr]]);
        Container.Status := STATUS_PREPARE;

        DoGetPageNumber;
        if Terminated then
        begin
          Container.PageLinks.Clear;
          Container.PageNumber := 0;
          Exit;
        end;
      end;

      // Check files, if exist set mark 'D', otherwise 'W' or 'G' for dynamic image url
      CheckForExists;

      // Get the real image urls
      if Container.PageLinks.Count = 0 then
        Container.PageLinks.Add('W');
      Container.PageNumber := Container.PageLinks.Count;
      if (not DynamicPageLink) and CheckForPrepare then
      begin
        Flag := CS_GETPAGELINK;
        Container.WorkCounter := 0;
        Container.DownCounter := 0;
        Container.DownloadInfo.iProgress := 0;
        Container.DownloadInfo.Progress :=
          Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
        Container.DownloadInfo.Status :=
          Format('[%d/%d] %s (%s)',
          [Container.CurrentDownloadChapterPtr + 1,
          Container.ChapterLinks.Count,
          RS_Preparing,
          Container.ChapterNames[Container.CurrentDownloadChapterPtr]]);
        Container.Status := STATUS_PREPARE;

        Checkout;
        if Terminated then Exit;

        //check if pagelink is found. Else set again to 'W'(some script return '')
        if Container.PageLinks.Count > 0 then
        begin
          for i := 0 to Container.PageLinks.Count - 1 do
          begin
            if Trim(Container.PageLinks[i]) = '' then
              Container.PageLinks[i] := 'W';
          end;
        end;
      end;
      if Terminated then Exit;

      // download the images
      // If Container doesn't have any image, we will skip the loop. Otherwise
      // download them
      Container.PageNumber := Container.PageLinks.Count;
      if Container.PageLinks.Count > 0 then
      begin
        Flag := CS_DOWNLOAD;
        Container.WorkCounter := 0;
        Container.DownCounter := 0;
        Container.DownloadInfo.iProgress := 0;
        Container.DownloadInfo.Progress :=
          Format('%d/%d', [Container.DownCounter, Container.PageNumber]);
        Container.DownloadInfo.Status :=
          Format('[%d/%d] %s (%s)',
          [Container.CurrentDownloadChapterPtr + 1,
          Container.ChapterLinks.Count,
          RS_Downloading,
          Container.ChapterNames[Container.CurrentDownloadChapterPtr]]);
        Container.Status := STATUS_DOWNLOAD;

        Checkout;
        if Terminated then Exit;

        //check if all page is downloaded
        if CheckForFinish then
        begin
          Container.DownloadInfo.Progress := '';
          Container.Status := STATUS_COMPRESS;
          if not Compress then
            Container.Status := STATUS_FAILED;
        end
        else
        begin
          Container.Status := STATUS_FAILED;
        end;
      end
      else
      begin
        Logger.SendWarning(Format('%s, pagelinks is empty. "%s" "%s" "%s"',
          [Self.ClassName,
           Container.DownloadInfo.Title,
           Container.ChapterNames[Container.CurrentDownloadChapterPtr],
           Container.ChapterLinks[Container.CurrentDownloadChapterPtr]
          ]));
        Container.Status := STATUS_FAILED;
      end;

      if Container.Status = STATUS_FAILED  then
        Container.ChaptersStatus[Container.CurrentDownloadChapterPtr] := 'F'
      else
        Container.ChaptersStatus[Container.CurrentDownloadChapterPtr] := 'D';

      Container.CurrentPageNumber := 0;
      Container.PageLinks.Clear;
      Container.PageContainerLinks.Clear;
      InterLockedIncrement(Container.CurrentDownloadChapterPtr);

      if (Container.CurrentDownloadChapterPtr = Container.ChapterLinks.Count) and
         (FailedRetryCount < OptionRetryFailedTask) then
      begin
        Container.CurrentDownloadChapterPtr := FirstFailedChapters;
        if Container.CurrentDownloadChapterPtr <> -1 then
          Inc(FailedRetryCount)
        else
          Container.CurrentDownloadChapterPtr := Container.ChapterLinks.Count;
      end;
    end;

    if FailedChaptersExist then
    begin
      Container.DownloadInfo.Status := Format('[%d/%d] %s', [
        Container.CurrentDownloadChapterPtr,
        Container.ChapterLinks.Count,
        RS_FailedTryResumeTask]);
      Container.DownloadInfo.Progress := '';
      Container.CurrentDownloadChapterPtr := 0;
      Container.Status := STATUS_FAILED;
      Logger.SendWarning(Self.ClassName+'.Execute end with failed chapter(s)');
    end
    else
    begin
      Container.DownloadInfo.Status := Format('[%d/%d] %s',[Container.ChapterLinks.Count,Container.ChapterLinks.Count,RS_Finish]);
      Container.DownloadInfo.Progress := '';
      Container.Status := STATUS_FINISH;
    end;
    ShowBalloonHint;
  except
    on E: Exception do
    begin
      E.Message := E.Message + LineEnding + '  In TTaskThread.Execute' + LineEnding + GetExceptionInfo;
      MainForm.ExceptionHandler(Self, E);
    end;
  end;
end;

{ TTaskContainer }

procedure TTaskContainer.SetStatus(AValue: TDownloadStatusType);
begin
  if FStatus = AValue then Exit;
  if Assigned(Manager) then
    Manager.ChangeStatusCount(FStatus, AValue);
  FStatus := AValue;
end;

procedure TTaskContainer.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if Assigned(Manager) then
  begin
    if not Manager.isRunningRestore then
      DBupdateEnabled;
    if Enabled then
      Dec(Manager.DisabledCount)
    else
      Inc(Manager.DisabledCount);
  end;
end;

function TTaskContainer.GetRunnning: Boolean;
begin
  Result:=TaskThread<>nil;
end;

constructor TTaskContainer.Create;
begin
  inherited Create;
  Order := -1;
  FDirty := False;
  DlId := '';
  InitCriticalSection(CS_Container);
  ChapterLinks := TStringList.Create;
  ChapterNames := TStringList.Create;
  ChaptersStatus := TStringList.Create;
  PageLinks := TStringList.Create;
  PageContainerLinks := TStringList.Create;
  FileNames := TStringList.Create;
  ReadCount := 0;
  WorkCounter := 0;
  CurrentPageNumber := 0;
  CurrentDownloadChapterPtr := 0;
  CustomFileName := OptionFilenameCustomRename;
  FStatus := STATUS_NONE;
  FEnabled := True;
end;

destructor TTaskContainer.Destroy;
begin
  FileNames.Free;
  PageContainerLinks.Free;
  PageLinks.Free;
  ChapterNames.Free;
  ChapterLinks.Free;
  ChaptersStatus.Free;
  DoneCriticalsection(CS_Container);
  if Assigned(Manager) then
    Manager.DecStatusCount(Status);
  inherited Destroy;
end;

procedure TTaskContainer.IncReadCount(const ACount: Integer);
begin
  InterlockedExchangeAdd(ReadCount, ACount);
end;

procedure TTaskContainer.DBInsert;
begin
  DlId:=Manager.FDownloadsDB.Add(
    FEnabled,
    Order,
    Integer(Status),
    CurrentDownloadChapterPtr,
    PageNumber,
    CurrentPageNumber,
    DownloadInfo.ModuleID,
    DownloadInfo.Link,
    DownloadInfo.Title,
    DownloadInfo.Status,
    DownloadInfo.Progress,
    DownloadInfo.SaveTo,
    DownloadInfo.DateAdded,
    DownloadInfo.DateLastDownloaded,
    ChapterLinks.Text,
    ChapterNames.Text,
    PageLinks.Text,
    PageContainerLinks.Text,
    FileNames.Text,
    CustomFileName,
    ChaptersStatus.Text);
end;

procedure TTaskContainer.DBUpdate;
begin
  Manager.FDownloadsDB.Update(
    DlId,
    Integer(Status),
    CurrentDownloadChapterPtr,
    PageNumber,
    CurrentPageNumber,
    DownloadInfo.Status,
    DownloadInfo.Progress,
    DownloadInfo.DateLastDownloaded,
    PageLinks.Text,
    PageContainerLinks.Text,
    FileNames.Text,
    ChaptersStatus.Text);
end;

procedure TTaskContainer.DBUpdateEnabled;
begin
  Manager.FDownloadsDB.UpdateEnabled(DlId,FEnabled);
end;

procedure TTaskContainer.DBUpdateStatus;
begin
  Manager.FDownloadsDB.UpdateStatus(DlId,Integer(Status),DownloadInfo.Status);
end;

{ TDownloadManager }

procedure TDownloadManager.AddItemsActiveTask(const Item: TTaskContainer);
begin
  EnterCriticalsection(CS_ItemsActiveTask);
  try
    ItemsActiveTask.Add(Item);
  finally
    LeaveCriticalsection(CS_ItemsActiveTask);
  end;
end;

procedure TDownloadManager.RemoveItemsActiveTask(const Item: TTaskContainer);
begin
  EnterCriticalsection(CS_ItemsActiveTask);
  try
    ItemsActiveTask.Remove(Item);
  finally
    LeaveCriticalsection(CS_ItemsActiveTask);
  end;
end;

function TDownloadManager.GetTask(const TaskId: Integer): TTaskContainer;
begin
  Result := Items[TaskId];
end;

function TDownloadManager.GetTaskCount: Integer;
begin
  Result := Items.Count;
end;

function TDownloadManager.GetTransferRate: Integer;
var
  i: Integer;
begin
  Result := 0;
  if ItemsActiveTask.Count = 0 then Exit;
  EnterCriticalSection(CS_ItemsActiveTask);
  try
    for i := 0 to ItemsActiveTask.Count - 1 do
      with ItemsActiveTask[i] do
      begin
        EnterCriticalSection(CS_Container);
        try
          DownloadInfo.TransferRate := FormatByteSize(ReadCount, True);
          Inc(Result, ReadCount);
          ReadCount := 0;
        finally
          LeaveCriticalSection(CS_Container);
        end;
      end;
  finally
    LeaveCriticalSection(CS_ItemsActiveTask);
  end;
end;

procedure TDownloadManager.ChangeStatusCount(const OldStatus,
  NewStatus: TDownloadStatusType);
begin
  EnterCriticalsection(CS_StatusCount);
  try
    if OldStatus = NewStatus then Exit;
    if StatusCount[OldStatus] > 0 then
      Dec(StatusCount[OldStatus]);
    Inc(StatusCount[NewStatus]);
  finally
    LeaveCriticalsection(CS_StatusCount);
  end;
end;

procedure TDownloadManager.DecStatusCount(const Status: TDownloadStatusType);
begin
  EnterCriticalsection(CS_StatusCount);
  try
    if StatusCount[Status] > 0 then
      Dec(StatusCount[Status]);
  finally
    LeaveCriticalsection(CS_StatusCount);
  end;
end;

procedure TDownloadManager.IncStatusCount(const Status: TDownloadStatusType);
begin
  EnterCriticalsection(CS_StatusCount);
  try
    Inc(StatusCount[Status]);
  finally
    LeaveCriticalsection(CS_StatusCount);
  end;
end;

constructor TDownloadManager.Create;
var
  ds: TDownloadStatusType;
begin
  inherited Create;
  InitCriticalSection(CS_Task);
  InitCriticalSection(CS_ItemsActiveTask);
  InitCriticalSection(CS_StatusCount);

  isFinishTaskAccessed := False;
  isRunningBackup := False;
  isRunningBackupDownloadedChaptersList := False;
  isReadyForExit := False;
  FUpdateOrderCount:=0;

  for ds := Low(StatusCount) to High(StatusCount) do
    StatusCount[ds] := 0;
  DisabledCount := 0;

  Items := TTaskContainers.Create;
  ItemsActiveTask := TTaskContainers.Create;

  ForceDirectories(USERDATA_FOLDER);
  DownloadedChapters := TDownloadedChaptersDB.Create;
  DownloadedChapters.Filename := DOWNLOADEDCHAPTERSDB_FILE;
  DownloadedChapters.OnError := @MainForm.ExceptionHandler;
  DownloadedChapters.Open;

  FDownloadsDB := TDownloadsDB.Create(DOWNLOADSDB_FILE);
  FDownloadsDB.Open(False, False);
end;

destructor TDownloadManager.Destroy;
var
  i: Integer;
begin
  for i:=0 to Items.Count-1 do
    Items[i].Free;
  Items.Free;
  ItemsActiveTask.Free;
  DownloadedChapters.Free;
  FDownloadsDB.Free;
  DoneCriticalsection(CS_ItemsActiveTask);
  DoneCriticalsection(CS_Task);
  DoneCriticalsection(CS_StatusCount);
  inherited Destroy;
end;

procedure TDownloadManager.Restore;
var
  t: TTaskContainer;
begin
  if not FDownloadsDB.Connection.Connected then Exit;
  if FDownloadsDB.OpenTable(False) then
  try
    isRunningRestore:=True;
    if FDownloadsDB.RecordCount = 0 then Exit;
    EnterCriticalsection(CS_Task);
    try
      //FDownloadsDB.Table.Last; //load all to memory
      FDownloadsDB.Table.First;
      while not FDownloadsDB.Table.EOF do
      begin
        t:=TTaskContainer.Create;
        t.Order:=Items.Add(t);
        t.Manager:=Self;
        with t, FDownloadsDB.Table do
        begin
          DlId                            := Fields[f_id].AsString;
          Enabled                         := Fields[f_enabled].AsBoolean;
          Status                          := TDownloadStatusType(Fields[f_taskstatus].AsInteger);
          CurrentDownloadChapterPtr       := Fields[f_chapterptr].AsInteger;
          PageNumber                      := Fields[f_numberofpages].AsInteger;
          CurrentPageNumber               := Fields[f_currentpage].AsInteger;
          DownloadInfo.ModuleID           := Fields[f_moduleid].AsString;
          DownloadInfo.Link               := Fields[f_link].AsString;
          DownloadInfo.Title              := Fields[f_title].AsString;
          DownloadInfo.Status             := Fields[f_status].AsString;
          DownloadInfo.Progress           := Fields[f_progress].AsString;
          if Pos('/', DownloadInfo.Progress) <> 0 then
            DownCounter := StrToIntDef(Trim(ExtractWord(1, DownloadInfo.Progress, ['/'])), 0);
          DownloadInfo.SaveTo             := Fields[f_saveto].AsString;
          DownloadInfo.DateAdded          := Fields[f_dateadded].AsDateTime;
          DownloadInfo.DateLastDownloaded   := Fields[f_datelastdownloaded].AsDateTime;
          ChapterLinks.Text               := Fields[f_chapterslinks].AsString;
          ChapterNames.Text                := Fields[f_chaptersnames].AsString;
          PageLinks.Text                  := Fields[f_pagelinks].AsString;
          PageContainerLinks.Text         := Fields[f_pagecontainerlinks].AsString;
          FileNames.Text                  := Fields[f_filenames].AsString;
          CustomFileName                  := Fields[f_customfilenames].AsString;
          ChaptersStatus.Text             := Fields[f_chaptersstatus].AsString;
        end;
        FDownloadsDB.Table.Next;
      end;
    finally
      LeaveCriticalsection(CS_Task);
    end;
    isRunningRestore:=False;
  finally
    FDownloadsDB.CloseTable;
  end;
end;

procedure TDownloadManager.Backup;
begin
  Lock;
  try
    //Logger.Send('TDownloadManager.Backup');
    DBUpdateOrder;
    FDownloadsDB.Commit(False);
    DownloadedChapters.Commit;
    DownloadedChapters.Refresh;
  finally
    UnLock;
  end;
end;

procedure TDownloadManager.Lock;
begin
  EnterCriticalSection(CS_Task);
  EnterCriticalSection(FDownloadsDB.Guardian);
  FDownloadsDB.BeginUpdate;
  isRunningBackup := True;
end;

procedure TDownloadManager.UnLock;
begin
  isRunningBackup := False;
  FDownloadsDB.EndUpdate;
  LeaveCriticalSection(FDownloadsDB.Guardian);
  LeaveCriticalSection(CS_Task);
end;

procedure TDownloadManager.DBUpdateOrder;
var
  i: Integer;
begin
  if FUpdateOrderCount=0 then Exit;
  for i := 0 to Items.Count-1 do
  with Items[i] do begin
    if i<>Order then
    begin
      Order:=i;
      FDownloadsDB.tempSQL+='UPDATE "downloads" SET "order"='+PrepSQLValue(Order)+' WHERE "id"='''+DlId+''';';
      Inc(FDownloadsDB.tempSQLcount);
      if FDownloadsDB.tempSQLcount>=MAX_BIG_SQL_FLUSH_QUEUE then
        FDownloadsDB.FlushSQL(False);
    end;
  end;
  FUpdateOrderCount:=0;
end;

procedure TDownloadManager.UpdateOrder;
begin
  Inc(FUpdateOrderCount);
end;

procedure TDownloadManager.GetDownloadedChaptersState(const AModuleID,
  ALink: String; var AChapters: array of TChapterStateItem);
var
  s: TStringList;
  i, p: Integer;
begin
  if Length(AChapters) = 0 then Exit;
  s := TStringList.Create;
  try
    s.Sorted := True;
    s.AddText(DownloadedChapters.Chapters[AModuleID, ALink]);
    if s.Count > 0 then
      for i := Low(AChapters) to High(AChapters) do
        AChapters[i].Downloaded := s.Find(LowerCase(AChapters[i].Link), p)
    else
      for i := Low(AChapters) to High(AChapters) do
        AChapters[i].Downloaded := False;
  finally
    s.Free;
  end;
end;

function TDownloadManager.AddTask: TTaskContainer;
begin
  Result := nil;
  EnterCriticalSection(CS_Task);
  try
    Result:=TTaskContainer.Create;
    Result.Order:=Items.Add(Result);
    Result.Manager:=Self;
    Result.FStatus:=STATUS_NONE;
    Result.CustomFileName:=OptionFilenameCustomRename;
  finally
    LeaveCriticalSection(CS_Task);
  end;
end;

procedure TDownloadManager.CheckAndActiveTask(const isCheckForFMDDo: Boolean);
var
  i, tcount: Integer;
begin
  if Items.Count = 0 then Exit;
  EnterCriticalSection(CS_Task);
  try
    tcount := 0;
    for i := 0 to Items.Count - 1 do
      if Items[i].Running then
        Inc(tcount);

    // item with missing module should be already checked by CheckAndActiveTaskAtStartup
    // and their status should be STATUS_STOP
    if tcount < OptionMaxParallel then
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if (Status = STATUS_WAIT) and
            (tcount < OptionMaxParallel) and
            TModuleContainer(DownloadInfo.Module).CanCreateTask then
          begin
            StartTask(i);
            Inc(tcount);
          end;
  finally
    LeaveCriticalSection(CS_Task);
  end;

  try
    if tcount > 0 then
    begin
      if not MainForm.tmRefreshDownloadsInfo.Enabled then
        MainForm.tmRefreshDownloadsInfo.Enabled := True;
      MainForm.UpdateVtDownload;
    end
    else
    begin
      FDownloadsDB.Commit(False);
      MainForm.tmRefreshDownloadsInfo.Enabled := False;
      MainForm.UpdateVtDownload;
      if isCheckForFMDDo and (OptionLetFMDDo <> DO_NOTHING) then begin
        DoAfterFMD := OptionLetFMDDo;
        MainForm.DoExitWaitCounter;
      end;
    end;
  except
    on E: Exception do
      MainForm.ExceptionHandler(Self, E);
  end;
end;

procedure TDownloadManager.SetTaskActive(const taskID: Integer);
begin
  with Items[taskID] do
    if not(Running or (Status in [STATUS_FINISH, STATUS_WAIT])) and Enabled and Assigned(DownloadInfo.Module) then
    begin
      DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Waiting]);
      Status := STATUS_WAIT;
      DBUpdateStatus;
    end;
end;

procedure TDownloadManager.RedownloadTask(const taskID: Integer);
begin
  with Items[taskID] do
  if not(Running or (Status = STATUS_WAIT)) and Enabled and Assigned(DownloadInfo.Module) then
  begin
    CurrentDownloadChapterPtr := 0;
    DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr,ChapterLinks.Count,RS_Waiting]);
    Status := STATUS_WAIT;
    ChaptersStatus := TStringList.Create;
    DBUpdate;
  end;
end;

procedure TDownloadManager.CheckAndActiveTaskAtStartup;
var
  i, tcount: Integer;
begin
  if Items.Count = 0 then Exit;
  tcount := 0;
  for i := 0 to Items.Count - 1 do
    with Items[i] do
    begin
      if Status in [STATUS_DOWNLOAD, STATUS_PREPARE, STATUS_WAIT] then
      begin
        if DownloadInfo.Module = nil then
        begin
          DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Stopped]);
          Status := STATUS_STOP;
        end
        else if (tcount < OptionMaxParallel) and  TModuleContainer(DownloadInfo.Module).CanCreateTask then
        begin
          Inc(tcount);
          StartTask(i);
        end
        else if Status <> STATUS_WAIT then
        begin
          DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Waiting]);
          Status := STATUS_WAIT;
        end;
      end;
    end;
  //force to check task if all task loaded is STATUS_WAIT
  if tcount = 0 then
    CheckAndActiveTask
  else if MainForm.tmRefreshDownloadsInfo.Enabled = False then
    MainForm.tmRefreshDownloadsInfo.Enabled := True;
  MainForm.UpdateVtDownload;
end;

procedure TDownloadManager.StartTask(const taskID: Integer);
begin
  TTaskThread.Create(Items[taskID]);
end;

procedure TDownloadManager.StopTask(const taskID: Integer;
  const isCheckForActive: Boolean; isWaitFor: Boolean);
begin
  with Items[taskID] do
  begin
    if Status = STATUS_WAIT then
    begin
      DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Stopped]);
      Status := STATUS_STOP;
      DBUpdateStatus;
    end
    else if Running then
    begin
      TaskThread.Terminate;
      if isWaitFor then
        TaskThread.WaitFor;
    end;
    if isCheckForActive then
      CheckAndActiveTask();
  end;
end;

procedure TDownloadManager.StartAllTasks;
var
  i: Integer;
begin
  if Items.Count = 0 then Exit;
  Lock;
  try
    for i := 0 to Items.Count - 1 do
      with Items[i] do
        if (Status <> STATUS_FINISH) and Assigned(DownloadInfo.Module) and (not Running) and Enabled then
        begin
          DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Waiting]);
          Status := STATUS_WAIT;
          DBUpdateStatus;
        end;
  finally
    UnLock;
  end;
  CheckAndActiveTask;
end;

procedure TDownloadManager.StopAllTasks;
var
  i: Integer;
begin
  if Items.Count = 0 then Exit;
  Lock;
  try
    for i := 0 to Items.Count - 1 do
      StopTask(i, False, False);
  finally
    UnLock;
  end;
end;

procedure TDownloadManager.StopAllDownloadTasksForExit;
var
  i: Integer;
begin
  if Items.Count > 0 then
  begin
    isReadyForExit := True;
    try
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if Running then
            TaskThread.Terminate;
      for i := 0 to Items.Count - 1 do
        with Items[i] do
          if Running then
            TaskThread.WaitFor;
    finally
      isReadyForExit := False;
    end;
  end;
end;

procedure TDownloadManager.Delete(const TaskId: Integer);
begin
  FDownloadsDB.Delete(Items[TaskId].DlId);
  Items[TaskID].Free;
  Items.Delete(taskID);
  UpdateOrder;
end;

procedure TDownloadManager.RemoveAllFinishedTasks;
var
  i: Integer;
begin
  if Items.Count = 0 then Exit;
  Lock;
  try
    for i := Items.Count - 1 downto 0 do
      if Items[i].Status = STATUS_FINISH then
        Delete(i);
  finally
    UnLock;
  end;
end;

function TDownloadManager.TaskStatusPresent(Stats: TDownloadStatusTypes): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Items.Count > 0 then
  begin
    EnterCriticalSection(CS_Task);
    try
      for i := 0 to Items.Count - 1 do
        if Items[i].Status in Stats then begin
          Result := True;
          Break;
        end;
    finally
      LeaveCriticalSection(CS_Task);
    end;
  end;
end;

procedure TDownloadManager.EnableTask(const TaskId: Integer);
begin
  If not Items[TaskId].Enabled then
    Items[TaskId].Enabled := True;
end;

procedure TDownloadManager.DisableTask(const TaskId: Integer);
begin
  with Items[TaskId] do
  begin
    if Running then
      StopTask(TaskId, False);
    if Enabled then
    begin
      if Status = STATUS_WAIT then
      begin
        DownloadInfo.Status := Format('[%d/%d] %s',[CurrentDownloadChapterPtr+1,ChapterLinks.Count,RS_Stopped]);
        Status := STATUS_STOP;
      end;
      Enabled := False;
    end;
  end;
end;

function CompareTaskContainer(const Item1, Item2: TTaskContainer): Integer;

  function GetStr(ARow: TTaskContainer): String;
  begin
    with ARow.DownloadInfo do
      case ARow.Manager.SortColumn of
        0: Result := Title;
        1: Result := Status;
        2: Result := Progress;
        3: Result := TransferRate;
        4: Result := ModuleID;
        5: Result := SaveTo;
        else
          Result := '';
      end;
  end;

  function GetDateTime(ARow: TTaskContainer): TDateTime;
  begin
    Result := ARow.DownloadInfo.DateAdded;
  end;

begin
  if (Item1.Manager.SortColumn = 6) or (Item1.Manager.SortColumn = 7) then
  begin
    if Item1.Manager.SortDirection then
      Result := CompareDateTime(GetDateTime(Item2), GetDateTime(Item1))
    else
      Result := CompareDateTime(GetDateTime(Item1), GetDateTime(Item2));
  end
  else
  begin
    if Item1.Manager.SortDirection then
      Result := NaturalCompareStr(GetStr(Item2), GetStr(Item1))
    else
      Result := NaturalCompareStr(GetStr(Item1), GetStr(Item2));
  end;
end;

procedure TDownloadManager.Sort(const AColumn: Integer);
begin
  if Items.Count < 2 then Exit;
  EnterCriticalSection(CS_Task);
  try
    SortColumn := AColumn;
    Items.Sort(@CompareTaskContainer);
    UpdateOrder;
  finally
    LeaveCriticalSection(CS_Task);
  end;

end;

end.
