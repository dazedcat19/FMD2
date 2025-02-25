unit frmLuaModulesUpdater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Menus, ExtCtrls, VirtualTrees, synautil, httpsendthread, BaseThread, StatusBarDownload,
  XQueryEngineHTML, GitHubRepoV3, MultiLog, fpjson, jsonparser, jsonscanner, dateutils;

type

  TLuaModuleRepoFlag = (fNone, fNew, fUpdate, fDelete, fDeleted, fDownloading,
    fDownloaded, fFailedDownload);

  PPLuaModuleRepo = ^PLuaModuleRepo;
  PLuaModuleRepo = ^TLuaModuleRepo;

  { TLuaModuleRepo }

  TLuaModuleRepo = class
    name: String;
    sha: String;
    last_modified: TDateTime;
    last_message: String;
    flag: TLuaModuleRepoFlag;
    oflag: TLuaModuleRepoFlag;
    function Clone: TLuaModuleRepo;
    function SyncTo(const t: TLuaModuleRepo): Boolean;
    constructor Create;
  end;

  { TLuaModulesRepos }

  TLuaModulesRepos = class
  private
    function GetCount: Integer; inline;
    function GetRepo(const AIndex: Integer): TLuaModuleRepo; inline;
  public
    Items: TStringList;

    constructor Create;
    destructor Destroy; override;

    procedure Clear; inline;
    function Add(const AName: String): TLuaModuleRepo; overload;
    procedure Add(const I: TLuaModuleRepo); overload;
    procedure LoadFromFile(const AFileName: String);
    procedure SaveToFile(const AFileName: String);
    procedure Sort;
    function Clone: TLuaModulesRepos;
    property Count: Integer read GetCount;
    property Repo[const AIndex: Integer]: TLuaModuleRepo read GetRepo; default;
  end;

  TCheckUpdateThread = class;

  { TLuaModulesUpdaterForm }

  TLuaModulesUpdaterForm = class(TForm)
    btCheckUpdate: TBitBtn;
    ckShowUpdateWarning: TCheckBox;
    ckAutoRestart: TCheckBox;
    imStates: TImageList;
    btCheckUpdateTerminate: TSpeedButton;
    tmRepaintList: TTimer;
    vtLuaModulesRepos: TVirtualStringTree;
    procedure btCheckUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btCheckUpdateTerminateClick(Sender: TObject);
    procedure tmRepaintListTimer(Sender: TObject);
    procedure vtLuaModulesReposBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtLuaModulesReposCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtLuaModulesReposGetHint(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
    procedure vtLuaModulesReposGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtLuaModulesReposGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vtLuaModulesReposHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
  private
    FListCS: TRTLCriticalSection;
    FListDirty: Boolean;
  public
    Repos: TLuaModulesRepos;
    ThreadCheck: TCheckUpdateThread;
    procedure ListDirty;
    procedure LoadLocalRepos;
    procedure ReinitList(const ASort: Boolean = True);
    procedure SortList;
  end;

  { TDownloadThread }

  TDownloadThread = class(TBaseThread)
  private
    FOwner: TCheckUpdateThread;
    FModule: TLuaModuleRepo;
    FHTTP: THTTPSendThread;
  protected
    procedure Execute; override;
  public
    constructor Create(const Owner: TCheckUpdateThread; const T: TLuaModuleRepo);
    destructor Destroy; override;
  end;

  { TCheckUpdateThread }

  TCheckUpdateThread = class(TStatusBarDownload)
  private
    FGitHubRepo: TGitHubRepo;
    FOwner: TLuaModulesUpdaterForm;
    FReposUp: TLuaModulesRepos;
    FRepos: TLuaModulesRepos;
    FMainRepos: TLuaModulesRepos;
    FThreads: TFPList;
    FThreadsCS: TRTLCriticalSection;
    FDownloadTotalCount: Integer;
    FDownloadedCount: Integer;
    FProceed: Boolean;
    FStatusList: TStringList;
    procedure RemoveThread(const T: TDownloadThread);
    procedure AddThread(const T: TDownloadThread);
  protected
    procedure SyncStartChecking;
    procedure SyncFinishChecking;
    procedure SyncAskToProceed;
    procedure SyncStartDownload;
    procedure SyncFinishDownload;
    procedure SyncFinal;
    function SyncRepos(const ARepos, AReposUp: TLuaModulesRepos): Boolean;
    procedure Download;
    procedure DoSync;
    procedure Execute; override;
  public
    constructor Create(const AOwner: TLuaModulesUpdaterForm);
    destructor Destroy; override;
    procedure AddStatus(const S: String);
  end;

var
  LuaModulesUpdaterForm: TLuaModulesUpdaterForm;

resourcestring
  RS_CheckLocalModules = 'Checking local modules...';
  RS_AwaitingProceed = 'Awaiting permission to proceed...';
  RS_DeletingFlaggedModules = 'Deleting flagged modules...';
  RS_DownloadingModule = 'Downloading %s';
  RS_GitHubConnecting = 'Connecting to GitHub...';
  RS_CheckUpdate = 'Check update';
  RS_Checking = 'Checking...';
  RS_FinishChecking = 'Finish checking';
  RS_StartDownloading = 'Downloading...';
  RS_FinishDownload = 'Finish download';
  RS_NewUpdateFoundTitle = 'Modules update found!';
  RS_NewUpdateFoundLostChanges = 'Modules update found, any local changes will be lost, procced?';
  RS_ModulesUpdatedTitle = 'Modules updated!';
  RS_ModulesUpdatedRestart = 'Modules updated, restart now?';
  RS_StatusNew = '%s NEW*';
  RS_StatusUpdate = '%s UPDATE*';
  RS_StatusRedownloaded = '%s REDOWNLOAD*';
  RS_StatusFailed = '%s FAILED*';
  RS_StatusDelete = '%s DELETE*';

implementation

uses frmCustomColor, frmDialogYesNo, frmMain, FMDOptions, LazFileUtils;

const
  // RFC 3339 - ISO 8601
  DateTimeFormatStrDecode = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss';
  DateTimeFormatStrEncode = DateTimeFormatStrDecode + '"Z"';
  UnknownDateTime = 43101.0423726852; // 01/01/2018 01:01:01

function JSONToDateTime(const s: String): TDateTime;
begin
  if Length(s) = 20 then
    Result := ScanDateTime(DateTimeFormatStrDecode, s)
  else
    Result := UnknownDateTime;
end;

function DateTimeToJSON(const d: TDateTime): String;
begin
  Result := FormatDateTime(DateTimeFormatStrEncode, d);
end;

{ TLuaModuleRepo }

function TLuaModuleRepo.Clone: TLuaModuleRepo;
begin
  Result := TLuaModuleRepo.Create;
  Result.name := name;
  Result.sha := sha;
  Result.last_modified := last_modified;
  Result.last_message := last_message;
  Result.flag := flag;
  Result.oflag := oflag;
end;

function TLuaModuleRepo.SyncTo(const t: TLuaModuleRepo): Boolean;
begin
  Result := name = t.name;
  if not Result then
    Exit;
  if sha <> t.sha then
  begin
    t.sha := sha;
    t.last_modified := last_modified;
    t.last_message := last_message;
    t.oflag := t.flag;
    t.flag := fUpdate;
  end;
end;

constructor TLuaModuleRepo.Create;
begin
  last_modified := Now;
end;

{ TLuaModulesRepos }

function TLuaModulesRepos.GetCount: Integer;
begin
  Result := Items.Count;
end;

function TLuaModulesRepos.GetRepo(const AIndex: Integer): TLuaModuleRepo;
begin
  Result := TLuaModuleRepo(Items.Objects[AIndex]);
end;

constructor TLuaModulesRepos.Create;
begin
  Items := TStringList.Create;
  Items.OwnsObjects := True;
  Clear;
end;

destructor TLuaModulesRepos.Destroy;
begin
  Clear;
  Items.Free;
  inherited Destroy;
end;

procedure TLuaModulesRepos.Clear;
begin
  Items.Clear;
end;

function TLuaModulesRepos.Add(const AName: String): TLuaModuleRepo;
begin
  Result := TLuaModuleRepo.Create;
  Result.name := AName;
  Items.AddObject(AName, Result);
end;

procedure TLuaModulesRepos.Add(const I: TLuaModuleRepo);
begin
  Items.AddObject(I.name, I);
end;

procedure TLuaModulesRepos.LoadFromFile(const AFileName: String);
var
  f: TFileStream;
  d: TJSONData;
  o: TJSONObject;
  i: Integer;
  m: TLuaModuleRepo;
begin
  if not FileExists(AFileName) then
    Exit;

  f := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    d:=GetJSON(f);
  finally
    f.Free;
  end;

  if d = nil then Exit;
  try
    Self.Clear;
    if d.JSONType=jtArray then
      with TJSONArray(d) do
        for i := 0 to Count - 1 do
        begin
          o := TJSONObject(Items[i]);
          m := Self.Add(o.Get('name', ''));
          m.sha := o.Get('sha', '');
          m.last_modified := JSONToDateTime(o.Get('last_modified', ''));
          m.last_message := o.Get('last_message', '');
          m.flag := TLuaModuleRepoFlag(o.Get('flag', 0));
          if (m.flag <> fFailedDownload) and
            (not FileExists(LUA_REPO_FOLDER + TrimFilename(m.name))) then
            m.flag := fFailedDownload;
        end;
  finally
    d.Free;
  end;
  Sort;
end;

procedure TLuaModulesRepos.SaveToFile(const AFileName: String);
var
  a: TJSONArray;
  o: TJSONObject;
  i: Integer;
  m: TLuaModuleRepo;
  f: TMemoryStream;
begin
  a := TJSONArray.Create;
  try
    for i := 0 to Items.Count - 1 do
    begin
      o := TJSONObject.Create;
      m := Repo[i];
      o.Add('name', m.name);
      o.Add('sha', m.sha);
      o.Add('last_modified', DateTimeToJSON(m.last_modified));
      o.Add('last_message', m.last_message);
      o.Add('flag', Integer(m.flag));
      a.Add(o);
    end;

    if FileExists(AFileName) then
      DeleteFile(AFileName);
    f := TMemoryStream.Create;
    try
      a.DumpJSON(f);
      f.SaveToFile(AFileName);
    finally
      f.Free;
    end;
  finally
    a.Free;
  end;
end;

procedure TLuaModulesRepos.Sort;
begin
  if Items.Count <> 0 then
    Items.Sort;
end;

function TLuaModulesRepos.Clone: TLuaModulesRepos;
var
  i: Integer;
begin
  Result := TLuaModulesRepos.Create;
  for i := 0 to Items.Count - 1 do
    Result.Items.AddObject(Items[i], Repo[i].Clone);
end;

{ TDownloadThread }

procedure TDownloadThread.Execute;
var
  f: String;
  c: Boolean;
begin
  FModule.oflag := FModule.flag;
  FModule.flag := fDownloading;
  FOwner.FOwner.ListDirty;
  FOwner.UpdateProgressBar(FOwner.FDownloadedCount, FOwner.FDownloadTotalCount);
  FOwner.UpdateStatusText(Format(RS_DownloadingModule, [ExtractFileName(FModule.name)]));
  if FHTTP.GET(FOwner.FGitHubRepo.GetDownloadURL(FModule.name)) then
  begin
    if ForceDirectories(LUA_REPO_FOLDER) then
    begin
      f := LUA_REPO_FOLDER + TrimFilename(FModule.name);
      c := True;
      if FileExists(f) then
        c := DeleteFile(f);
      c := ForceDirectories(ExtractFileDir(f));
      if c then
      begin
        FHTTP.SaveDocumentToFile(f, False, FModule.last_modified);
        if FileExists(f) then
        begin
          case FModule.oflag of
            fNew: FOwner.AddStatus(Format(RS_StatusNew, [FModule.name]));
            fUpdate: FOwner.AddStatus(Format(RS_StatusUpdate, [FModule.name]));
            fFailedDownload: FOwner.AddStatus(Format(RS_StatusRedownloaded, [FModule.name]));
            else;
          end;
          FModule.flag := fDownloaded;
          InterLockedIncrement(FOwner.FDownloadedCount);
        end;
      end;
    end;
  end
  else
  begin
    FOwner.AddStatus(Format(RS_StatusFailed, [FModule.name]));
    FModule.flag := fFailedDownload;
  end;
  FOwner.FOwner.ListDirty;
end;

constructor TDownloadThread.Create(const Owner: TCheckUpdateThread; const T: TLuaModuleRepo);
begin
  inherited Create(False);
  FOwner := Owner;
  FHTTP := THTTPSendThread.Create(Self);
  FModule := T;
  FOwner.AddThread(Self);
end;

destructor TDownloadThread.Destroy;
begin
  FOwner.RemoveThread(Self);
  FHTTP.Free;
  inherited Destroy;
end;

{ TCheckUpdateThread }

procedure TCheckUpdateThread.RemoveThread(const T: TDownloadThread);
begin
  EnterCriticalsection(FThreadsCS);
  try
    FThreads.Remove(T);
  finally
    LeaveCriticalsection(FThreadsCS);
  end;
end;

procedure TCheckUpdateThread.AddThread(const T: TDownloadThread);
begin
  EnterCriticalsection(FThreadsCS);
  try
    FThreads.Add(T);
  finally
    LeaveCriticalsection(FThreadsCS);
  end;
end;

procedure TCheckUpdateThread.SyncStartChecking;
begin
  FOwner.btCheckUpdate.Caption := RS_Checking;
  FOwner.btCheckUpdateTerminate.Visible := True;
  FOwner.tmRepaintList.Enabled := True;
end;

procedure TCheckUpdateThread.SyncFinishChecking;
begin
  FOwner.btCheckUpdate.Caption := RS_FinishChecking;
  FOwner.vtLuaModulesRepos.BeginUpdate;
  try
    FMainRepos := FOwner.Repos;
    FOwner.Repos := FRepos;
    FOwner.ReinitList;
  finally
    FOwner.vtLuaModulesRepos.EndUpdate;
  end;
end;

procedure TCheckUpdateThread.SyncAskToProceed;
begin
  with TfrmDialogYN.Create(FOwner) do
    try
      Caption := RS_NewUpdateFoundTitle;
      lbMessage.Caption := RS_NewUpdateFoundLostChanges;
      mMessages.Lines.AddStrings(FStatusList);
      FProceed := ShowModal = mrYes;
    finally
      free;
    end;
end;

procedure TCheckUpdateThread.SyncStartDownload;
begin
  FOwner.btCheckUpdate.Caption := RS_StartDownloading;
end;

procedure TCheckUpdateThread.SyncFinishDownload;
begin
  FOwner.btCheckUpdate.Caption := RS_FinishDownload;
end;

procedure TCheckUpdateThread.SyncFinal;
var
  yesRestart: Boolean;
begin
  FOwner.btCheckUpdateTerminate.Visible := False;
  FOwner.btCheckUpdate.Caption := RS_CheckUpdate;
  FOwner.ThreadCheck := nil;
  FOwner.tmRepaintList.Enabled := False;
  try
    if FMainRepos <> nil then
      try
        FOwner.vtLuaModulesRepos.BeginUpdate;
        FOwner.Repos := FMainRepos;
        FOwner.ReinitList;
        FMainRepos.SaveToFile(LUA_REPO_FILE);
      finally
        FOwner.vtLuaModulesRepos.EndUpdate;
      end;

    if not Terminated then
    begin
      if (FDownloadedCount <> 0) then
      begin
        yesRestart := OptionModulesUpdaterAutoRestart;
        if not yesRestart then
        begin
          LoadingProgressBar;
          UpdateStatusText(RS_AwaitingProceed);

          with TfrmDialogYN.Create(FOwner) do
          begin
            try
              Caption := RS_ModulesUpdatedTitle;
              lbMessage.Caption := RS_ModulesUpdatedRestart;
              mMessages.Lines.AddStrings(FStatusList);
              yesRestart := ShowModal = mrYes;
            finally
              free;
            end;
          end;
        end;
        if yesRestart then
        begin
          MainForm.RestartFMD;
        end;
      end;
    end;
  except
    on E: Exception do
       Logger.SendException('LuaModulesUpdater.SyncFinal Error!', E);
  end;
end;

function TCheckUpdateThread.SyncRepos(const ARepos, AReposUp: TLuaModulesRepos): Boolean;
var
  i, j, imax, jmax, k, inew, iupdate: Integer;
  newfound: Boolean;
  m, u: TLuaModuleRepo;
begin
  i := 0;
  j := 0;
  inew := 0;
  iupdate := 0;
  imax := ARepos.Items.Count;
  jmax := AReposUp.Items.Count;
  while (i < imax) or (j < jmax) do
  begin
    if i < imax then
      m := ARepos[i]
    else
      m := nil;
    if j < jmax then
      u := AReposUp[j]
    else
      u := nil;

    if (m <> nil) and (u <> nil) then
    begin
      if u.SyncTo(m) then // look for new update
      begin
        Inc(i);
        Inc(j);
        if m.flag = fUpdate then
          Inc(iupdate);
      end
      else
      begin  // scan remote ARepos till end
        newfound := False;
        for k := j + 1 to jmax - 1 do
        begin
          if m.name = AReposUp[k].name then // j to k-1 is new
          begin
            newfound := True;
            Break;
          end;
        end;
        if newfound then // add new
        begin
          for k := j to k - 1 do
          begin
            m := AReposUp[k].Clone;
            m.flag := fNew;
            ARepos.Add(m);
            Inc(inew);
          end;
          j := k + 1;
        end
        else  // current is marked to delete
        begin
          m.flag := fDelete;
          Inc(iupdate);
          Inc(i);
        end;
      end;
    end
    else
    if m = nil then // new found
    begin
      m := u.Clone;
      m.flag := fNew;
      ARepos.Add(m);
      Inc(inew);
      Inc(j);
    end
    else
    begin // current is marked to delete
      m.flag := fDelete;
      Inc(iupdate);
      Inc(i);
    end;
  end;
  if inew <> 0 then
    ARepos.Items.Sort;
  Result := inew + iupdate <> 0;
end;

procedure TCheckUpdateThread.Download;
var
  i, imax: Integer;
  m: TLuaModuleRepo;
  f, message: String;
begin
  Synchronize(@SyncStartDownload);

  FStatusList.Clear;

  // do delete first
  for i := 0 to FRepos.Items.Count-1 do
  begin
    UpdateProgressBar(i + 1, FRepos.Items.Count);
    UpdateStatusText(RS_DeletingFlaggedModules);
    if FRepos[i].flag = fDelete then
    begin
      m := FRepos[i];
      f := LUA_REPO_FOLDER + TrimFilename(m.Name);
      if FileExists(f) and DeleteFile(f) then
      begin
        AddStatus(Format(RS_StatusDelete, [m.Name]));
        m.flag := fDeleted;
      end;
    end;
  end;

  i := 0;
  imax := FRepos.Items.Count;
  while i < imax do
  begin
    m := FRepos[i];
    if not (m.flag in [fNew, fUpdate, fFailedDownload]) then
      Inc(i)
    else
    begin
      while FThreads.Count >= OptionMaxThreads do
        Sleep(HeartBeatRate);
      if Terminated then
        Break;
      TDownloadThread.Create(Self, FRepos[i]);
      message := FGitHubRepo.GetLastCommitMessage(m.Name);
      if message <> '' then
         m.last_message := message;
      Inc(i);
    end;
  end;

  while FThreads.Count <> 0 do
  begin
    if Terminated then
      Break;
    Sleep(HeartBeatRate);
  end;

  EnterCriticalsection(FThreadsCS);
  try
    for i := 0 to FThreads.Count - 1 do
    begin
      TDownloadThread(FThreads[i]).Terminate;
    end;
  finally
    LeaveCriticalsection(FThreadsCS);
  end;

  while FThreads.Count <> 0 do
    Sleep(HeartBeatRate);
  Synchronize(@SyncFinishDownload);
end;

procedure TCheckUpdateThread.DoSync;
var
  foundupdate: Boolean;
  i, imax: Integer;
  m: TLuaModuleRepo;
  trepos: TLuaModulesRepos;
begin
  FRepos := FOwner.Repos.Clone;

  LoadingProgressBar;
  UpdateStatusText(RS_GitHubConnecting);
  if FGitHubRepo.GetUpdate then
  begin
    FReposUp := TLuaModulesRepos.Create;
    for i := 0 to FGitHubRepo.Tree.Count-1 do
    begin
      FReposUp.Add(FGitHubRepo.Tree[i].path).sha := FGitHubRepo.Tree[i].sha;
    end;
    FReposUp.Sort;
  end;

  if FReposUp = nil then FReposUp := FRepos.Clone;
  if (FReposUp.Count <> 0) and not Terminated then
  begin
    // check
    foundupdate := SyncRepos(FRepos, FReposUp);

    // look for missing local files and previously failed download
    for i := 0 to FRepos.Items.Count - 1 do
    begin
      m := FRepos[i];
      UpdateProgressBar(i + 1, FRepos.Items.Count);
      UpdateStatusText(RS_CheckLocalModules);
      if m.flag = fFailedDownload then
         foundupdate := True
      else
      if (not (m.flag in [fNew, fUpdate])) and
        (not FileExists(LUA_REPO_FOLDER + TrimFilename(m.name))) then
      begin
        m.flag := fFailedDownload;
        if not foundupdate then foundupdate := True;
      end;
      case m.flag of
        fNew: FStatusList.Add(Format(RS_StatusNew, [m.name]));
        fUpdate: FStatusList.Add(Format(RS_StatusUpdate, [m.name]));
        fDelete: FStatusList.Add(Format(RS_StatusDelete, [m.name]));
        fFailedDownload: FStatusList.Add(Format(RS_StatusFailed, [m.name]));
        else;
      end;
    end;

    // get properties
    //if foundupdate and (not Terminated) then
      //LoadReposProps;

    Synchronize(@SyncFinishChecking);

    if foundupdate and (not Terminated) then
    begin
      if OptionModulesUpdaterShowUpdateWarning then
      begin
        LoadingProgressBar;
        UpdateStatusText(RS_AwaitingProceed);
        Synchronize(@SyncAskToProceed)
      end
      else
      begin
        FProceed := True;
        Sleep(1500); // delay to show the update status
      end;
      if FProceed then
      begin
        FDownloadTotalCount := FStatusList.Count;
        Download;
      end;
    end;

    // cleanup
    i := 0;
    imax := FRepos.Items.Count;
    while i < imax do
    begin
      m := FRepos[i];
      if m.flag = fDeleted then
      begin
        FRepos.Items.Delete(i);
        Dec(imax);
      end
      else
      begin
        if m.flag in [fNew, fUpdate, fFailedDownload] then
          m.flag := fFailedDownload
        else
          m.flag := fNone;
        Inc(i);
      end;
    end;
    trepos := FMainRepos;
    FMainRepos := FRepos;
    FRepos := trepos;
  end;
end;

procedure TCheckUpdateThread.Execute;
begin
  try
    Synchronize(@SyncStartChecking);
    DoSync;
    if not Terminated then
      Sleep(1000);
  except
    on E: Exception do
      Logger.SendException('LuaModulesUpdater.Error!', E);
  end;
  Synchronize(@SyncFinal);
end;

constructor TCheckUpdateThread.Create(const AOwner: TLuaModulesUpdaterForm);
begin
  inherited Create(False, MainForm, MainForm.IconList, 24);
  InitCriticalSection(FThreadsCS);
  FOwner := AOwner;
  FThreads := TFPList.Create;
  FDownloadTotalCount := 0;
  FDownloadedCount := 0;
  FStatusList := TStringList.Create;
  FGitHubRepo := TGitHubRepo.Create(CONFIG_FILE, LUA_REPO_WORK_FILE, Self);
end;

destructor TCheckUpdateThread.Destroy;
begin
  if Assigned(FRepos) then
    FRepos.Free;
  if Assigned(FReposUp) then
    FReposUp.Free;
  FStatusList.Free;
  FThreads.Free;
  FGitHubRepo.Free;
  DoneCriticalsection(FThreadsCS);
  inherited Destroy;
end;

procedure TCheckUpdateThread.AddStatus(const S: String);
begin
  EnterCriticalsection(FThreadsCS);
  try
    FStatusList.Add(S);
  finally
    LeaveCriticalsection(FThreadsCS);
  end;
end;

{$R *.lfm}

{ TLuaModulesUpdaterForm }

procedure TLuaModulesUpdaterForm.btCheckUpdateClick(Sender: TObject);
begin
  if ThreadCheck = nil then
    ThreadCheck := TCheckUpdateThread.Create(Self);
end;

procedure TLuaModulesUpdaterForm.FormCreate(Sender: TObject);
begin
  AddVT(vtLuaModulesRepos);
  InitCriticalSection(FListCS);
  Repos := TLuaModulesRepos.Create;
  btCheckUpdate.Caption := RS_CheckUpdate;
  btCheckUpdateTerminate.Visible := False;
  vtLuaModulesRepos.NodeDataSize := SizeOf(TLuaModuleRepo);
  LoadLocalRepos;
end;

procedure TLuaModulesUpdaterForm.FormDestroy(Sender: TObject);
begin
  if ThreadCheck <> nil then
  begin
    ThreadCheck.Terminate;
    ThreadCheck.WaitFor;
  end;
  Repos.Free;
  DoneCriticalsection(FListCS);
  RemoveVT(vtLuaModulesRepos);
end;

procedure TLuaModulesUpdaterForm.btCheckUpdateTerminateClick(Sender: TObject);
begin
  if ThreadCheck <> nil then
    ThreadCheck.Terminate;
end;

procedure TLuaModulesUpdaterForm.tmRepaintListTimer(Sender: TObject);
begin
  if FListDirty then
  begin
    vtLuaModulesRepos.Repaint;
    EnterCriticalsection(FListCS);
    try
      FListDirty := False;
    finally
      LeaveCriticalsection(FListCS);
    end;
  end;
end;

procedure TLuaModulesUpdaterForm.vtLuaModulesReposBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  with PLuaModuleRepo(Sender.GetNodeData(Node))^ do
  begin
    if last_modified > IncDay(Now, -7) then
    begin
      TargetCanvas.Brush.Color := CL_MDNewUpdate;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure TLuaModulesUpdaterForm.vtLuaModulesReposCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  m1, m2: TLuaModuleRepo;
begin
  m1 := PLuaModuleRepo(Sender.GetNodeData(Node1))^;
  m2 := PLuaModuleRepo(Sender.GetNodeData(Node2))^;
  case Column of
    0: Result := AnsiCompareStr(m1.name, m2.name);
    1:
    begin
      if m1.last_modified > m2.last_modified then
        Result := 1
      else
        Result := -1;
    end;
    2: Result := AnsiCompareStr(m1.last_message, m2.last_message);
  end;
end;

procedure TLuaModulesUpdaterForm.vtLuaModulesReposGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);
begin
  with PLuaModuleRepo(Sender.GetNodeData(Node))^ do
  begin
    case Column of
      0: HintText := name;
      1: HintText := DateTimeToStr(last_modified);
      2: HintText := last_message;
    end;
  end;
end;

procedure TLuaModulesUpdaterForm.vtLuaModulesReposGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if Column <> 0 then
    Exit;
  ImageIndex := Integer(PLuaModuleRepo(Sender.GetNodeData(Node))^.flag) - 1;
end;

procedure TLuaModulesUpdaterForm.vtLuaModulesReposGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  xNode: PLuaModuleRepo;
begin
  xNode := Sender.GetNodeData(Node);
  if Assigned(xNode) then
    with xNode^ do
      case Column of
        0: CellText := name;
        1: CellText := DateTimeToStr(last_modified);
        2: CellText := last_message;
      end;
end;

procedure TLuaModulesUpdaterForm.vtLuaModulesReposHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if Sender.SortColumn <> HitInfo.Column then
    Sender.SortColumn := HitInfo.Column
  else
  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;
  SortList;
end;

procedure TLuaModulesUpdaterForm.ListDirty;
begin
  if TryEnterCriticalsection(FListCS) <> 0 then
    try
      FListDirty := True;
    finally
      LeaveCriticalsection(FListCS);
    end;
end;

procedure TLuaModulesUpdaterForm.LoadLocalRepos;
begin
  Repos.LoadFromFile(LUA_REPO_FILE);
  ReinitList(False);
end;

procedure TLuaModulesUpdaterForm.ReinitList(const ASort: Boolean);
var
  i: Integer;
begin
  vtLuaModulesRepos.Clear;
  for i := 0 to Repos.Items.Count - 1 do
    vtLuaModulesRepos.AddChild(nil, Repos[i]);
  if ASort then
    SortList;
end;

procedure TLuaModulesUpdaterForm.SortList;
begin
  with vtLuaModulesRepos do
    Sort(nil, Header.SortColumn, Header.SortDirection, False);
end;

end.

