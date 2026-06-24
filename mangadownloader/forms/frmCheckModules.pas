unit frmCheckModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, uCustomControls, WebsiteModules, LuaMangaInfo,
  LuaMangaCheck, LuaHTTPSend, LuaWebsiteModuleHandler, LuaWebsiteModules, uData,
  LuaUtils, uBaseUnit, uDownloadsManager, LuaDownloadTask, httpsendthread,
  StrUtils, frmCustomMessageDlg, frmCustomColor, StatusBarDownload, FMDOptions,
  VirtualTrees,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

type
  TTestResult = record
    Success: Boolean;
    Message: string;
    Data: string;
    MangaTitle: string;
    ChapterTitle: string;
  end;

  // Per-node data stored in vtModules. Status/Details are mutable and get
  // updated live while a check is running (unlike the other MangaCheck
  // fields, which are static once the module is scanned).
  TModuleNodeData = record
    MangaCheck: TMangaInformation;
    Status: String;
    Details: String;
  end;
  PModuleNodeData = ^TModuleNodeData;

  TModuleScanThread = class;
  TModuleCheckThread = class;
  TModuleCheckWorkerThread = class;

  { TFormCheckModules }
  TFormCheckModules = class(TForm)
    btnCheckIntegrity: TToolButton;
    btnRefreshModules: TToolButton;
    btnStopCheck: TToolButton;
    edtFilter: TCustomEditButton;
    imlCheckModules: TImageList;
    vtModules: TVirtualStringTree;
    pnlTop: TPanel;
    pnlFilter: TPanel; // Add this line
    pnlBottom: TPanel;
    StatusBar: TStatusBar;
    memCheckModules: TMemo;
    sptCheckModules: TSplitter;
    tbWebsitesSelectAll: TToolButton;
    tbWebsitesSelectInverse: TToolButton;
    tbWebsitesSelectNone: TToolButton;
    tbCheckModules: TToolBar;
    procedure btnCheckIntegrityClick(Sender: TObject);
    procedure btnRefreshModulesClick(Sender: TObject);
    procedure btnStopCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure vtModulesCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtModulesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtModulesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtModulesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure tbWebsitesSelectAllClick(Sender: TObject);
    procedure tbWebsitesSelectInverseClick(Sender: TObject);
    procedure tbWebsitesSelectNoneClick(Sender: TObject);
    procedure UpdateProgress;
    procedure edtFilterChange(Sender: TObject); // Add this line
    procedure edtFilterButtonClick(Sender: TObject);
    procedure SortvtModules(AColumnIndex: Integer;
      ASortDirection: TSortDirection = sdAscending);
  private
    FIsScanning: Boolean;
    FIsChecking: Boolean;
    FCheckedCount: Integer;
    FSuccessCount: Integer;
    FFailCount: Integer;
    FModulesList: TList;
    FScanThread: TModuleScanThread;
    FCheckThread: TModuleCheckThread;
    FFilterText: string;
    procedure FilterModules;
    procedure InitializeModulesTree;
    procedure ScanLuaModules;
    procedure CheckModuleIntegrity;
    function CalcTotalToCheck: Integer;
    procedure SetModuleSelection(AChecked: Boolean; AInverse: Boolean = False);
    procedure AddModuleToList(const AMangaCheck: TMangaInformation);
    function TestGetInfo(const AMangaCheck: TMangaInformation): TTestResult;
    function TestGetPageNumber(
      const AMangaCheck: TMangaInformation): TTestResult;
    function MaybeFillPrefix(const Prefix, URL: String): String;
    procedure LogMessage(const AMsg: string);
    procedure EnableStopCheck(const AEnable: Boolean);
    procedure ClearModulesList;
    procedure UpdateItemStatus(ANode: PVirtualNode;
      const AStatus, ADetails: string);
    // Thread callbacks
    procedure OnScanComplete(Sender: TObject);
    procedure OnScanProgress(const AMsg: string;
      AModule: TMangaInformation);
    procedure OnCheckComplete(Sender: TObject);
    procedure OnCheckProgress(ANode: PVirtualNode; const AStatus, ADetails,
      AMsg: string);
  public
  end;

  { TModuleScanThread }
  TModuleScanThread = class(TThread)
  private
    FForm: TFormCheckModules;
    FProgressMsg: string;
    FProgressModule: TMangaInformation;
    FModuleCount: Integer;
    procedure SyncProgress;
    procedure SyncComplete;
  protected
    procedure Execute; override;
  public
    constructor Create(AForm: TFormCheckModules);
  end;

  { TModuleCheckQueueItem }
  TModuleCheckQueueItem = record
    Node: PVirtualNode;
    MangaCheck: TMangaInformation;
  end;
  TModuleCheckQueueItems = array of TModuleCheckQueueItem;

  { TModuleCheckThread }
  // Coordinator thread: builds the work queue (one entry per checked module),
  // then spawns up to OptionMaxBackgroundLoadThreads TModuleCheckWorkerThread
  // instances that pull items from the queue and check them in parallel.
  TModuleCheckThread = class(TStatusBarDownload)
  private
    FForm: TFormCheckModules;
    FQueueItems: TModuleCheckQueueItems;
    FQueuePos: LongInt; // next queue slot to hand out, advanced via InterLockedIncrement
    FProgressNode: PVirtualNode;
    FProgressStatus: string;
    FProgressDetails: string;
    FProgressMsg: string;
    PCheckedCount: PInteger; // Pointer to the integer
    PSuccessCount: PInteger; // Pointer to the integer
    PFailCount: PInteger; // Pointer to the integer
    procedure SyncProgress;
    procedure SyncComplete;
    procedure BuildQueue;
    function GetNextQueueItem(out AItem: TModuleCheckQueueItem): Boolean;
    procedure UpdateOverallProgress;
  protected
    procedure Execute; override;
  public
    constructor Create(AForm: TFormCheckModules);
    procedure LinkVariable(ACheckedCountPtr, ASuccessCountPtr,
      AFailCountPtr: PInteger);
  end;

  { TModuleCheckWorkerThread }
  // Worker thread: repeatedly claims the next queued module from its owner
  // (TModuleCheckThread) and runs the integrity tests for it. Multiple
  // workers run concurrently, one per "max number of background load
  // threads" option, each with its own Lua state (threadvar-based) and
  // its own HTTP connection, so they don't interfere with one another.
  TModuleCheckWorkerThread = class(TThread)
  private
    FOwner: TModuleCheckThread;
    FForm: TFormCheckModules;
    FProgressNode: PVirtualNode;
    FProgressStatus: string;
    FProgressDetails: string;
    FProgressMsg: string;
    procedure SyncProgress;
    procedure CallUpdateProgress;
    procedure CheckOneModule(ANode: PVirtualNode;
      const AMangaCheck: TMangaInformation);
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TModuleCheckThread; AForm: TFormCheckModules);
  end;

var
  FormCheckModules: TFormCheckModules;

implementation

uses
  FileUtil, LazUTF8, LazFileUtils, frmMain;

{$R *.lfm}

{ TModuleScanThread }

constructor TModuleScanThread.Create(AForm: TFormCheckModules);
begin
  inherited Create(True);
  FForm := AForm;
  FModuleCount := 0;
  FreeOnTerminate := True;
end;

procedure TModuleScanThread.SyncProgress;
begin
  if Assigned(FProgressModule) then
  begin
    FForm.OnScanProgress(FProgressMsg, FProgressModule);
  end;
end;

procedure TModuleScanThread.SyncComplete;
begin
  FForm.OnScanComplete(Self);
end;

procedure TModuleScanThread.Execute;
var
  i: Integer;
  L: TLuaWebsiteModuleHandler;
  AMangaCheck: TMangaInformation;
begin
  try
    if Modules = nil then
    begin
      FProgressMsg := 'Warning: No modules loaded';
      Synchronize(@SyncProgress);
      Exit;
    end;

    for i := 0 to Modules.Count - 1 do
    begin
      if Terminated then
      begin
        Break;
      end;

      with Modules.List[i] do
      begin
        if not Assigned(OnCheckSite) then
        begin
          Continue
        end;

        AMangaCheck := nil;
        with TLuaWebsiteModule(LuaModule) do
        try
          begin
            L := GetLuaWebsiteModuleHandler(Modules.List[i]);
            LuaPushNetStatus(L.Handle);
            AMangaCheck := TMangaInformation.Create();
            AMangaCheck.MangaCheck.TestToCheck := 0;
            AMangaCheck.MangaCheck.Module := Modules.List[i];
            L.LoadObject('MANGACHECK', AMangaCheck.MangaCheck,
              @luaMangaCheckAddMetaTable);
            L.CallFunction(OnCheckSite);

            with AMangaCheck.MangaCheck do
            begin
              if MangaURL <> '' then
              begin
                if AMangaCheck.MangaCheck.MangaURLAddRootHost then
                begin
                  MangaURL := MaybeFillHost(RootURL, MangaURL);
                end;
                Inc(AMangaCheck.MangaCheck.TestToCheck);
                if MangaTitle <> '' then
                begin
                  Inc(AMangaCheck.MangaCheck.TestToCheck);
                end;
              end;
              if ChapterURL <> '' then
              begin
                if AMangaCheck.MangaCheck.ChapterURLPrefix <> '' then
                begin
                  ChapterURL := FForm.MaybeFillPrefix(
                    AMangaCheck.MangaCheck.ChapterURLPrefix, ChapterURL)
                end
                else
                begin
                  if copy(ChapterURL,1,4) <> 'http' then
                  begin
                    ChapterURL := AppendURLDelimLeft(ChapterURL);
                  end;
                end;
                if AMangaCheck.MangaCheck.ChapterURLAddRootHost then
                begin
                  ChapterURL := MaybeFillHost(RootURL, ChapterURL);
                end;
                Inc(AMangaCheck.MangaCheck.TestToCheck);
                if ChapterTitle <> '' then
                begin
                  Inc(AMangaCheck.MangaCheck.TestToCheck);
                end;
              end;

              if (MangaURL <> '') or (ChapterURL <> '') then
              begin
                AMangaCheck.MangaInfo.URL := MangaURL;
                Inc(FModuleCount);
                FProgressMsg := Format('Found: %s (CheckSite: %s, CheckChapter: %s)',
                  [ModuleName,
                   IfThen(MangaURL <> '', 'Yes' + IfThen(MangaTitle <> '',
                     ' with Title', ' without Title'), 'No'),
                   IfThen(ChapterURL <> '', 'Yes' + IfThen(ChapterTitle <> '',
                     ' with Title', ' without Title'), 'No')]);
                FProgressModule := AMangaCheck;
                Synchronize(@SyncProgress);
              end
              else
                AMangaCheck.Free;
            end;
          end;
        except
          on E: Exception do
          begin
            FProgressMsg := 'Error loading module ' +
              ExtractFileName(Container.FileName) + ': ' + E.Message;
            FProgressModule := nil;
            Synchronize(@SyncProgress);
            if Assigned(AMangaCheck) then
            begin
              AMangaCheck.Free;
            end;
          end;
        end;
      end;
    end;

    if not Terminated then
    begin
      FProgressMsg := Format('Scan complete: Found %d modules with check data',
        [FModuleCount]);
      FProgressModule := nil;
      Synchronize(@SyncProgress);
    end;
  finally
    Synchronize(@SyncComplete);
    FForm.SortvtModules(1);
  end;
end;

{ TModuleCheckThread }

constructor TModuleCheckThread.Create(AForm: TFormCheckModules);
begin
  inherited Create(False, frmMain.MainForm, AForm.imlCheckModules, 16);
  FForm := AForm;
  FQueuePos := 0;
  FreeOnTerminate := True;
end;

procedure TModuleCheckThread.SyncProgress;
begin
  FForm.OnCheckProgress(FProgressNode, FProgressStatus, FProgressDetails,
    FProgressMsg);
  UpdateOverallProgress;
end;

procedure TModuleCheckThread.SyncComplete;
begin
  FForm.OnCheckComplete(Self);
end;

procedure TModuleCheckThread.LinkVariable(ACheckedCountPtr,
  ASuccessCountPtr, AFailCountPtr: PInteger);
begin
  // Store the address
  PCheckedCount := ACheckedCountPtr;
  PSuccessCount := ASuccessCountPtr;
  PFailCount := AFailCountPtr;
end;

// Builds the snapshot of checked modules to test. Runs once, synchronously,
// at the very start of Execute (i.e. before any worker thread exists), so
// it's the only place this thread touches vtModules directly -
// exactly like the previous single-threaded implementation did.
procedure TModuleCheckThread.BuildQueue;
var
  n: Integer;
  Node: PVirtualNode;
  Data: PModuleNodeData;
begin
  n := 0;
  SetLength(FQueueItems, Integer(FForm.vtModules.TotalCount));
  Node := FForm.vtModules.GetFirst();
  while Assigned(Node) do
  begin
    if Node^.CheckState = csCheckedNormal then
    begin
      Data := FForm.vtModules.GetNodeData(Node);
      FQueueItems[n].Node := Node;
      FQueueItems[n].MangaCheck := Data^.MangaCheck;
      Inc(n);
    end;
    Node := FForm.vtModules.GetNext(Node);
  end;
  SetLength(FQueueItems, n);
end;

// Thread-safe, lock-free queue claim: each worker calls this to grab the
// next module to check. Safe to call from any number of worker threads at
// once since FQueuePos is only ever advanced via InterLockedIncrement.
function TModuleCheckThread.GetNextQueueItem(out AItem: TModuleCheckQueueItem
  ): Boolean;
var
  Slot: LongInt;
begin
  Slot := InterLockedIncrement(FQueuePos) - 1;
  if (Slot >= 0) and (Slot < Length(FQueueItems)) then
  begin
    AItem := FQueueItems[Slot];
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

// Recomputes the running totals and repaints the floating progress bar /
// status text owned by this thread (inherited from TStatusBarDownload).
// Always called from the main thread (either directly from this thread's
// own Synchronize, or from a worker thread's Synchronize), so it's safe
// for it to touch vtModules.
procedure TModuleCheckThread.UpdateOverallProgress;
var
  TotalToCheck: Integer;
  Node: PVirtualNode;
  Data: PModuleNodeData;
begin
  TotalToCheck := 0;
  Node := FForm.vtModules.GetFirst();
  while Assigned(Node) do
  begin
    if Node^.CheckState = csCheckedNormal then
    begin
      Data := FForm.vtModules.GetNodeData(Node);
      TotalToCheck := TotalToCheck + Data^.MangaCheck.MangaCheck.TestToCheck;
    end;
    Node := FForm.vtModules.GetNext(Node);
  end;

  UpdateProgressBar(PCheckedCount^, TotalToCheck);
  UpdateStatusText(Format('Checking: %d/%d (Success: %d, Failed: %d)',
  [PCheckedCount^, TotalToCheck, PSuccessCount^, PFailCount^]));
end;

procedure TModuleCheckThread.Execute;
var
  i, MaxThreads, NumToSpawn: Integer;
  Workers: array of TModuleCheckWorkerThread;
begin
  try
    BuildQueue;
    FQueuePos := 0;

    FProgressMsg := '=== Starting Integrity Check ===';
    PCheckedCount^ := 0;
    FProgressNode := nil;
    PSuccessCount^ := 0;
    PFailCount^ := 0;
    Synchronize(@SyncProgress);

    if Length(FQueueItems) = 0 then
    begin
      Exit;
    end;

    // "max number of background load threads" (Options > Connections >
    // Miscellaneous) decides how many modules get checked in parallel.
    MaxThreads := OptionMaxBackgroundLoadThreads;
    if MaxThreads < 1 then
    begin
      MaxThreads := 1;
    end;

    NumToSpawn := MaxThreads;
    if NumToSpawn > Length(FQueueItems) then
    begin
      NumToSpawn := Length(FQueueItems);
    end;

    SetLength(Workers, NumToSpawn);
    for i := 0 to NumToSpawn - 1 do
    begin
      Workers[i] := TModuleCheckWorkerThread.Create(Self, FForm);
    end;

    // Wait for every worker to drain the queue (or to notice that this
    // thread was terminated and stop early). WaitFor pumps this thread's
    // Synchronize queue while it waits, so it won't deadlock against the
    // workers' own Synchronize calls.
    for i := 0 to NumToSpawn - 1 do
    begin
      Workers[i].WaitFor;
      Workers[i].Free;
    end;

    if not Terminated then
    begin
      FProgressMsg := '=== Check Complete ===';
      FProgressNode := nil;
      Synchronize(@SyncProgress);

      FProgressMsg := Format('Total tests passed: %d', [PSuccessCount^]);
      Synchronize(@SyncProgress);

      FProgressMsg := Format('Total tests failed: %d', [PFailCount^]);
      Synchronize(@SyncProgress);
    end;
  finally
    Synchronize(@SyncComplete);
  end;
end;

{ TModuleCheckWorkerThread }

constructor TModuleCheckWorkerThread.Create(AOwner: TModuleCheckThread;
  AForm: TFormCheckModules);
begin
  FOwner := AOwner;
  FForm := AForm;
  inherited Create(False);
end;

procedure TModuleCheckWorkerThread.SyncProgress;
begin
  FForm.OnCheckProgress(FProgressNode, FProgressStatus, FProgressDetails,
    FProgressMsg);
  FOwner.UpdateOverallProgress;
end;

procedure TModuleCheckWorkerThread.CallUpdateProgress;
begin
  FForm.UpdateProgress;
end;

procedure TModuleCheckWorkerThread.CheckOneModule(ANode: PVirtualNode;
  const AMangaCheck: TMangaInformation);
var
  GetInfoResult, GetPageResult: TTestResult;
  Details, ExtraDetails: string;
  AllTestsPassed: Boolean;
begin
  if AMangaCheck = nil then
  begin
    Exit;
  end;

  Details := '';
  ExtraDetails := '';
  AllTestsPassed := True;
  FProgressNode := ANode;

  // Update status to "Checking..."
  FProgressStatus := 'Checking...';
  FProgressDetails := '';
  FProgressMsg := 'Checking module: ' + AMangaCheck.MangaCheck.ModuleName;
  Synchronize(@SyncProgress);

  try
    // Test GetInfo if MangaURL exists
    if AMangaCheck.MangaCheck.MangaURL <> '' then
    begin
      FProgressMsg := '  Testing GetInfo with: '
      + AMangaCheck.MangaCheck.MangaURL;
      FProgressNode := nil;
      Synchronize(@SyncProgress);
      FProgressNode := ANode;

      GetInfoResult := FForm.TestGetInfo(AMangaCheck);

      if GetInfoResult.Success then
      begin
        Details := 'OnGetInfo: PASS';
        InterLockedIncrement(FOwner.PSuccessCount^);
        InterLockedIncrement(FOwner.PCheckedCount^);
        Queue(@CallUpdateProgress);
        FProgressMsg := '  OnGetInfo: PASS - ' + GetInfoResult.Message;
        FProgressNode := nil;
        Synchronize(@SyncProgress);
        FProgressNode := ANode;

        // Validate manga title if provided
        if AMangaCheck.MangaCheck.MangaTitle <> '' then
        begin
          FProgressMsg := '  Testing Manga Title with: ' +
            AMangaCheck.MangaCheck.MangaTitle;
          FProgressNode := nil;
          Synchronize(@SyncProgress);
          FProgressNode := ANode;

          if AMangaCheck.MangaCheck.MangaTitle
          = GetInfoResult.MangaTitle then
          begin
            InterLockedIncrement(FOwner.PSuccessCount^);
            InterLockedIncrement(FOwner.PCheckedCount^);
            Queue(@CallUpdateProgress);
            ExtraDetails := ' | Extra Test Check Manga Title: PASS';
            FProgressMsg := '  Check Manga Title: PASS';
          end
          else
          begin
            InterLockedIncrement(FOwner.PFailCount^);
            InterLockedIncrement(FOwner.PCheckedCount^);
            Queue(@CallUpdateProgress);
            ExtraDetails := ' | Extra Test Check Manga Title: FAIL';
            FProgressMsg := Format('  Check Manga Title: FAIL -'
            + ' Returned "%s" vs Expected "%s"',
              [GetInfoResult.MangaTitle,
              AMangaCheck.MangaCheck.MangaTitle]);
          end;
          FProgressNode := nil;
          Synchronize(@SyncProgress);
          FProgressNode := ANode;
        end;

        // Validate chapter title if provided
        if (AMangaCheck.MangaCheck.ChapterURL <> '') and
           (AMangaCheck.MangaCheck.ChapterTitle <> '') then
        begin
          FProgressMsg := '  Testing Chapter Title with: ' +
            AMangaCheck.MangaCheck.ChapterTitle;
          FProgressNode := nil;
          Synchronize(@SyncProgress);
          FProgressNode := ANode;

          if not ContainsStr(GetInfoResult.ChapterTitle,
          'Don''t Match Any Chapter URL') then
          begin
            if AMangaCheck.MangaCheck.ChapterTitle
            = GetInfoResult.ChapterTitle then
            begin
              InterLockedIncrement(FOwner.PSuccessCount^);
              InterLockedIncrement(FOwner.PCheckedCount^);
              Queue(@CallUpdateProgress);
              ExtraDetails := ExtraDetails
              + ' | Extra Test Check Chapter Title: PASS';
              FProgressMsg := '  Check Chapter Title: PASS';
            end
            else
            begin
              InterLockedIncrement(FOwner.PFailCount^);
              InterLockedIncrement(FOwner.PCheckedCount^);
              Queue(@CallUpdateProgress);
              ExtraDetails := ExtraDetails +
              ' | Extra Test Check Chapter Title: FAIL';
              FProgressMsg := Format('  Check Chapter Title: FAIL -'
              + ' Returned "%s" vs Expected "%s"',
                [GetInfoResult.ChapterTitle,
                AMangaCheck.MangaCheck.ChapterTitle]);
            end;
          end
          else
          begin
            InterLockedIncrement(FOwner.PFailCount^);
            InterLockedIncrement(FOwner.PCheckedCount^);
            Queue(@CallUpdateProgress);
            ExtraDetails := ExtraDetails
            + ' | Extra Test Check Chapter Title: FAIL';
            FProgressMsg := '  Check Chapter Title: FAIL - '
            + GetInfoResult.ChapterTitle;
          end;
          FProgressNode := nil;
          Synchronize(@SyncProgress);
          FProgressNode := ANode;
        end
        else if AMangaCheck.MangaCheck.ChapterURL = '' then
        begin
          FProgressMsg := '  No chapter link provided,'
          + ' using first chapter link';
          AMangaCheck.MangaCheck.ChapterURL := GetInfoResult.Data;
          FProgressNode := nil;
          Inc(AMangaCheck.MangaCheck.TestToCheck);
          Synchronize(@SyncProgress);
          FProgressNode := ANode;
        end;
      end
      else
      begin
        Details := 'OnGetInfo: FAIL (' + GetInfoResult.Message + ')';
        InterLockedIncrement(FOwner.PFailCount^);
        InterLockedIncrement(FOwner.PCheckedCount^);
        Queue(@CallUpdateProgress);
        AllTestsPassed := False;
        FProgressMsg := '  OnGetInfo: FAIL - ' + GetInfoResult.Message;
        FProgressNode := nil;
        Synchronize(@SyncProgress);
        FProgressNode := ANode;
      end;
    end;

    // Test GetPageNumber if ChapterURL exists
    if AMangaCheck.MangaCheck.ChapterURL <> '' then
    begin
      if Details <> '' then
        Details := Details + ' | ';

      FProgressMsg := '  Testing GetPageNumber with: ' +
        AMangaCheck.MangaCheck.ChapterURL;
      FProgressNode := nil;
      Synchronize(@SyncProgress);
      FProgressNode := ANode;

      GetPageResult := FForm.TestGetPageNumber(AMangaCheck);

      if GetPageResult.Success then
      begin
        Details := Details + 'OnGetPageNumber: PASS';
        InterLockedIncrement(FOwner.PSuccessCount^);
        InterLockedIncrement(FOwner.PCheckedCount^);
        Queue(@CallUpdateProgress);
        FProgressMsg := '  OnGetPageNumber: PASS - '
        + GetPageResult.Message;
      end
      else
      begin
        Details := Details + 'OnGetPageNumber: FAIL ('
        + GetPageResult.Message + ')';
        InterLockedIncrement(FOwner.PFailCount^);
        InterLockedIncrement(FOwner.PCheckedCount^);
        Queue(@CallUpdateProgress);
        AllTestsPassed := False;
        FProgressMsg := '  OnGetPageNumber: FAIL - '
        + GetPageResult.Message;
      end;
      FProgressNode := nil;
      Synchronize(@SyncProgress);
      FProgressNode := ANode;
    end;

    // Update final status
    FProgressMsg := ''; //clear FProgressMsg befoer Update final status
    if AllTestsPassed then
    begin
      if (ExtraDetails = '') or not ContainsStr(ExtraDetails, 'FAIL') then
      begin
        FProgressStatus := 'PASSED'
      end
      else
      begin
        FProgressStatus := 'PASSED (FAILED Extra Tests)';
      end;
    end
    else
    begin
      FProgressStatus := 'FAILED';
    end;

    FProgressDetails := Details + ExtraDetails;
    Synchronize(@SyncProgress);

  except
    on E: Exception do
    begin
      FProgressStatus := 'ERROR';
      FProgressDetails := 'Exception: ' + E.Message;
      FProgressMsg := '  ERROR: ' + E.Message;
      InterLockedIncrement(FOwner.PFailCount^);
      InterLockedIncrement(FOwner.PCheckedCount^);
      Queue(@CallUpdateProgress);
      Synchronize(@SyncProgress);
    end;
  end;
end;

procedure TModuleCheckWorkerThread.Execute;
var
  QItem: TModuleCheckQueueItem;
begin
  while (not Terminated) and (not FOwner.Terminated) do
  begin
    if not FOwner.GetNextQueueItem(QItem) then
    begin
      Break;
    end;

    CheckOneModule(QItem.Node, QItem.MangaCheck);
  end;
end;


{ TFormCheckModules }

procedure TFormCheckModules.FormCreate(Sender: TObject);
begin
  FIsScanning := False;
  FIsChecking := False;
  FCheckedCount := 0;
  FSuccessCount := 0;
  FFailCount := 0;
  FFilterText := ''; // Add this line
  FModulesList := TList.Create;
  FScanThread := nil;
  FCheckThread := nil;
  AddVT(vtModules);
  InitializeModulesTree;
  memCheckModules.Clear;
  memCheckModules.ScrollBars := ssVertical;
  memCheckModules.ReadOnly := True;
end;

procedure TFormCheckModules.InitializeModulesTree;
begin
  vtModules.NodeDataSize := SizeOf(TModuleNodeData);
end;

procedure TFormCheckModules.FormShow(Sender: TObject);
begin
  // User should explicitly click refresh
end;

procedure TFormCheckModules.FormDestroy(Sender: TObject);
begin
  // Terminate threads if running
  if Assigned(FScanThread) then
  begin
    FScanThread.Terminate;
    FScanThread.WaitFor;
  end;
  if Assigned(FCheckThread) then
  begin
    FCheckThread.Terminate;
    FCheckThread.WaitFor;
  end;

  ClearModulesList;
  FModulesList.Free;
  RemoveVT(vtModules);
end;

procedure TFormCheckModules.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FIsScanning or FIsChecking then
  begin
    CanClose := CenteredMessageDlg(frmMain.MainForm,
      'An operation is in progress. Are you sure you want to close?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;

    if CanClose then
    begin
      if Assigned(FScanThread) then
        FScanThread.Terminate;
      if Assigned(FCheckThread) then
        FCheckThread.Terminate;
    end;
  end
  else
  begin
    CanClose := True;
  end;
end;

procedure TFormCheckModules.vtModulesHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if not (HitInfo.Column in [1, 2, 5]) then
  begin
    Exit;
  end;

  if Sender.SortColumn <> HitInfo.Column then
  begin
    Sender.SortColumn := HitInfo.Column;
    Sender.SortDirection := sdAscending;
  end
  else
  begin
    if Sender.SortDirection = sdAscending then
    begin
      Sender.SortDirection := sdDescending;
    end
    else
    begin
      Sender.SortDirection := sdAscending;
    end;
  end;
  vtModules.Sort(nil, Sender.SortColumn, Sender.SortDirection, False);
end;

procedure TFormCheckModules.vtModulesCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PModuleNodeData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    1: Result := CompareText(Data1^.MangaCheck.MangaCheck.ModuleName,
      Data2^.MangaCheck.MangaCheck.ModuleName);
    2: Result := CompareText(Data1^.MangaCheck.MangaCheck.ModuleFilename,
      Data2^.MangaCheck.MangaCheck.ModuleFilename);
    5: Result := CompareText(Data1^.Status, Data2^.Status);
    else Result := 0;
  end;
end;

procedure TFormCheckModules.vtModulesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PModuleNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    Finalize(Data^);
  end;
end;

procedure TFormCheckModules.vtModulesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PModuleNodeData;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    1: CellText := Data^.MangaCheck.MangaCheck.ModuleName;
    2: CellText := Data^.MangaCheck.MangaCheck.ModuleFilename;
    3: CellText := IfThen(Data^.MangaCheck.MangaCheck.MangaURL <> '',
      'Yes' + IfThen(Data^.MangaCheck.MangaCheck.MangaTitle <> '',
      ' with Title', ' without Title'), 'No');
    4: CellText := IfThen(Data^.MangaCheck.MangaCheck.ChapterURL <> '',
      'Yes' + IfThen(Data^.MangaCheck.MangaCheck.ChapterTitle <> '',
      ' with Title', ' without Title'), 'No');
    5: CellText := Data^.Status;
    6: CellText := Data^.Details;
  end;
end;

procedure TFormCheckModules.tbWebsitesSelectAllClick(Sender: TObject);
begin
  SetModuleSelection(True);
end;

procedure TFormCheckModules.tbWebsitesSelectInverseClick(Sender: TObject);
begin
  SetModuleSelection(False, True);
end;

procedure TFormCheckModules.tbWebsitesSelectNoneClick(Sender: TObject);
begin
  SetModuleSelection(False);
end;

procedure TFormCheckModules.ClearModulesList;
var
  i: Integer;
begin
  for i := 0 to FModulesList.Count - 1 do
    TMangaInformation(FModulesList[i]).Free;
  FModulesList.Clear;
end;

procedure TFormCheckModules.LogMessage(const AMsg: string);
begin
  memCheckModules.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + AMsg);
end;

procedure TFormCheckModules.EnableStopCheck(const AEnable: Boolean);
begin
  if AEnable then
  begin
    btnStopCheck.Enabled := True;
    btnRefreshModules.Enabled := False;
    btnCheckIntegrity.Enabled := False;
    tbWebsitesSelectAll.Enabled := False;
    tbWebsitesSelectInverse.Enabled := False;
    tbWebsitesSelectNone.Enabled := False;
  end
  else
  begin
    btnStopCheck.Enabled := False;
    btnRefreshModules.Enabled := True;
    btnCheckIntegrity.Enabled := True;
    tbWebsitesSelectAll.Enabled := True;
    tbWebsitesSelectInverse.Enabled := True;
    tbWebsitesSelectNone.Enabled := True;
  end;
end;

procedure TFormCheckModules.ScanLuaModules;
begin
  if FIsScanning then
  begin
    CenteredMessageDlg(frmMain.MainForm, 'A scan is already in progress.',
      mtInformation, [mbOK], 0);
    Exit;
  end;

  FIsScanning := True;
  EnableStopCheck(True);
  memCheckModules.Clear;

  vtModules.BeginUpdate;
  try
    vtModules.Clear;
    ClearModulesList;
  finally
    vtModules.EndUpdate;
  end;

  LogMessage('Scanning loaded modules...');

  FScanThread := TModuleScanThread.Create(Self);
  FScanThread.Start;
end;

procedure TFormCheckModules.OnScanProgress(const AMsg: string;
  AModule: TMangaInformation);
begin
  LogMessage(AMsg);
  if Assigned(AModule) then
  begin
    AddModuleToList(AModule);
  end;
end;

procedure TFormCheckModules.OnScanComplete(Sender: TObject);
begin
  FIsScanning := False;
  FScanThread := nil;
  EnableStopCheck(False);
  StatusBar.SimpleText := Format('Scan complete: %d modules found',
    [vtModules.TotalCount]);
end;

procedure TFormCheckModules.AddModuleToList(const AMangaCheck:
  TMangaInformation);
var
  Node: PVirtualNode;
  Data: PModuleNodeData;
begin
  Node := vtModules.AddChild(nil);
  Node^.CheckType := ctCheckBox;
  Node^.CheckState := csCheckedNormal;
  Data := vtModules.GetNodeData(Node);
  Data^.MangaCheck := AMangaCheck;
  Data^.Status := 'Not Checked';
  Data^.Details := '';
  FModulesList.Add(AMangaCheck);
  FilterModules; // Refilter to show the new module if it matches
end;

procedure TFormCheckModules.btnRefreshModulesClick(Sender: TObject);
begin
  ScanLuaModules;
end;

procedure TFormCheckModules.btnCheckIntegrityClick(Sender: TObject);
begin
  CheckModuleIntegrity;
end;

procedure TFormCheckModules.btnStopCheckClick(Sender: TObject);
begin
  if not FIsChecking then
  begin
    Exit;
  end;

  LogMessage('Stop requested, waiting for active checks to complete...');
  if Assigned(FScanThread) then
  begin
    FScanThread.Terminate;
    FScanThread.WaitFor;
  end;
  if Assigned(FCheckThread) then
  begin
    FCheckThread.Terminate;
    FCheckThread.WaitFor;
  end;
end;

procedure TFormCheckModules.SetModuleSelection(AChecked: Boolean;
  AInverse: Boolean = False);
var
  Node: PVirtualNode;
begin
  Node := vtModules.GetFirst();
  while Assigned(Node) do
  begin
    if AInverse then
    begin
      if Node^.CheckState = csCheckedNormal then
      begin
        Node^.CheckState := csUncheckedNormal
      end
      else
      begin
        Node^.CheckState := csCheckedNormal;
      end;
    end
    else
    begin
      if AChecked then
      begin
        Node^.CheckState := csCheckedNormal
      end
      else
      begin
        Node^.CheckState := csUncheckedNormal;
      end;
    end;
    Node := vtModules.GetNext(Node);
  end;
  vtModules.Invalidate;
end;

procedure TFormCheckModules.UpdateItemStatus(ANode: PVirtualNode;
  const AStatus, ADetails: string);
var
  Data: PModuleNodeData;
begin
  Data := vtModules.GetNodeData(ANode);
  Data^.Status := AStatus;
  Data^.Details := ADetails;
  vtModules.InvalidateNode(ANode);
end;

procedure TFormCheckModules.CheckModuleIntegrity;
begin
  if vtModules.TotalCount = 0 then
  begin
    CenteredMessageDlg(frmMain.MainForm, 'No modules to check.'
    + ' Please refresh the module list first.',
      mtError, [mbOK], 0);
    Exit;
  end;

  if FIsChecking then
  begin
    CenteredMessageDlg(frmMain.MainForm, 'An integrity check is already in progress.',
      mtInformation, [mbOK], 0);
    Exit;
  end;
  if CalcTotalToCheck = 0 then
  begin
    CenteredMessageDlg(frmMain.MainForm,
      'No modules selected. Please check at least one module.',
      mtError, [mbOK], 0);
    FIsChecking := False;
    Exit;
  end;

  FIsChecking := True;
  EnableStopCheck(True);

  FCheckThread := TModuleCheckThread.Create(Self);
  FCheckThread.LinkVariable(@Self.FCheckedCount,
  @Self.FSuccessCount, @Self.FFailCount);
  FCheckThread.Start;
end;

procedure TFormCheckModules.OnCheckProgress(ANode: PVirtualNode;
  const AStatus, ADetails, AMsg: string);
begin
  if AMsg <> '' then
  begin
    LogMessage(AMsg);
  end;

  if Assigned(ANode) then
  begin
    UpdateItemStatus(ANode, AStatus, ADetails);
  end;
end;

procedure TFormCheckModules.UpdateProgress;
begin
  // Progress bar is handled by TStatusBarDownload base class
  StatusBar.SimpleText := Format('Checking: %d/%d (Success: %d, Failed: %d)',
  [FCheckedCount, CalcTotalToCheck, FSuccessCount, FFailCount]);
end;

procedure TFormCheckModules.OnCheckComplete(Sender: TObject);
begin
  FIsChecking := False;
  FCheckThread := nil;

  EnableStopCheck(False);

  StatusBar.SimpleText := Format('Check complete: %d tests passed, %d tests failed',
    [FSuccessCount, FFailCount]);

  CenteredMessageDlg(frmMain.MainForm, Format('Integrity check complete.'#13#10 +
    'Tests Passed: %d'#13#10 +
    'Tests Failed: %d', [FSuccessCount, FFailCount]),
    mtInformation, [mbOK], 0);
end;

function TFormCheckModules.CalcTotalToCheck: Integer;
var
  Node: PVirtualNode;
  Data: PModuleNodeData;
begin
  Result := 0;
  Node := vtModules.GetFirst();
  while Assigned(Node) do
  begin
    if Node^.CheckState = csCheckedNormal then
    begin
      Data := vtModules.GetNodeData(Node);
      Result := Result + Data^.MangaCheck.MangaCheck.TestToCheck;
    end;
    Node := vtModules.GetNext(Node);
  end;
end;

function TFormCheckModules.TestGetInfo(const AMangaCheck:
  TMangaInformation): TTestResult;
var
  L: TLuaWebsiteModuleHandler;
  ModuleCheck: TModuleContainer;
  ChapterIndex, i: Integer;
  http: THTTPSendThread;
begin
  Result.Success := False;
  Result.Message := 'Unknown error';
  Result.Data := '';
  Result.MangaTitle := '';
  Result.ChapterTitle := '';

  ModuleCheck := Modules.LocateModule(AMangaCheck.MangaCheck.ModuleID);
  if ModuleCheck = nil then
  begin
    Result.Message := 'Module not found in loaded modules';
    Exit;
  end;

  try
    with TLuaWebsiteModule(ModuleCheck.LuaModule) do
    begin
      http := ModuleCheck.CreateHTTP();
      L := GetLuaWebsiteModuleHandler(ModuleCheck);
      luaPushStringGlobal(L.Handle, 'URL', AMangaCheck.MangaCheck.MangaURL);
      LuaPushNetStatus(L.Handle);

      L.LoadObject('MANGAINFO', AMangaCheck.MangaInfo, @luaMangaInfoAddMetaTable);
      L.LoadObject('HTTP', http, @luaHTTPSendThreadAddMetaTable);

      L.CallFunction(OnGetInfo);

      if lua_tointeger(L.Handle, -1) <> 0 then
      begin
        Result.Message := 'Runtime error: ' + lua_tostring(L, -1);
        lua_pop(L, 2);
        Exit;
      end;

      if lua_toboolean(L, -1) then
      begin
        if AMangaCheck.MangaInfo.Title = '' then
        begin
          Result.Message := 'OnGetInfo returned true, but no title found';
          Exit;
        end;
        if AMangaCheck.MangaInfo.ChapterLinks.Count = 0 then
        begin
          Result.Message := 'OnGetInfo returned true, but no chapters found';
          Exit;
        end
        else
        begin
          if AMangaCheck.MangaCheck.ChapterURLPrefix <> '' then
          begin
            for i := 0 to AMangaCheck.MangaInfo.ChapterLinks.Count - 1 do
            begin
              AMangaCheck.MangaInfo.ChapterLinks[i] :=
                MaybeFillPrefix(AMangaCheck.MangaCheck.ChapterURLPrefix,
                AMangaCheck.MangaInfo.ChapterLinks[i])
            end;
          end
          else
          begin
            for i := 0 to AMangaCheck.MangaInfo.ChapterLinks.Count - 1 do
            begin
              if copy(AMangaCheck.MangaInfo.ChapterLinks[i],1,4) <> 'http' then
              begin
                AMangaCheck.MangaInfo.ChapterLinks[i] := AppendURLDelimLeft(
                  AMangaCheck.MangaInfo.ChapterLinks[i]);
              end;
            end;
          end;
          if AMangaCheck.MangaCheck.ChapterURLAddRootHost then
          begin
            for i := 0 to AMangaCheck.MangaInfo.ChapterLinks.Count - 1 do
            begin
              AMangaCheck.MangaInfo.ChapterLinks[i] :=
                MaybeFillHost(GetHostURL(AMangaCheck.MangaInfo.URL),
                AMangaCheck.MangaInfo.ChapterLinks[i]);
            end;
          end;
        end;

        Result.Success := True;
        Result.Message := 'Success';
        Result.MangaTitle := AMangaCheck.MangaInfo.Title;

        if AMangaCheck.MangaCheck.ChapterURL = '' then
        begin
          Result.Data := AMangaCheck.MangaInfo.ChapterLinks[0]
        end
        else
        begin
          ChapterIndex := AMangaCheck.MangaInfo.ChapterLinks.IndexOf(
            AMangaCheck.MangaCheck.ChapterURL);
          if ChapterIndex <> -1 then
          begin
            Result.ChapterTitle := AMangaCheck.MangaInfo.ChapterNames[ChapterIndex]
          end
          else
          begin
            Result.ChapterTitle := AMangaCheck.MangaCheck.ChapterURL +
              ' Don''t Match Any Chapter URL';
          end;
        end;
      end
      else
      begin
        Result.Message := 'OnGetInfo returned false or unexpected type';
      end;
    end;
  except
    on E: Exception do
      Result.Message := 'Exception: ' + E.Message;
  end;
end;

function TFormCheckModules.TestGetPageNumber(const AMangaCheck:
  TMangaInformation): TTestResult;
var
  L: TLuaWebsiteModuleHandler;
  ModuleCheck: TModuleContainer;
  ATaskContainer: TTaskContainer;
  http: THTTPSendThread;
begin
  Result.Success := False;
  Result.Message := 'Unknown error';

  ModuleCheck := Modules.LocateModule(AMangaCheck.MangaCheck.ModuleID);
  if ModuleCheck = nil then
  begin
    Result.Message := 'Module not found in loaded modules';
    Exit;
  end;

  ATaskContainer := nil;
  try
    with TLuaWebsiteModule(ModuleCheck.LuaModule) do
    begin
      http := ModuleCheck.CreateHTTP();
      L := GetLuaWebsiteModuleHandler(ModuleCheck);
      luaPushStringGlobal(L.Handle, 'URL', AMangaCheck.MangaCheck.ChapterURL);

      ATaskContainer := TTaskContainer.Create;

      L.LoadObject('TASK', ATaskContainer, @luaDownloadTaskMetaTable);
      L.LoadObject('HTTP', http, @luaHTTPSendThreadAddMetaTable);

      L.CallFunction(OnGetPageNumber);

      if lua_tointeger(L.Handle, -1) <> 0 then
      begin
        Result.Message := 'Runtime error: ' + lua_tostring(L, -1);
        lua_pop(L, 2);
        Exit;
      end;

      //if lua_isboolean(L, -1) and lua_toboolean(L, -1) then
      if lua_toboolean(L, -1) then
      begin
        if ATaskContainer.PageLinks.Count = 0 then
        begin
          Result.Message := 'OnGetPageNumber returned true, but no pages found';
          Exit;
        end;

        Result.Success := True;
        Result.Message := 'Success';
      end
      else
        Result.Message := 'OnGetPageNumber returned false or unexpected type';
    end;
  except
    on E: Exception do
      Result.Message := 'Exception: ' + E.Message;
  end;

  if Assigned(ATaskContainer) then
  begin
    ATaskContainer.Free;
  end;
end;

function TFormCheckModules.MaybeFillPrefix(const Prefix, URL: String): String;
var
  UrlHost, UrlPath: string;
begin
  UrlHost := GetHostURL(URL);
  UrlPath := RemoveHostFromURL(URL);
  if ContainsStr(UrlPath, Prefix) then
  begin
    Result := UrlHost + UrlPath;
  end
  else
  begin
    Result := UrlHost + Prefix + UrlPath;
  end;
end;

procedure TFormCheckModules.FilterModules;
begin
  if FModulesList.Count = 0 then
  begin
    Exit;
  end;

  FilterVST(vtModules, FFilterText, 1);

  StatusBar.SimpleText := Format('Showing %d of %d modules',
    [vtModules.VisibleCount, FModulesList.Count]);
end;

// Add event handler for the filter edit box:
procedure TFormCheckModules.edtFilterChange(Sender: TObject);
begin
  FFilterText := TEdit(Sender).Text;
  FilterModules;
end;

procedure TFormCheckModules.edtFilterButtonClick(Sender: TObject);
begin
  edtFilter.Clear;
end;

procedure TFormCheckModules.SortvtModules(AColumnIndex: Integer;
  ASortDirection: TSortDirection);
begin
  if AColumnIndex in [1, 2, 5] then
  begin
    vtModules.Header.SortColumn := AColumnIndex;
    vtModules.Header.SortDirection := ASortDirection;
    vtModules.Sort(nil, AColumnIndex, ASortDirection, False);
  end;
end;
end.
