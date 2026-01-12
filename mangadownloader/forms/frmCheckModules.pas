unit frmCheckModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, WebsiteModules, LuaMangaInfo, LuaMangaCheck, LuaHTTPSend,
  LuaWebsiteModuleHandler, LuaWebsiteModules, uData, LuaUtils, uBaseUnit,
  uDownloadsManager, LuaDownloadTask, httpsendthread, StrUtils,
  frmCustomMessageDlg, StatusBarDownload,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

type
  TTestResult = record
    Success: Boolean;
    Message: string;
    Data: string;
    MangaTitle: string;
    ChapterTitle: string;
  end;

  TModuleScanThread = class;
  TModuleCheckThread = class;

  { TFormCheckModules }
  TFormCheckModules = class(TForm)
    btnCheckIntegrity: TButton;
    btnRefreshModules: TButton;
    btnStopCheck: TButton;
    ImageList1: TImageList;
    lvModules: TListView;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    StatusBar: TStatusBar;
    Memo1: TMemo;
    Splitter1: TSplitter;
    tbWebsitesSelectAll: TToolButton;
    tbWebsitesSelectInverse: TToolButton;
    tbWebsitesSelectNone: TToolButton;
    ToolBar2: TToolBar;
    procedure btnCheckIntegrityClick(Sender: TObject);
    procedure btnRefreshModulesClick(Sender: TObject);
    procedure btnStopCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tbWebsitesSelectAllClick(Sender: TObject);
    procedure tbWebsitesSelectInverseClick(Sender: TObject);
    procedure tbWebsitesSelectNoneClick(Sender: TObject);
    procedure UpdateProgress;
  private
    FIsScanning: Boolean;
    FIsChecking: Boolean;
    FCheckedCount: Integer;
    FSuccessCount: Integer;
    FFailCount: Integer;
    FModulesList: TList;
    FScanThread: TModuleScanThread;
    FCheckThread: TModuleCheckThread;
    procedure InitializeListView;
    procedure ScanLuaModules;
    procedure CheckModuleIntegrity;
    function CalcTotalToCheck: Integer;
    procedure SetModuleSelection(AChecked: Boolean; AInverse: Boolean = False);
    procedure AddModuleToList(const AMangaCheck: TMangaInformation);
    function TestGetInfo(const AMangaCheck: TMangaInformation): TTestResult;
    function TestGetPageNumber(
      const AMangaCheck: TMangaInformation): TTestResult;
    procedure LogMessage(const AMsg: string);
    procedure EnableStopCheck(const AEnable: Boolean);
    procedure ClearModulesList;
    procedure UpdateItemStatus(AItem: TListItem;
      const AStatus, ADetails: string);
    // Thread callbacks
    procedure OnScanComplete(Sender: TObject);
    procedure OnScanProgress(const AMsg: string;
      AModule: TMangaInformation);
    procedure OnCheckComplete(Sender: TObject);
    procedure OnCheckProgress(AIndex: Integer; const AStatus, ADetails,
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

  { TModuleCheckThread }
  TModuleCheckThread = class(TStatusBarDownload)
  private
    FForm: TFormCheckModules;
    FProgressIndex: Integer;
    PCheckedCount: PInteger; // Pointer to the integer
    FProgressStatus: string;
    FProgressDetails: string;
    FProgressMsg: string;
    PSuccessCount: PInteger; // Pointer to the integer
    PFailCount: PInteger; // Pointer to the integer
    procedure SyncProgress;
    procedure SyncComplete;
  protected
    procedure Execute; override;
    procedure CallUpdateProgress; // Wrapper for Synchronize
  public
    constructor Create(AForm: TFormCheckModules);
    procedure LinkVariable(ACheckedCountPtr, ASuccessCountPtr,
      AFailCountPtr: PInteger);
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
    FForm.OnScanProgress(FProgressMsg, FProgressModule);
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
      if Terminated then Break;

      with Modules.List[i] do
      begin
        if not Assigned(OnCheckSite) then
          Continue;

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
                MangaURL := MaybeFillHost(RootURL, MangaURL);
                Inc(AMangaCheck.MangaCheck.TestToCheck);
                if MangaTitle <> '' then
                begin
                  Inc(AMangaCheck.MangaCheck.TestToCheck);
                end;
              end;
              if ChapterURL <> '' then
              begin
                ChapterURL := MaybeFillHost(RootURL, ChapterURL);
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
              AMangaCheck.Free;
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
  end;
end;

{ TModuleCheckThread }

constructor TModuleCheckThread.Create(AForm: TFormCheckModules);
begin
  inherited Create(False, frmMain.MainForm, AForm.ImageList1, 16);
  FForm := AForm;
  FreeOnTerminate := True;
end;

procedure TModuleCheckThread.SyncProgress;
var
  i, TotalToCheck: Integer;
  Item: TListItem;
  AMangaCheck: TMangaInformation;
begin
  TotalToCheck := 0;
  for i := 0 to FForm.lvModules.Items.Count - 1 do
  begin
      Item := FForm.lvModules.Items[i];
      if not Item.Checked then Continue;

      AMangaCheck := TMangaInformation(Item.Data);
      TotalToCheck := TotalToCheck + AMangaCheck.MangaCheck.TestToCheck;
  end;

  FForm.OnCheckProgress(FProgressIndex, FProgressStatus, FProgressDetails,
  FProgressMsg);
  UpdateProgressBar(PCheckedCount^ ,TotalToCheck)
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

procedure TModuleCheckThread.CallUpdateProgress;
begin
  FForm.UpdateProgress;
end;

procedure TModuleCheckThread.Execute;
var
  i: Integer;
  Item: TListItem;
  AMangaCheck: TMangaInformation;
  GetInfoResult, GetPageResult: TTestResult;
  Details, ExtraDetails: string;
  AllTestsPassed: Boolean;
begin
  try
    FProgressMsg := '=== Starting Integrity Check ===';
    PCheckedCount^ := 0;
    FProgressIndex := -1;
    PSuccessCount^ := 0;
    PFailCount^ := 0;
    Synchronize(@SyncProgress);

    for i := 0 to FForm.lvModules.Items.Count - 1 do
    begin
      if Terminated then Break;

      Item := FForm.lvModules.Items[i];
      if not Item.Checked then Continue;

      AMangaCheck := TMangaInformation(Item.Data);
      if AMangaCheck = nil then Continue;

      Details := '';
      ExtraDetails := '';
      AllTestsPassed := True;
      FProgressIndex := i;

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
          FProgressIndex := -1;
          Synchronize(@SyncProgress);
          FProgressIndex := i;

          GetInfoResult := FForm.TestGetInfo(AMangaCheck);

          if GetInfoResult.Success then
          begin
            Details := 'OnGetInfo: PASS';
            Inc(PSuccessCount^);
            Inc(PCheckedCount^);
            Queue(@CallUpdateProgress);
            FProgressMsg := '  OnGetInfo: PASS - ' + GetInfoResult.Message;
            FProgressIndex := -1;
            Synchronize(@SyncProgress);
            FProgressIndex := i;

            // Validate manga title if provided
            if AMangaCheck.MangaCheck.MangaTitle <> '' then
            begin
              FProgressMsg := '  Testing Manga Title with: ' +
                AMangaCheck.MangaCheck.MangaTitle;
              FProgressIndex := -1;
              Synchronize(@SyncProgress);
              FProgressIndex := i;

              if AMangaCheck.MangaCheck.MangaTitle
              = GetInfoResult.MangaTitle then
              begin
                Inc(PSuccessCount^);
                Inc(PCheckedCount^);
                Queue(@CallUpdateProgress);
                ExtraDetails := ' | Extra Test Check Manga Title: PASS';
                FProgressMsg := '  Check Manga Title: PASS';
              end
              else
              begin
                Inc(PFailCount^);
                Inc(PCheckedCount^);
                Queue(@CallUpdateProgress);
                ExtraDetails := ' | Extra Test Check Manga Title: FAIL';
                FProgressMsg := Format('  Check Manga Title: FAIL -'
                + ' Returned "%s" vs Expected "%s"',
                  [GetInfoResult.MangaTitle,
                  AMangaCheck.MangaCheck.MangaTitle]);
              end;
              FProgressIndex := -1;
              Synchronize(@SyncProgress);
              FProgressIndex := i;
            end;

            // Validate chapter title if provided
            if (AMangaCheck.MangaCheck.ChapterURL <> '') and
               (AMangaCheck.MangaCheck.ChapterTitle <> '') then
            begin
              FProgressMsg := '  Testing Chapter Title with: ' +
                AMangaCheck.MangaCheck.ChapterTitle;
              FProgressIndex := -1;
              Synchronize(@SyncProgress);
              FProgressIndex := i;

              if not ContainsStr(GetInfoResult.ChapterTitle,
              'Don''t Match Any Chapter URL') then
              begin
                if AMangaCheck.MangaCheck.ChapterTitle
                = GetInfoResult.ChapterTitle then
                begin
                  Inc(PSuccessCount^);
                  Inc(PCheckedCount^);
                  Queue(@CallUpdateProgress);
                  ExtraDetails := ExtraDetails
                  + ' | Extra Test Check Chapter Title: PASS';
                  FProgressMsg := '  Check Chapter Title: PASS';
                end
                else
                begin
                  Inc(PFailCount^);
                  Inc(PCheckedCount^);
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
                Inc(PFailCount^);
                Inc(PCheckedCount^);
                Queue(@CallUpdateProgress);
                ExtraDetails := ExtraDetails
                + ' | Extra Test Check Chapter Title: FAIL';
                FProgressMsg := '  Check Chapter Title: FAIL - '
                + GetInfoResult.ChapterTitle;
              end;
              FProgressIndex := -1;
              Synchronize(@SyncProgress);
              FProgressIndex := i;
            end
            else if AMangaCheck.MangaCheck.ChapterURL = '' then
            begin
              FProgressMsg := '  No chapter link provided,'
              + ' using first chapter link';
              AMangaCheck.MangaCheck.ChapterURL := GetInfoResult.Data;
              FProgressIndex := -1;
              Inc(AMangaCheck.MangaCheck.TestToCheck);
              Synchronize(@SyncProgress);
              FProgressIndex := i;
            end;
          end
          else
          begin
            Details := 'OnGetInfo: FAIL (' + GetInfoResult.Message + ')';
            Inc(PFailCount^);
            Inc(PCheckedCount^);
            Queue(@CallUpdateProgress);
            AllTestsPassed := False;
            FProgressMsg := '  OnGetInfo: FAIL - ' + GetInfoResult.Message;
            FProgressIndex := -1;
            Synchronize(@SyncProgress);
            FProgressIndex := i;
          end;
        end;

        // Test GetPageNumber if ChapterURL exists
        if AMangaCheck.MangaCheck.ChapterURL <> '' then
        begin
          if Details <> '' then
            Details := Details + ' | ';

          FProgressMsg := '  Testing GetPageNumber with: ' +
            AMangaCheck.MangaCheck.ChapterURL;
          FProgressIndex := -1;
          Synchronize(@SyncProgress);
          FProgressIndex := i;

          GetPageResult := FForm.TestGetPageNumber(AMangaCheck);

          if GetPageResult.Success then
          begin
            Details := Details + 'OnGetPageNumber: PASS';
            Inc(PSuccessCount^);
            Inc(PCheckedCount^);
            Queue(@CallUpdateProgress);
            FProgressMsg := '  OnGetPageNumber: PASS - '
            + GetPageResult.Message;
          end
          else
          begin
            Details := Details + 'OnGetPageNumber: FAIL ('
            + GetPageResult.Message + ')';
            Inc(PFailCount^);
            Inc(PCheckedCount^);
            Queue(@CallUpdateProgress);
            AllTestsPassed := False;
            FProgressMsg := '  OnGetPageNumber: FAIL - '
            + GetPageResult.Message;
          end;
          FProgressIndex := -1;
          Synchronize(@SyncProgress);
          FProgressIndex := i;
        end;

        // Update final status
        FProgressMsg := ''; //clear FProgressMsg befoer Update final status
        if AllTestsPassed then
        begin
          if (ExtraDetails = '') or not ContainsStr(ExtraDetails, 'FAIL') then
            FProgressStatus := 'PASSED'
          else
            FProgressStatus := 'PASSED (FAILED Extra Tests)';
        end
        else
          FProgressStatus := 'FAILED';

        FProgressDetails := Details + ExtraDetails;
        Synchronize(@SyncProgress);

      except
        on E: Exception do
        begin
          FProgressStatus := 'ERROR';
          FProgressDetails := 'Exception: ' + E.Message;
          FProgressMsg := '  ERROR: ' + E.Message;
          Inc(PFailCount^);
          Inc(PCheckedCount^);
          Queue(@CallUpdateProgress);
          Synchronize(@SyncProgress);
        end;
      end;
    end;

    if not Terminated then
    begin
      FProgressMsg := '=== Check Complete ===';
      FProgressIndex := -1;
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

{ TFormCheckModules }

procedure TFormCheckModules.FormCreate(Sender: TObject);
begin
  FIsScanning := False;
  FIsChecking := False;
  FCheckedCount := 0;
  FSuccessCount := 0;
  FFailCount := 0;
  FModulesList := TList.Create;
  FScanThread := nil;
  FCheckThread := nil;
  InitializeListView;
  Memo1.Clear;
  Memo1.ScrollBars := ssVertical;
  Memo1.ReadOnly := True;
end;

procedure TFormCheckModules.InitializeListView;
begin
  lvModules.ViewStyle := vsReport;
  lvModules.RowSelect := True;
  lvModules.GridLines := True;
  lvModules.Columns.Clear;

  with lvModules.Columns.Add do
  begin
    Caption := 'Module Name';
    Width := 180;
    AutoSize := True;
  end;

  with lvModules.Columns.Add do
  begin
    Caption := 'Filename';
    Width := 180;
    AutoSize := True;
  end;

  with lvModules.Columns.Add do
  begin
    Caption := 'CheckSite';
    Width := 80;
  end;

  with lvModules.Columns.Add do
  begin
    Caption := 'CheckChapter';
    Width := 90;
  end;

  with lvModules.Columns.Add do
  begin
    Caption := 'Status';
    Width := 250;
  end;

  with lvModules.Columns.Add do
  begin
    Caption := 'Details';
    Width := 250;
  end;
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
    CanClose := True;
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
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + AMsg);
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
  Memo1.Clear;

  lvModules.Items.BeginUpdate;
  try
    lvModules.Clear;
    ClearModulesList;
  finally
    lvModules.Items.EndUpdate;
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
    AddModuleToList(AModule);
end;

procedure TFormCheckModules.OnScanComplete(Sender: TObject);
begin
  FIsScanning := False;
  FScanThread := nil;
  EnableStopCheck(False);
  StatusBar.SimpleText := Format('Scan complete: %d modules found',
    [lvModules.Items.Count]);
end;

procedure TFormCheckModules.AddModuleToList(const AMangaCheck:
  TMangaInformation);
var
  Item: TListItem;
begin
  Item := lvModules.Items.Add;
  Item.Caption := AMangaCheck.MangaCheck.ModuleName;
  Item.SubItems.Add(AMangaCheck.MangaCheck.ModuleFilename);
  Item.SubItems.Add(IfThen(AMangaCheck.MangaCheck.MangaURL <> '',
    'Yes' + IfThen(AMangaCheck.MangaCheck.MangaTitle <> '', ' with Title',
    ' without Title'), 'No'));
  Item.SubItems.Add(IfThen(AMangaCheck.MangaCheck.ChapterURL <> '',
    'Yes' + IfThen(AMangaCheck.MangaCheck.ChapterTitle <> '', ' with Title',
    ' without Title'), 'No'));
  Item.SubItems.Add('Not Checked');
  Item.SubItems.Add('');
  Item.Checked := True;
  Item.Data := AMangaCheck;
  FModulesList.Add(AMangaCheck);
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
  if not FIsChecking then Exit;

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
  i: Integer;
begin
  for i := 0 to lvModules.Items.Count - 1 do
  begin
    if AInverse then
      lvModules.Items[i].Checked := not lvModules.Items[i].Checked
    else
      lvModules.Items[i].Checked := AChecked;
  end;
end;

procedure TFormCheckModules.UpdateItemStatus(AItem: TListItem; const AStatus,
  ADetails: string);
begin
  AItem.SubItems[3] := AStatus;
  AItem.SubItems[4] := ADetails;
end;

procedure TFormCheckModules.CheckModuleIntegrity;
var
  i: Integer;
begin
  if lvModules.Items.Count = 0 then
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

procedure TFormCheckModules.OnCheckProgress(AIndex: Integer;
  const AStatus, ADetails, AMsg: string);
begin
  if AMsg <> '' then
    LogMessage(AMsg);

  if AIndex >= 0 then
    UpdateItemStatus(lvModules.Items[AIndex], AStatus, ADetails);
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
  i: Integer;
  Item: TListItem;
  AMangaCheck: TMangaInformation;
begin
  Result := 0;
  for i := 0 to lvModules.Items.Count - 1 do
  begin
      Item := lvModules.Items[i];
      if not Item.Checked then Continue;

      AMangaCheck := TMangaInformation(Item.Data);
      Result := Result + AMangaCheck.MangaCheck.TestToCheck;
  end;
end;

function TFormCheckModules.TestGetInfo(const AMangaCheck:
  TMangaInformation): TTestResult;
var
  L: TLuaWebsiteModuleHandler;
  ModuleCheck: TModuleContainer;
  ChapterIndex: Integer;
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
      L := GetLuaWebsiteModuleHandler(ModuleCheck);
      luaPushStringGlobal(L.Handle, 'URL', AMangaCheck.MangaCheck.MangaURL);
      LuaPushNetStatus(L.Handle);

      L.LoadObject('MANGAINFO', AMangaCheck.MangaInfo, @luaMangaInfoAddMetaTable);
      L.LoadObject('HTTP', AMangaCheck.HTTP, @luaHTTPSendThreadAddMetaTable);

      L.CallFunction(OnGetInfo);

      if lua_tointeger(L.Handle, -1) <> 0 then
      begin
        Result.Message := 'Runtime error: ' + lua_tostring(L, -1);
        lua_pop(L, 2);
        Exit;
      end;

      if lua_isboolean(L, -1) and lua_toboolean(L, -1) then
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
        end;

        Result.Success := True;
        Result.Message := 'Success';
        Result.MangaTitle := AMangaCheck.MangaInfo.Title;

        if AMangaCheck.MangaCheck.ChapterURL = '' then
          Result.Data := AMangaCheck.MangaInfo.ChapterLinks[0]
        else
        begin
          ChapterIndex := AMangaCheck.MangaInfo.ChapterLinks.IndexOf(
            AMangaCheck.MangaCheck.ChapterURL);
          if ChapterIndex <> -1 then
            Result.ChapterTitle := AMangaCheck.MangaInfo.ChapterNames[ChapterIndex]
          else
            Result.ChapterTitle := AMangaCheck.MangaCheck.ChapterURL +
              ' Don''t Match Any Chapter URL';
        end;
      end
      else
        Result.Message := 'OnGetInfo returned false or unexpected type';
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
      L := GetLuaWebsiteModuleHandler(ModuleCheck);
      luaPushStringGlobal(L.Handle, 'URL', AMangaCheck.MangaCheck.ChapterURL);

      ATaskContainer := TTaskContainer.Create;

      L.LoadObject('TASK', ATaskContainer, @luaDownloadTaskMetaTable);
      L.LoadObject('HTTP', AMangaCheck.HTTP, @luaHTTPSendThreadAddMetaTable);

      L.CallFunction(OnGetPageNumber);

      if lua_tointeger(L.Handle, -1) <> 0 then
      begin
        Result.Message := 'Runtime error: ' + lua_tostring(L, -1);
        lua_pop(L, 2);
        Exit;
      end;

      if lua_isboolean(L, -1) and lua_toboolean(L, -1) then
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
    ATaskContainer.Free;
end;

end.
