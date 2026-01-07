unit frmCheckModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, WebsiteModules, LuaMangaInfo, LuaMangaCheck, LuaHTTPSend,
  LuaWebsiteModuleHandler, LuaWebsiteModules, uData, LuaUtils, uBaseUnit,
  uDownloadsManager, LuaDownloadTask, httpsendthread, StrUtils,
  frmCustomMessageDlg,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

type
  TTestResult = record
    Success: Boolean;
    Message: string;
    Data: string;
    MangaTitle: string;
    ChapterTitle: string;
  end;

  { TFormCheckModules }
  TFormCheckModules = class(TForm)
    btnCheckIntegrity: TButton;
    btnRefreshModules: TButton;
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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbWebsitesSelectAllClick(Sender: TObject);
    procedure tbWebsitesSelectInverseClick(Sender: TObject);
    procedure tbWebsitesSelectNoneClick(Sender: TObject);
  private
    FIsScanning: Boolean;
    FModulesList: TList;
    procedure InitializeListView;
    procedure ScanLuaModules;
    procedure CheckModuleIntegrity;
    procedure SetModuleSelection(AChecked: Boolean; AInverse: Boolean = False);
    procedure AddModuleToList(const AMangaCheck: TMangaInformation);
    function TestGetInfo(const AMangaCheck: TMangaInformation): TTestResult;
    function TestGetPageNumber(const AMangaCheck:
      TMangaInformation): TTestResult;
    procedure LogMessage(const AMsg: string); inline;
    procedure ClearModulesList;
    procedure UpdateItemStatus(AItem: TListItem; const AStatus,
      ADetails: string);
  public
  end;

var
  FormCheckModules: TFormCheckModules;

implementation

uses
  FileUtil, LazUTF8, LazFileUtils;

{$R *.lfm}

{ TFormCheckModules }

procedure TFormCheckModules.FormCreate(Sender: TObject);
begin
  FIsScanning := False;
  FModulesList := TList.Create;
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
  // Removed auto-scan on show - user should explicitly click refresh
end;

procedure TFormCheckModules.FormDestroy(Sender: TObject);
begin
  ClearModulesList;
  FModulesList.Free;
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

procedure TFormCheckModules.ScanLuaModules;
var
  i: Integer;
  L: TLuaWebsiteModuleHandler;
  AMangaCheck: TMangaInformation;
  ModuleCount: Integer;
begin
  if FIsScanning then Exit;
  FIsScanning := True;
  Screen.Cursor := crHourGlass;
  Memo1.Clear;
  ModuleCount := 0;

  try
    lvModules.Items.BeginUpdate;
    try
      lvModules.Clear;
      ClearModulesList;
      LogMessage('Scanning loaded modules...');

      if Modules = nil then
      begin
        LogMessage('Warning: No modules loaded');
        Exit;
      end;

      for i := 0 to Modules.Count - 1 do
      begin
        with Modules.List[i] do
        begin
          if not Assigned(OnCheckSite) then
            Continue;
          with TLuaWebsiteModule(LuaModule) do
          try
            L := GetLuaWebsiteModuleHandler(Modules.List[i]);
            LuaPushNetStatus(L.Handle);
            AMangaCheck := TMangaInformation.Create();
            AMangaCheck.MangaCheck.Module := Modules.List[i];
            L.LoadObject('MANGACHECK', AMangaCheck.MangaCheck,
            @luaMangaCheckAddMetaTable);
            L.CallFunction(OnCheckSite);

            with AMangaCheck.MangaCheck do
            begin
              if MangaURL <> '' then
                MangaURL := MaybeFillHost(RootURL, MangaURL);
              if ChapterURL <> '' then
                ChapterURL := MaybeFillHost(RootURL, ChapterURL);

              if (MangaURL <> '') or (ChapterURL <> '') then
              begin
                AddModuleToList(AMangaCheck);
                Inc(ModuleCount);
                LogMessage(Format('Found: %s (CheckSite: %s, CheckChapter: %s)',
                [ModuleName,
                IfThen(MangaURL <> '', 'Yes' + IfThen(MangaTitle <> '',
                ' with Title', ' without Title'), 'No'),
                IfThen(ChapterURL <> '', 'Yes' + IfThen(ChapterTitle <> '',
                ' with Title', ' without Title'), 'No')]));
              end
              else
                AMangaCheck.Free;
            end;
          except
            on E: Exception do
            begin
              LogMessage('Error loading module '
              + ExtractFileName(Container.FileName) + ': ' + E.Message);
              if Assigned(AMangaCheck) then
                AMangaCheck.Free;
            end;
          end;
        end;
      end;

      LogMessage(Format('Scan complete: Found %d modules with check data',
      [ModuleCount]));
    finally
      lvModules.Items.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
    FIsScanning := False;
  end;
end;

procedure TFormCheckModules.AddModuleToList(const AMangaCheck: TMangaInformation);
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
  Item: TListItem;
  FailCount, SuccessCount: Integer;
  AMangaCheck: TMangaInformation;
  GetInfoResult, GetPageResult: TTestResult;
  Details, ExtraDetails: string;
  AllTestsPassed: Boolean;
begin
  if lvModules.Items.Count = 0 then
  begin
    CenteredMessageDlg(Self, 'No modules to check.'
    + ' Please refresh the module list first.', mtError, [mbOK], 0);
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  btnCheckIntegrity.Enabled := False;
  FailCount := 0;
  SuccessCount := 0;

  LogMessage('=== Starting Integrity Check ===');

  try
    for i := 0 to lvModules.Items.Count - 1 do
    begin
      Item := lvModules.Items[i];
      if not Item.Checked then Continue;

      AMangaCheck := TMangaInformation(Item.Data);
      if AMangaCheck = nil then Continue;

      Details := '';
      ExtraDetails := '';
      AllTestsPassed := True;

      UpdateItemStatus(Item, 'Checking...', '');
      Application.ProcessMessages;

      LogMessage('Checking module: ' + AMangaCheck.MangaCheck.ModuleName);

      try
        // Test GetInfo if MangaURL exists
        if AMangaCheck.MangaCheck.MangaURL <> '' then
        begin
          LogMessage('  Testing GetInfo with: '
          + AMangaCheck.MangaCheck.MangaURL);
          GetInfoResult := TestGetInfo(AMangaCheck);

          if GetInfoResult.Success then
          begin
            Details := 'OnGetInfo: PASS';
            Inc(SuccessCount);
            LogMessage('  OnGetInfo: PASS - ' + GetInfoResult.Message);

            // Validate manga title if provided
            if AMangaCheck.MangaCheck.MangaTitle <> '' then
            begin
              LogMessage('  Testing Manga Title with: '
              + AMangaCheck.MangaCheck.MangaTitle);
              if AMangaCheck.MangaCheck.MangaTitle =
              GetInfoResult.MangaTitle then
              begin
                Inc(SuccessCount);
                ExtraDetails := ' | Extra Test Check Manga Title: PASS';
                LogMessage('  Check Manga Title: PASS');
              end
              else
              begin
                Inc(FailCount);
                ExtraDetails := ' | Extra Test Check Manga Title: FAIL';
                LogMessage(Format('  Check Manga Title: FAIL '
                + '- Returned "%s" vs Expected "%s"',
                [GetInfoResult.MangaTitle, AMangaCheck.MangaCheck.MangaTitle]));
              end;
            end;

            // Validate chapter title if provided
            if (AMangaCheck.MangaCheck.ChapterURL <> '') and
            (AMangaCheck.MangaCheck.ChapterTitle <> '') then
            begin
              LogMessage('  Testing Chapter Title with: '
              + AMangaCheck.MangaCheck.ChapterTitle);
              if not ContainsStr(GetInfoResult.ChapterTitle,
              'Don''t Match Any Chapter URL') then
              begin
                if AMangaCheck.MangaCheck.ChapterTitle
                = GetInfoResult.ChapterTitle then
                begin
                  Inc(SuccessCount);
                  ExtraDetails := ExtraDetails
                  + ' | Extra Test Check Chapter Title: PASS';
                  LogMessage('  Check Chapter Title: PASS');
                end
                else
                begin
                  Inc(FailCount);
                  ExtraDetails := ExtraDetails
                  + ' | Extra Test Check Chapter Title: FAIL';
                  LogMessage(Format('  Check Chapter Title: FAIL '
                  + '- Returned "%s" vs Expected "%s"',
                  [GetInfoResult.ChapterTitle,
                  AMangaCheck.MangaCheck.ChapterTitle]));
                end;
              end
              else
              begin
                Inc(FailCount);
                ExtraDetails := ExtraDetails
                + ' | Extra Test Check Chapter Title: FAIL';
                LogMessage('  Check Chapter Title: FAIL - '
                + GetInfoResult.ChapterTitle);
              end;
            end
            else if AMangaCheck.MangaCheck.ChapterURL = '' then
            begin
              LogMessage('  No chapter link provided, using first chapter link');
              AMangaCheck.MangaCheck.ChapterURL := GetInfoResult.Data;
            end;
          end
          else
          begin
            Details := 'OnGetInfo: FAIL (' + GetInfoResult.Message + ')';
            Inc(FailCount);
            AllTestsPassed := False;
            LogMessage('  OnGetInfo: FAIL - ' + GetInfoResult.Message);
          end;
        end;

        // Test GetPageNumber if ChapterURL exists
        if AMangaCheck.MangaCheck.ChapterURL <> '' then
        begin
          if Details <> '' then
            Details := Details + ' | ';

          LogMessage('  Testing GetPageNumber with: '
          + AMangaCheck.MangaCheck.ChapterURL);
          GetPageResult := TestGetPageNumber(AMangaCheck);

          if GetPageResult.Success then
          begin
            Details := Details + 'OnGetPageNumber: PASS';
            Inc(SuccessCount);
            LogMessage('  OnGetPageNumber: PASS - ' + GetPageResult.Message);
          end
          else
          begin
            Details := Details + 'OnGetPageNumber: FAIL ('
            + GetPageResult.Message + ')';
            Inc(FailCount);
            AllTestsPassed := False;
            LogMessage('  OnGetPageNumber: FAIL - ' + GetPageResult.Message);
          end;
        end;

        // Update final status
        if AllTestsPassed then
        begin
          if (ExtraDetails = '') or not ContainsStr(ExtraDetails, 'FAIL') then
            UpdateItemStatus(Item, 'PASSED', Details + ExtraDetails)
          else
            UpdateItemStatus(Item, 'PASSED (FAILED Extra Tests)', Details
            + ExtraDetails);
        end
        else
          UpdateItemStatus(Item, 'FAILED', Details + ExtraDetails);

      except
        on E: Exception do
        begin
          UpdateItemStatus(Item, 'ERROR', 'Exception: ' + E.Message);
          LogMessage('  ERROR: ' + E.Message);
          Inc(FailCount);
        end;
      end;

      Application.ProcessMessages;
    end;

    LogMessage('=== Check Complete ===');
    LogMessage(Format('Total tests passed: %d', [SuccessCount]));
    LogMessage(Format('Total tests failed: %d', [FailCount]));

    StatusBar.SimpleText := Format('Check complete: %d tests passed,'
    + ' %d tests failed', [SuccessCount, FailCount]);

    CenteredMessageDlg(Self, Format('Integrity check complete.'#13#10 +
      'Tests Passed: %d'#13#10 +
      'Tests Failed: %d', [SuccessCount, FailCount]),
      mtInformation, [mbOK], 0);
  finally
    Screen.Cursor := crDefault;
    btnCheckIntegrity.Enabled := True;
  end;
end;

function TFormCheckModules.TestGetInfo(const AMangaCheck:
  TMangaInformation): TTestResult;
var
  L: TLuaWebsiteModuleHandler;
  ModuleCheck: TModuleContainer;
  AMangaInfo: TMangaInformation;
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

  AMangaInfo := nil;
  try
    with TLuaWebsiteModule(ModuleCheck.LuaModule) do
    begin
      L := GetLuaWebsiteModuleHandler(ModuleCheck);
      luaPushStringGlobal(L.Handle, 'URL', AMangaCheck.MangaCheck.MangaURL);
      LuaPushNetStatus(L.Handle);

      AMangaInfo := TMangaInformation.Create();
      AMangaInfo.MangaInfo.URL := AMangaCheck.MangaCheck.MangaURL;

      L.LoadObject('MANGAINFO', AMangaInfo.MangaInfo, @luaMangaInfoAddMetaTable);
      L.LoadObject('HTTP', AMangaInfo.HTTP, @luaHTTPSendThreadAddMetaTable);

      L.CallFunction(OnGetInfo);

      if lua_tointeger(L.Handle, -1) <> 0 then
      begin
        Result.Message := 'Runtime error: ' + lua_tostring(L, -1);
        lua_pop(L, 2);
        Exit;
      end;

      if lua_isboolean(L, -1) and lua_toboolean(L, -1) then
      begin
        if AMangaInfo.MangaInfo.Title = '' then
        begin
          Result.Message := 'OnGetInfo returned true, but no title found';
          Exit;
        end;

        if AMangaInfo.MangaInfo.ChapterLinks.Count = 0 then
        begin
          Result.Message := 'OnGetInfo returned true, but no chapters found';
          Exit;
        end;

        Result.Success := True;
        Result.Message := 'Success';
        Result.MangaTitle := AMangaInfo.MangaInfo.Title;

        if AMangaCheck.MangaCheck.ChapterURL = '' then
          Result.Data := AMangaInfo.MangaInfo.ChapterLinks[0]
        else
        begin
          ChapterIndex := AMangaInfo.MangaInfo.ChapterLinks.IndexOf(
          AMangaCheck.MangaCheck.ChapterURL);
          if ChapterIndex <> -1 then
            Result.ChapterTitle :=
            AMangaInfo.MangaInfo.ChapterNames[ChapterIndex]
          else
            Result.ChapterTitle := AMangaCheck.MangaCheck.ChapterURL
            + ' Don''t Match Any Chapter URL';
        end;
      end
      else
        Result.Message := 'OnGetInfo returned false or unexpected type';
    end;
  except
    on E: Exception do
      Result.Message := 'Exception: ' + E.Message;
  end;

  //if Assigned(AMangaInfo) then
  //  AMangaInfo.Free;
end;

function TFormCheckModules.TestGetPageNumber(const AMangaCheck:
  TMangaInformation): TTestResult;
var
  L: TLuaWebsiteModuleHandler;
  ModuleCheck: TModuleContainer;
  ATaskContainer: TTaskContainer;
  AHTTP: THTTPSendThread;
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
  AHTTP := nil;
  try
    with TLuaWebsiteModule(ModuleCheck.LuaModule) do
    begin
      L := GetLuaWebsiteModuleHandler(ModuleCheck);
      luaPushStringGlobal(L.Handle, 'URL', AMangaCheck.MangaCheck.ChapterURL);

      ATaskContainer := TTaskContainer.Create;
      AHTTP := THTTPSendThread.Create();

      L.LoadObject('TASK', ATaskContainer, @luaDownloadTaskMetaTable);
      L.LoadObject('HTTP', AHTTP, @luaHTTPSendThreadAddMetaTable);

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
  if Assigned(AHTTP) then
    AHTTP.Free;
end;

end.
