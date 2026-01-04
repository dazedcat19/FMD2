unit frmCheckModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, WebsiteModules, LuaMangaInfo, LuaHTTPSend,
  LuaWebsiteModuleHandler, LuaWebsiteModules, uData, LuaUtils,
  uDownloadsManager, LuaDownloadTask, httpsendthread, StrUtils,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

type
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
    procedure btnCheckAllClick(Sender: TObject);
    procedure btnCheckNoneClick(Sender: TObject);
    procedure btnCheckReverseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbWebsitesSelectAllClick(Sender: TObject);
    procedure tbWebsitesSelectInverseClick(Sender: TObject);
    procedure tbWebsitesSelectNoneClick(Sender: TObject);
  private
    FIsScanning: Boolean;
    FModulesList: TList;
    procedure ScanLuaModules;
    procedure CheckModuleIntegrity;
    procedure SelectModuleAll;
    procedure SelectModuleInverse;
    procedure SelectModuleNone;
    procedure AddModuleToList(const AModuleName: string;
      AModuleFilename: string; AModuleID: string; ACheckSite: string;
      ACheckChapter: string);
    function TestModuleFunction(const AModuleID: string; AFunctionType,
      AURL: string; var AResultMsg: string): Boolean;
    procedure LogMessage(const AMsg: string);
    procedure ClearModulesList;
  public
  end;

type
  TModuleCheckInfo = class
    ModuleName: string;
    ModuleFilename: string;
    ModuleID: string;
    CheckSite: string;
    CheckChapter: string;
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

  // Setup ListView
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

  Memo1.Clear;
  Memo1.ScrollBars := ssVertical;
  Memo1.ReadOnly := True;
end;

procedure TFormCheckModules.FormShow(Sender: TObject);
begin
  if lvModules.Items.Count = 0 then
    //ScanLuaModules;
end;

procedure TFormCheckModules.FormDestroy(Sender: TObject);
begin
  ClearModulesList;
  FModulesList.Free;
end;

procedure TFormCheckModules.tbWebsitesSelectAllClick(Sender: TObject);
begin
  SelectModuleAll;
end;

procedure TFormCheckModules.tbWebsitesSelectInverseClick(Sender: TObject);
begin
  SelectModuleInverse;
end;

procedure TFormCheckModules.tbWebsitesSelectNoneClick(Sender: TObject);
begin
  SelectModuleNone;
end;

procedure TFormCheckModules.ClearModulesList;
var
  i: Integer;
begin
  for i := 0 to FModulesList.Count - 1 do
    TModuleCheckInfo(FModulesList[i]).Free;
  FModulesList.Clear;
end;

procedure TFormCheckModules.LogMessage(const AMsg: string);
begin
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + AMsg);
end;

procedure TFormCheckModules.ScanLuaModules;
var
  i: Integer;
  ModuleName: string;
  ModuleFilename: string;
  ModuleID: string;
  CheckSiteValue, CheckChapterValue: string;
  HasCheckVars: Boolean;
begin
  if FIsScanning then Exit;
  FIsScanning := True;

  Screen.Cursor := crHourGlass;
  Memo1.Clear;

  try
    lvModules.Items.BeginUpdate;
    try
      lvModules.Clear;
      ClearModulesList;

      LogMessage('Scanning loaded modules...');

      // Iterate through already loaded modules in FMD2
      if Modules <> nil then
      begin
        for i := 0 to Modules.Count - 1 do
        begin
          CheckSiteValue := '';
          CheckChapterValue := '';
          HasCheckVars := False;

          with Modules.List[i] do
          begin
            ModuleName := Name;
            ModuleFilename := ExtractFileName(
            TLuaWebsiteModule(LuaModule).Container.FileName);
            ModuleID := ID;
            // Try to read CheckSite and CheckChapter from the loaded module
            if Checksite <> '' then
            begin
              CheckSiteValue := Checksite;
              HasCheckVars := True;
            end;
            if Checkchapter <> '' then
            begin
              CheckChapterValue  := Checkchapter;
              HasCheckVars := True;
            end;
          end;

          if HasCheckVars then
          begin
            AddModuleToList(ModuleName, ModuleFilename, ModuleID,
            CheckSiteValue, CheckChapterValue);
            LogMessage('Found: ' + ModuleName +
            ' (CheckSite: ' + BoolToStr(CheckSiteValue <> '', 'Yes', 'No') +
            ', CheckChapter: ' + BoolToStr(CheckChapterValue <> '', 'Yes',
            'No') + ')');
          end;
        end;
      end;
    finally
      lvModules.Items.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
    FIsScanning := False;
  end;

end;

procedure TFormCheckModules.AddModuleToList(const AModuleName: string;
  AModuleFilename: string; AModuleID: string; ACheckSite: string;
  ACheckChapter: string);
var
  Item: TListItem;
  CheckInfo: TModuleCheckInfo;
begin
  Item := lvModules.Items.Add;
  Item.Caption := AModuleName;
  Item.SubItems.Add(AModuleFilename);
  Item.SubItems.Add(BoolToStr(ACheckSite <> '', 'Yes', 'No'));
  Item.SubItems.Add(BoolToStr(ACheckChapter <> '', 'Yes', 'No'));
  Item.SubItems.Add('Not Checked');
  Item.SubItems.Add('');
  Item.Checked:= True;

  // Store check info
  CheckInfo := TModuleCheckInfo.Create;
  CheckInfo.ModuleName := AModuleName;
  CheckInfo.ModuleFilename := AModuleFilename;
  CheckInfo.ModuleID := AModuleID;
  CheckInfo.CheckSite := ACheckSite;
  CheckInfo.CheckChapter := ACheckChapter;
  FModulesList.Add(CheckInfo);

  Item.Data := CheckInfo;
end;

procedure TFormCheckModules.btnRefreshModulesClick(Sender: TObject);
begin
  ScanLuaModules;
end;

procedure TFormCheckModules.btnCheckIntegrityClick(Sender: TObject);
begin
  CheckModuleIntegrity;
end;

procedure TFormCheckModules.btnCheckAllClick(Sender: TObject);
begin
  SelectModuleAll;
end;

procedure TFormCheckModules.btnCheckNoneClick(Sender: TObject);

begin
  SelectModuleNone;
end;

procedure TFormCheckModules.btnCheckReverseClick(Sender: TObject);
begin
  SelectModuleInverse;
end;

procedure TFormCheckModules.SelectModuleAll;
var
  i: Integer;
begin
  for i := 0 to lvModules.Items.Count - 1 do
  begin
    lvModules.Items[i].Checked := True;
  end;
end;

procedure TFormCheckModules.SelectModuleNone;
var
  i: Integer;
begin
  for i := 0 to lvModules.Items.Count - 1 do
  begin
    lvModules.Items[i].Checked := False;
  end;
end;

procedure TFormCheckModules.SelectModuleInverse;
var
  i: Integer;
begin
  for i := 0 to lvModules.Items.Count - 1 do
  begin
    lvModules.Items[i].Checked := not lvModules.Items[i].Checked;
  end;
end;
procedure TFormCheckModules.CheckModuleIntegrity;
var
  i: Integer;
  Item: TListItem;
  CheckInfo: TModuleCheckInfo;
  FailCount, SuccessCount: Integer;
  ResultMsg, Details: string;
  HasCheckSite, HasCheckChapter: Boolean;
  AllTestsPassed: Boolean;
begin
  if lvModules.Items.Count = 0 then
  begin
    ShowMessage('No modules to check. Please refresh the module list first.');
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
      CheckInfo := TModuleCheckInfo(Item.Data);

      if CheckInfo = nil then Continue;

      HasCheckSite := CheckInfo.CheckSite <> '';
      HasCheckChapter := CheckInfo.CheckChapter <> '';
      Details := '';
      AllTestsPassed := True;

      Item.SubItems[3] := 'Checking...';
      Item.SubItems[4] := '';
      Application.ProcessMessages;

      LogMessage('Checking module: ' + CheckInfo.ModuleName);

      try
        // Test GetInfo if CheckSite exists
        if HasCheckSite then
        begin
          LogMessage('  Testing GetInfo with: ' + CheckInfo.CheckSite);
          if TestModuleFunction(CheckInfo.ModuleID, 'OnGetInfo',
          CheckInfo.CheckSite, ResultMsg) then
          begin
            Details := 'OnGetInfo: PASS';
            Inc(SuccessCount);
            if AnsiContainsStr(ResultMsg, 'Success') then
            begin
              LogMessage('  OnGetInfo: PASS - ' + 'Success');
            end;

            if CheckInfo.CheckChapter = '' then
            begin
              LogMessage('  No chapter link was provide,' +
              ' will test first chapter link');
              CheckInfo.CheckChapter :=  Copy(ResultMsg, 9,
              Length(ResultMsg) - 9);
              HasCheckChapter := True
            end;
          end
          else
          begin
            Details := 'OnGetInfo: FAIL (' + ResultMsg + ')';
            Inc(FailCount);
            AllTestsPassed := False;
            LogMessage('  OnGetInfo: FAIL - ' + ResultMsg);
          end;
        end;

        // Test GetPageNumber if CheckChapter exists
        if HasCheckChapter then
        begin
          if Details <> '' then
            Details := Details + ' | ';

          LogMessage('  Testing GetPageNumber with: ' + CheckInfo.CheckChapter);
          if TestModuleFunction(CheckInfo.ModuleID, 'OnGetPageNumber',
          CheckInfo.CheckChapter, ResultMsg) then
          begin
            Details := Details + 'OnGetPageNumber: PASS';
            Inc(SuccessCount);
            LogMessage('  OnGetPageNumber: PASS - ' + ResultMsg);
          end
          else
          begin
            Details := Details + 'OnGetPageNumber: FAIL (' + ResultMsg + ')';
            Inc(FailCount);
            AllTestsPassed := False;
            LogMessage('  OnGetPageNumber: FAIL - ' + ResultMsg);
          end;
        end;

        if AllTestsPassed then
          Item.SubItems[3] := 'PASSED'
        else
          Item.SubItems[3] := 'FAILED';

        Item.SubItems[4] := Details;
      except
        on E: Exception do
        begin
          Item.SubItems[3] := 'ERROR';
          Item.SubItems[4] := 'Exception: ' + E.Message;
          LogMessage('  ERROR: ' + E.Message);
          Inc(FailCount);
        end;
      end;

      Application.ProcessMessages;
    end;

    LogMessage('=== Check Complete ===');
    LogMessage(Format('Total tests passed: %d', [SuccessCount]));
    LogMessage(Format('Total tests failed: %d', [FailCount]));

    StatusBar.SimpleText := Format('Check complete: %d tests passed, %d tests failed',
      [SuccessCount, FailCount]);

    ShowMessage(Format('Integrity check complete.'#13#10 +
                      'Tests Passed: %d'#13#10 +
                      'Tests Failed: %d', [SuccessCount, FailCount]));
  finally
    Screen.Cursor := crDefault;
    btnCheckIntegrity.Enabled := True;
  end;
end;

function TFormCheckModules.TestModuleFunction(const AModuleID: string;
  AFunctionType, AURL: string; var AResultMsg: string): Boolean;
var
  L: TLuaWebsiteModuleHandler;
  ModuleCheck: TModuleContainer;
  AMangaInfo: TMangaInformation;
  ATaskContainer: TTaskContainer;
  AHTTP: THTTPSendThread;
begin
  Result := False;
  AResultMsg := 'Unknown error';

  ModuleCheck := nil;
  ModuleCheck := Modules.LocateModule(AModuleID);

  if ModuleCheck = nil then
  begin
    AResultMsg := 'Module not found in loaded modules';
    Exit;
  end;

  with TLuaWebsiteModule(ModuleCheck.LuaModule) do
  try
    L := GetLuaWebsiteModuleHandler(ModuleCheck);
    luaPushStringGlobal(L.Handle, 'URL', AURL);

    if AFunctionType = 'OnGetInfo' then
    begin
      LuaPushNetStatus(L.Handle);
      // Create and initialize MangaInfo
      AMangaInfo := TMangaInformation.Create();

      L.LoadObject('MANGAINFO', AMangaInfo.MangaInfo, @luaMangaInfoAddMetaTable);
      L.LoadObject('HTTP', AMangaInfo.HTTP, @luaHTTPSendThreadAddMetaTable);

      L.CallFunction(OnGetInfo);
      if lua_tointeger(L.Handle, -1) <> 0 then
      begin
        AResultMsg := 'Runtime error: ' + lua_tostring(L, -1);
        lua_pop(L, 2);
        Exit;
      end;
      if lua_isboolean(L, -1) then
      begin
        Result := lua_toboolean(L, -1);
        if Result then
        begin
          if AMangaInfo.MangaInfo.Title <> '' then
          begin
            if AMangaInfo.MangaInfo.ChapterLinks.Count <> 0 then
            begin
              AResultMsg := 'Success|' + AMangaInfo.MangaInfo.ChapterLinks[0];
            end
            else
            begin
              Result := False;
              AResultMsg := 'OnGetInfo returned true,'
              + ' but was not able to get chapters list';
            end;
          end
          else
          begin
            Result := False;
            AResultMsg := 'OnGetInfo returned true,'
            + ' but was not able to get title';
          end;
        end
        else
        begin
          AResultMsg := 'OnGetInfo returned false';
        end;
      end
      else
      begin
        AResultMsg := 'Unexpected return type';
      end;
    end;
    if AFunctionType = 'OnGetPageNumber' then
    begin
      // Create and initialize ATaskThread
      ATaskContainer := TTaskContainer.Create;
      AHTTP := THTTPSendThread.Create();

      L.LoadObject('TASK', ATaskContainer, @luaDownloadTaskMetaTable);
      L.LoadObject('HTTP', AHTTP, @luaHTTPSendThreadAddMetaTable);

      L.CallFunction(OnGetPageNumber);
      if lua_tointeger(L.Handle, -1) <> 0 then
      begin
        AResultMsg := 'Runtime error: ' + lua_tostring(L, -1);
        lua_pop(L, 2);
        Exit;
      end;
      if lua_isboolean(L, -1) then
      begin
        Result := lua_toboolean(L, -1);
        if Result then
        begin
          if ATaskContainer.PageLinks.Count <> 0 then
          begin
            AResultMsg := 'Success';
          end
          else
          begin
            Result := False;
            AResultMsg := 'OnGetInfo returned true,'
            + ' but was not able to get pages list';
          end;
        end
        else
        begin
          AResultMsg := 'OnGetPageNumber returned false';
        end;
      end
      else
      begin
        AResultMsg := 'Unexpected return type';
      end;
    end;


  except
      on E: Exception do
        LogMessage('LUA>DoGetInfo("' + ExtractFileName(Container.FileName)
        + '")>,' + E.Message);
  end;
end;

end.
