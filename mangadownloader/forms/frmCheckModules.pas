unit frmCheckModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, WebsiteModules, LuaMangaInfo, LuaMangaCheck, LuaHTTPSend,
  LuaWebsiteModuleHandler, LuaWebsiteModules, uData, LuaUtils, uBaseUnit,
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
    procedure AddModuleToList(const AMangaCheck: TMangaInformation);
    function TestModuleFunction(const AMangaCheck: TMangaInformation;
      AFunctionType: string; var AResultMsg, AResultData: string): Boolean;
    procedure LogMessage(const AMsg: string);
    procedure ClearModulesList;
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
          HasCheckVars := False;

          with Modules.List[i] do
          begin
            if Assigned(OnCheckSite) then
            begin
              with TLuaWebsiteModule(LuaModule) do
              try
                L := GetLuaWebsiteModuleHandler(Modules.List[i]);
                LuaPushNetStatus(L.Handle);
                AMangaCheck := TMangaInformation.Create();
                AMangaCheck.MangaCheck.Module:= Modules.List[i];
                L.LoadObject('MANGACHECK', AMangaCheck.MangaCheck,
                @luaMangaCheckAddMetaTable);
                L.CallFunction(OnCheckSite);
              except
                on E: Exception do
                LogMessage('LUA>DoGetInfo("' + ExtractFileName(Container.FileName)
                + '")>,' + E.Message);
              end;
              with AMangaCheck.MangaCheck do
              begin
                // Try to read CheckSite and CheckChapter from the loaded module
                if MangaURL <> '' then HasCheckVars := True;
                if ChapterURL <> '' then HasCheckVars := True;
                if HasCheckVars then
                begin
                  AddModuleToList(AMangaCheck);
                  LogMessage('Found: ' + ModuleName +
                ' (CheckSite: ' + BoolToStr(MangaURL <> '', 'Yes' +
                BoolToStr(MangaTitle <> '', ' with Title', ' without Title'),
                'No') + ', CheckChapter: ' + BoolToStr(ChapterURL <> '', 'Yes'
                + BoolToStr(ChapterTitle <> '', ' with Title', ' without Title')
                , 'No') + ')');
                end;
              end;
            end;
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

procedure TFormCheckModules.AddModuleToList(const AMangaCheck:
  TMangaInformation);
var
  Item: TListItem;
begin
  Item := lvModules.Items.Add;
  Item.Caption := AMangaCheck.MangaCheck.ModuleName;
  Item.SubItems.Add(AMangaCheck.MangaCheck.ModuleFilename);
  Item.SubItems.Add(BoolToStr(AMangaCheck.MangaCheck.MangaURL <> '', 'Yes' +
  BoolToStr(AMangaCheck.MangaCheck.MangaTitle <> '', ' with Title',
  ' without Title'), 'No'));
  Item.SubItems.Add(BoolToStr(AMangaCheck.MangaCheck.ChapterURL <> '', 'Yes' +
  BoolToStr(AMangaCheck.MangaCheck.ChapterTitle <> '', ' with Title',
  ' without Title'), 'No'));
  Item.SubItems.Add('Not Checked');
  Item.SubItems.Add('');
  Item.Checked:= True;
  FModulesList.Add(AMangaCheck);

  Item.Data := AMangaCheck;
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
  FailCount, SuccessCount: Integer;
  ResultMsg, ResultData, Details, ExDetails: string;
  HasMangaURL, HasChapterURL: Boolean;
  AllTestsPassed: Boolean;
  AMangaCheck: TMangaInformation;
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
      AMangaCheck :=  TMangaInformation(Item.Data);
      ResultMsg := '';
      ResultData := '';

      if AMangaCheck = nil then Continue;

      HasMangaURL := AMangaCheck.MangaCheck.MangaURL <> '';
      HasChapterURL := AMangaCheck.MangaCheck.ChapterURL <> '';
      Details := '';
      ExDetails := '';
      AllTestsPassed := True;

      Item.SubItems[3] := 'Checking...';
      Item.SubItems[4] := '';
      Application.ProcessMessages;

      LogMessage('Checking module: ' + AMangaCheck.MangaCheck.ModuleName);

      try
        // Test GetInfo if CheckSite exists
        if HasMangaURL then
        begin
          LogMessage('  Testing GetInfo with: '
          + AMangaCheck.MangaCheck.MangaURL);
          if TestModuleFunction(AMangaCheck, 'OnGetInfo', ResultMsg,
          ResultData) then
          begin
            Details := 'OnGetInfo: PASS';
            Inc(SuccessCount);
            LogMessage('  OnGetInfo: PASS - ' + ResultMsg);
            if ResultMsg = 'Success' then
            begin
              Inc(SuccessCount);
              ExDetails := ' | Extra Test Check Manga Title: PASS';
            end
            else
            begin
              Inc(FailCount);
              ExDetails := ' | Extra Test Check Manga Title: FAIL';
            end;
            if AMangaCheck.MangaCheck.ChapterURL <> '' then
            begin
              if AMangaCheck.MangaCheck.MangaTitle <> '' then
              begin
                if ResultData = 'Chapter match Check Title' then
                begin
                  Inc(SuccessCount);
                  ExDetails := ExDetails +
                  ' | Extra Test Check Chapter Title: PASS';
                end
                else
                begin
                  Inc(FailCount);
                  ExDetails := ExDetails +
                  ' | Extra Test Check Chapter Title: FAIL';
                end;
              end;
            end
            else
            begin
              LogMessage('  No chapter link was provide,' +
              ' will test first chapter link');
              AMangaCheck.MangaCheck.ChapterURL :=  ResultData;
              HasChapterURL := True
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
        if HasChapterURL then
        begin
          if Details <> '' then
            Details := Details + ' | ';

          LogMessage('  Testing GetPageNumber with: '
          + AMangaCheck.MangaCheck.ChapterURL);
          if TestModuleFunction(AMangaCheck, 'OnGetPageNumber', ResultMsg,
          ResultData) then
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
        begin
          if ExDetails = '' then
            Item.SubItems[3] := 'PASSED'
          else
          if not ContainsStr(ExDetails, 'FAIL') then
            Item.SubItems[3] := 'PASSED'
          else
            Item.SubItems[3] := 'PASSED (FAILED Extra Tests)'
        end
        else
          Item.SubItems[3] := 'FAILED';

        Item.SubItems[4] := Details + ExDetails;
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

function TFormCheckModules.TestModuleFunction(const AMangaCheck:
  TMangaInformation; AFunctionType: string; var AResultMsg, AResultData: string)
: Boolean;
var
  FullChapterURL: string;
  ChapterIndex: Integer;
  L: TLuaWebsiteModuleHandler;
  ModuleCheck: TModuleContainer;
  AMangaInfo: TMangaInformation;
  ATaskContainer: TTaskContainer;
  AHTTP: THTTPSendThread;
  HasReturnedTitle, HasCheckTitle, IsCorrectTitle, HasReturnedChapterLinks:
  Boolean;
begin
  Result := False;
  AResultMsg := 'Unknown error';

  ModuleCheck := nil;
  ModuleCheck := Modules.LocateModule(AMangaCheck.MangaCheck.ModuleID);

  if ModuleCheck = nil then
  begin
    AResultMsg := 'Module not found in loaded modules';
    Exit;
  end;

  with TLuaWebsiteModule(ModuleCheck.LuaModule) do
  try
    L := GetLuaWebsiteModuleHandler(ModuleCheck);

    if AFunctionType = 'OnGetInfo' then
    begin
      luaPushStringGlobal(L.Handle, 'URL', AMangaCheck.MangaCheck.MangaURL);
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
          HasReturnedTitle := AMangaInfo.MangaInfo.Title <> '';
          HasCheckTitle := AMangaCheck.MangaCheck.MangaTitle <> '';
          IsCorrectTitle := AMangaInfo.MangaInfo.Title =
          AMangaCheck.MangaCheck.MangaTitle;
          HasReturnedChapterLinks := AMangaInfo.MangaInfo.ChapterLinks.Count <> 0;
          if HasReturnedTitle then
          begin
            if HasReturnedChapterLinks then
            begin
              if HasCheckTitle then
              begin
                if IsCorrectTitle then
                begin
                  AResultMsg := 'Success';
                end
                else
                begin
                  AResultMsg := 'OnGetInfo returned Title and Chapter Links,'
                  + ' but the returned Title does not match checked Title';
                end;
              end
              else
              begin
                AResultMsg := 'Success';
              end;
              if AMangaCheck.MangaCheck.ChapterURL = '' then
              begin
                AResultData := AMangaInfo.MangaInfo.ChapterLinks[0];
              end
              else
              begin
                FullChapterURL := MaybeFillHost(
                GetHostURL(AMangaCheck.MangaCheck.MangaURL),
                AMangaCheck.MangaCheck.ChapterURL);
                ChapterIndex := AMangaInfo.MangaInfo.ChapterLinks.IndexOf(
                FullChapterURL);
                if AMangaInfo.MangaInfo.ChapterNames[ChapterIndex] =
                AMangaCheck.MangaCheck.ChapterTitle then
                begin
                  AResultData := 'Chapter match Check Title'
                end
                else
                begin
                  AResultData := 'Chapter Don''t match Check Title'
                end;

              end;
            end
            else
            begin
              Result := False;
              AResultMsg := 'OnGetInfo returned Title,'
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
      luaPushStringGlobal(L.Handle, 'URL', AMangaCheck.MangaCheck.ChapterURL);
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
