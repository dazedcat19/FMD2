unit DBUpdater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread, FMDOptions, StatusBarDownload,
  WebsiteModules, process, Controls, Dialogs, Buttons;

type

  { TDBUpdaterThread }

  TDBUpdaterThread = class(TStatusBarDownload)
  private
    FIndex: Integer;
    FModule: TModuleContainer;
    FItems,
    FFailedList: TStringList;
  protected
    procedure HTTPRedirected(const AHTTP: THTTPSendThread; const AURL: String);
  protected
    procedure SyncStart;
    procedure SyncFinal;
    procedure SyncUpdateHint;
    procedure SyncShowFailed;
    procedure SyncCloseUsed;
    procedure SyncReopenUsed;
    procedure SyncRemoveAttached;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AModule: TModuleContainer); overload;
    procedure Add(const S: TStrings); overload;
    procedure UpdateStatus;    // should be called from mainthread
    property Items: TStringList read FItems;
  end;

resourcestring
  RS_Downloading = 'Downloading %s';
  RS_FailedItemsTitle = 'Failed';
  RS_FailedItems = 'Failed to finish:'#13#10#13#10'%s';
  RS_FailedDownload = '%s: %d %s';
  RS_FailedToSave = '%s: failed to save';
  RS_MissingZipExe = '%s: Missing %s';
  RS_Extracting = 'Extracting %s';
  RS_FailedExtract = '%s: failed to extract, exitstatus = %d';
  RS_ButtonCancel = 'Abort';

implementation

uses FMDVars, LazFileUtils, frmMain, frmCustomMessageDlg;

function GetDBURL(const AName: String): String;
begin
  if Pos('<website>', DB_URL) <> -1 then
    Result := StringReplace(DB_URL, '<website>', AName, [rfIgnoreCase, rfReplaceAll])
  else
    Result := DB_URL + AName;
end;

{ TDBUpdaterThread }

procedure TDBUpdaterThread.HTTPRedirected(const AHTTP: THTTPSendThread;
  const AURL: String);
begin
  UpdateStatusText(Format('[%d/%d] ' + RS_Downloading, [FIndex + 1, FItems.Count, FModule.Name + ' ' + AURL]));
end;

procedure TDBUpdaterThread.SyncStart;
begin
  DBUpdaterThread := Self;
end;

procedure TDBUpdaterThread.SyncFinal;
begin
  DBUpdaterThread := nil;
end;

procedure TDBUpdaterThread.SyncUpdateHint;
begin
  StatusBar.Hint := Trim(FItems.Text);
end;

procedure TDBUpdaterThread.SyncShowFailed;
begin
  CenteredMessageDlg(MainForm, RS_FailedItemsTitle, Format(RS_FailedItems, [FFailedList.Text]),
    mtError, [mbOK], 0);
end;

procedure TDBUpdaterThread.SyncCloseUsed;
begin
  FormMain.edMangaListSearch.Clear;
  FormMain.vtMangaList.Clear;
  dataProcess.Close;
end;

procedure TDBUpdaterThread.SyncReopenUsed;
begin
  FormMain.OpenDataDB(FModule.ID);
end;

procedure TDBUpdaterThread.SyncRemoveAttached;
begin
  dataProcess.RemoveFilter;
end;

procedure TDBUpdaterThread.Execute;
var
  currentfilename: String;
  cont: Boolean;
  used: Boolean;
begin
  FIndex := 0;
  Synchronize(@SyncUpdateHint);
  while FIndex < FItems.Count do
  begin
    if Terminated then
      Break;
    try
      FModule := TModuleContainer(FItems.Objects[FIndex]);
      UpdateStatusText(Format('[%d/%d] ' + RS_Downloading, [FIndex + 1, FItems.Count, FModule.Name]));
      if HTTP.GET(GetDBURL(FModule.ID)) and (HTTP.ResultCode < 300) then
      begin
        cont := True;
        // save to data folder
        ForceDirectories(DATA_FOLDER);
        currentfilename := DATA_FOLDER + FModule.ID + DBDATA_SERVER_EXT;
        if FileExists(currentfilename) then
          DeleteFile(currentfilename);
        if not FileExists(currentfilename) then
        begin
          HTTP.Document.SaveToFile(currentfilename);
          if not FileExists(currentfilename) then
          begin
            FFailedList.Add(Format(RS_FailedToSave, [FModule.Name]));
            cont := False;
          end;
        end
        else
        begin
          FFailedList.Add(Format(RS_FailedToSave, [FModule.Name]));
          cont := False;
        end;

        if cont and (not FileExists(CURRENT_ZIP_EXE)) then
        begin
          FFailedList.Add(Format(RS_MissingZipExe, [FModule.Name, ZIP_EXE]));
          cont := False;
        end;

        if cont then
        begin
          // close and reopen current used
          used := FModule = TModuleContainer(currentWebsite);

          if used then
            Synchronize(@SyncCloseUsed)
          else if dataProcess.WebsiteLoaded(FModule.ID) then
          begin
            Synchronize(@SyncRemoveAttached);
          end;

          with TProcess.Create(nil) do
          begin
            try
              UpdateStatusText(Format('[%d/%d] ' + RS_Extracting, [FIndex + 1, FItems.Count, FModule.Name]));
              Executable := CURRENT_ZIP_EXE;
              CurrentDirectory := FMD_DIRECTORY;
              Parameters.Add('x');                                     // extract
              Parameters.Add(currentfilename);                         // input
              Parameters.Add('-o' + AnsiQuotedStr(DATA_FOLDER, '"'));  // destination
              Parameters.Add('-aoa');                                  // overwrite all
              Options := Options + [poWaitOnExit];
              ShowWindow := swoHIDE;
              Execute;

              cont := ExitStatus = 0;
              if cont then
              begin
                DeleteFile(currentfilename);
              end
              else
              begin
                FFailedList.Add(RS_FailedExtract, [FModule.Name, ExitStatus]);
              end;
            finally
              Free;
            end;
          end;

          if cont and used then
          begin
            Synchronize(@SyncReopenUsed);
          end;
        end;

        dataProcess.Open(FModule.ID);
      end
      else
        FFailedList.Add(Format(RS_FailedDownload, [FModule.Name + ' Manga List', HTTP.ResultCode,
          HTTP.ResultString]));
    except
      on E: Exception do
        FFailedList.Add(E.Message);
    end;
    Inc(FIndex);
  end;
end;

constructor TDBUpdaterThread.Create;
begin
  inherited Create(True, FormMain, FormMain.IconList, 24);
  FItems := TStringList.Create;
  FFailedList := TStringList.Create;
  HTTP.OnRedirected := @HTTPRedirected;
  Synchronize(@SyncStart);
end;

destructor TDBUpdaterThread.Destroy;
begin
  if (not Terminated) and (FFailedList.Count <> 0) then
    Synchronize(@SyncShowFailed);
  Synchronize(@SyncFinal);
  FFailedList.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TDBUpdaterThread.Add(const AModule: TModuleContainer);
var
  i: Integer;
begin
  // search on not sorted
  for i := 0 to FItems.Count - 1 do
    if AModule.Name = FItems[i] then
      Exit;
  FItems.AddObject(AModule.Name, AModule);
  UpdateStatus;
end;

procedure TDBUpdaterThread.Add(const S: TStrings);
var
  i, j, jmax: Integer;
  m: TModuleContainer;
begin
  if FItems = nil then Exit;
  // search on not sorted
  jmax := FItems.Count;
  for i := 0 to S.Count - 1 do
  begin
    j := 0;
    while j < jmax do
      if S.Objects[i] = FItems.Objects[j] then
        Break
      else
        Inc(j);
    if j = jmax then
    begin
      m := TModuleContainer(S.Objects[i]);
      FItems.AddObject(m.ID, m);
    end;
  end;
  UpdateStatus;
end;

procedure TDBUpdaterThread.UpdateStatus;
begin
  SyncUpdateHint;
end;

end.
