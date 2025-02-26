unit SelfUpdater;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread, FMDOptions, StatusBarDownload, process,
  Controls, Dialogs, Forms;

type

  { TSelfUpdaterThread }

  TSelfUpdaterThread = class(TStatusBarDownload)
  private
    FFailedMessage: String;
  protected
    procedure HTTPRedirected(const AHTTP: THTTPSendThread; const URL: String);
  protected
    procedure SyncStart;
    procedure SyncFinal;
    procedure SyncShowFailed;
    procedure SyncFinishRestart;
    procedure ProceedUpdate;
    procedure Execute; override;
  public
    UpdateURL: String;
    NewVersionString: String;
    Filename: String;
    DownloadSuccess: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

resourcestring
  RS_Downloading = 'Downloading new version %s';
  RS_FailedTitle = 'Failed';
  RS_FailedDownload = 'Failed to download new version %s'#13#10#13#10'%d %s';
  RS_FailedToSave = 'Failed to save %s';
  RS_MissingFile = 'Missing %s';
  RS_FailedExtract = 'Failed to extract %s, exitstatus = %d';
  RS_ButtonCancel = 'Abort';
  RS_FinishRestartTitle = 'Download finished';
  RS_FinishRestart = 'Download update package finished, restart to proceed?';

implementation

uses FMDVars, frmMain;

{ TSelfUpdaterThread }

procedure TSelfUpdaterThread.HTTPRedirected(const AHTTP: THTTPSendThread;
  const URL: String);
begin
  UpdateStatusText(Format(RS_Downloading, [URL]));
end;

procedure TSelfUpdaterThread.SyncStart;
begin
  SelfUpdaterThread := Self;
end;

procedure TSelfUpdaterThread.SyncFinal;
begin
  SelfUpdaterThread := nil;
end;

procedure TSelfUpdaterThread.SyncShowFailed;
begin
  CenteredMessageDlg(MainForm, RS_FailedTitle, FFailedMessage, mtError, [mbOK], 0);
end;

procedure TSelfUpdaterThread.SyncFinishRestart;
begin
  if CenteredMessageDlg(MainForm, RS_FinishRestartTitle, RS_FinishRestart, mtConfirmation,
    mbYesNo, 0) = mrYes then
  begin
    ProceedUpdate;
  end;
end;

procedure TSelfUpdaterThread.ProceedUpdate;
begin
  if not DownloadSuccess then Exit;
  if FileExists(OLD_CURRENT_UPDATER_EXE) then
    DeleteFile(OLD_CURRENT_UPDATER_EXE);
  if FileExists(CURRENT_UPDATER_EXE) then
    RenameFile(CURRENT_UPDATER_EXE, OLD_CURRENT_UPDATER_EXE);
  if FileExists(OLD_CURRENT_UPDATER_EXE) then
  begin
    with TProcess.Create(nil) do
      try
        InheritHandles := False;
        CurrentDirectory := FMD_DIRECTORY;
        Executable := OLD_CURRENT_UPDATER_EXE;
        Parameters.Add(Application.ExeName);
        Parameters.Add(CURRENT_ZIP_EXE);
        Parameters.Add(Self.Filename);
        Parameters.Add(FMD_DIRECTORY);
        Execute;
      finally
        Free;
      end;
    DoAfterFMD := DO_UPDATE;
    FormMain.tmExitCommand.Interval := 32;
    FormMain.tmExitCommand.Enabled := True;
  end
  else
    FFailedMessage := Format(RS_MissingFile, [OLD_CURRENT_UPDATER_EXE]);
end;

procedure TSelfUpdaterThread.Execute;
begin
  DownloadSuccess := False;
  if UpdateURL = '' then
    Exit;
  try
    UpdateStatusText(Format(RS_Downloading, [UpdateURL]));
    if HTTP.GET(UpdateURL) and (HTTP.ResultCode < 300) then
    begin
      DownloadSuccess := True;
      Filename := FMD_DIRECTORY + UPDATE_PACKAGE_NAME;
      if FileExists(Filename) then
        DeleteFile(Filename);
      if not FileExists(Filename) then
        HTTP.Document.SaveToFile(Filename);

      if FileExists(Filename) then
        DeleteFile(Filename);
      if not FileExists(Filename) then
      begin
        HTTP.Document.SaveToFile(Filename);
        if not FileExists(Filename) then
        begin
          FFailedMessage := Format(RS_FailedToSave, [Filename]);
          DownloadSuccess := False;
        end;
      end
      else
      begin
        FFailedMessage := Format(RS_FailedToSave, [Filename]);
        DownloadSuccess := False;
      end;

      if DownloadSuccess and (not FileExists(CURRENT_ZIP_EXE)) then
      begin
        FFailedMessage := Format(RS_MissingFile, [CURRENT_ZIP_EXE]);
        DownloadSuccess := False;
      end;
    end
    else
      FFailedMessage := Format(RS_FailedDownload, [NewVersionString,
        HTTP.ResultCode, HTTP.ResultString]);
  except
    on E: Exception do
      FFailedMessage := E.Message;
  end;
end;

constructor TSelfUpdaterThread.Create;
begin
  inherited Create(True, FormMain, FormMain.IconList, 24);
  FFailedMessage := '';
  HTTP.OnRedirected := @HTTPRedirected;
  Synchronize(@SyncStart);
end;

destructor TSelfUpdaterThread.Destroy;
begin
  if (not Terminated) and (FFailedMessage <> '') then
    Synchronize(@SyncShowFailed)
  else
  if DownloadSuccess then
    Synchronize(@SyncFinishRestart);
  Synchronize(@SyncFinal);
  inherited Destroy;
end;

end.
