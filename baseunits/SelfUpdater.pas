unit SelfUpdater;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpsendthread, StatusBarDownload, process, Controls,
  Dialogs, Forms;

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

uses
  uVars, uOptions, frmMain, frmCustomMessageDlg;

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
  if not DownloadSuccess then
  begin
    Exit;
  end;

  if FileExists(OLD_CURRENT_UPDATER_EXE) then
  begin
    DeleteFile(OLD_CURRENT_UPDATER_EXE);
  end;

  if FileExists(CURRENT_UPDATER_EXE) then
  begin
    RenameFile(CURRENT_UPDATER_EXE, OLD_CURRENT_UPDATER_EXE);
  end;

  if FileExists(OLD_CURRENT_UPDATER_EXE) then
  begin
    with TProcess.Create(nil) do
    begin
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
    end;

    FMDOptions.General.AfterFMDDo := DO_UPDATE;
    MainForm.tmExitCommand.Interval := 32;
    MainForm.tmExitCommand.Enabled := True;
  end
  else
  begin
    FFailedMessage := Format(RS_MissingFile, [OLD_CURRENT_UPDATER_EXE]);
  end;
end;

procedure TSelfUpdaterThread.Execute;
begin
  DownloadSuccess := False;

  if UpdateURL = '' then
  begin
    Exit;
  end;

  try
    UpdateStatusText(Format(RS_Downloading, [UpdateURL]));

    if HTTP.GET(UpdateURL) and (HTTP.ResultCode < 300) then
    begin
      DownloadSuccess := True;
      Filename := FMD_DIRECTORY + UPDATE_PACKAGE_NAME;

      if FileExists(Filename) then
      begin
        DeleteFile(Filename);
      end;

      if not FileExists(Filename) then
      begin
        HTTP.Document.SaveToFile(Filename);
      end;

      if FileExists(Filename) then
      begin
        DeleteFile(Filename);
      end;

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
    begin
      FFailedMessage := Format(RS_FailedDownload, [NewVersionString,
        HTTP.ResultCode, HTTP.ResultString]);
    end;
  except
    on E: Exception do
      FFailedMessage := E.Message;
  end;
end;

constructor TSelfUpdaterThread.Create;
begin
  inherited Create(True, MainForm, MainForm.IconList, 24);
  FFailedMessage := '';
  HTTP.OnRedirected := @HTTPRedirected;
  Synchronize(@SyncStart);
end;

destructor TSelfUpdaterThread.Destroy;
begin
  if (not Terminated) and (FFailedMessage <> '') then
  begin
    Synchronize(@SyncShowFailed);
  end
  else if DownloadSuccess then
  begin
    Synchronize(@SyncFinishRestart);
  end;

  Synchronize(@SyncFinal);
  inherited Destroy;
end;

end.
