unit uMain;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}
  cthreads,
  cmem,
  {$endif}
  Classes, SysUtils, zipper, FileUtil, LazFileUtils, LazUTF8, LazUTF8Classes,
  Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, RegExpr, blcksock,
  ssl_openssl3, ssl_openssl3_lib, synacode, httpsendthread, uMisc, BaseThread,
  SimpleTranslator, SimpleException;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    itMonitor: TIdleTimer;
    lbTransferRate: TLabel;
    lbFileSize: TLabel;
    lbFileSizeValue: TLabel;
    lbStatus: TLabel;
    lbTransferRateValue: TLabel;
    pbDownload: TProgressBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure itMonitorTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  { TDownloadThread }

  TDownloadThread = class(TBaseThread)
  private
    FTotalSize, FCurrentSize: Integer;
    FHTTP: THTTPSendThread;
    FStatus: String;
    FProgress: String;
    FErrorMessage: String;
    UZip: TUnZipper;
  protected
    procedure SockOnStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: String);
    procedure UZipOnStartFile(Sender: TObject; const AFileName: String);
    procedure MainThreadUpdateStatus;
    procedure MainThreadUpdateProgress;
    procedure MainThreadUpdateProgressLabel;
    procedure MainThreadErrorGetting;
    procedure UpdateStatus(AStatus: String);
    procedure ShowErrorMessage(AMessage: String);
    procedure Execute; override;
  public
    URL: String;
    FileName: String;
    DirPath: String;
    MaxRetry: Cardinal;
    Extract: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

procedure IncReadCount(const ACount: Int64);

var
  frmMain: TfrmMain;
  dl: TDownloadThread;
  isDownload: Boolean = False;
  ReadCount: Integer = 0;
  CS_ReadCount: TRTLCriticalSection;

  _UpdApp: Boolean = False;
  _Extract: Boolean = False;
  _NoError: Boolean = False;
  _URL: String = '';
  _MaxRetry: Cardinal = 1;
  _LaunchApp: String = '';

  ProxyType: String;
  ProxyHost: String;
  ProxyPort: String;
  ProxyUser: String;
  ProxyPass: String;

  FMD_DIR: String;
  CONFIG_DIR: String;
  LANGUAGES_DIR: String;
  CONFIG_FILE: String;
  SZA: String;
  OSZA: String;

const
  Symbols: array [0..10] of Char =
    ('\', '/', ':', '*', '?', '"', '<', '>', '|', #9, ';');

  UA_CURL = 'curl/7.42.1';

resourcestring
  RS_InvalidURL = 'Invalid URL!';
  RS_Response = 'Response';
  RS_AnErrorOccured = 'An error occured when trying to download file.';
  RS_FileNotFound = 'File not found!';
  RS_ServerError = 'Server response error!';
  RS_LoadingPage = 'Loading page...';
  RS_FailedLoadPage = 'Failed loading page!';
  RS_RetryLoadPage = 'Retry loading page[%d]...';
  RS_Redirected = 'Redirected...';
  RS_Downloading = 'Downloading [%s]...';
  RS_FailedDownloadPage = 'Failed downloading file!';
  RS_RetryDownloading = 'Retry downloading[%d] [%s]...';
  RS_SaveFile = 'Saving file... ';
  RS_WaitMainApp = 'Waiting main app to close...';
  RS_UnpackFile = 'Unpacking file [%s]...';
  RS_Finished = 'Finished.';
  RS_ErrorCheckAntiVirus = 'Error saving file, check your AntiVirus!';
  RS_7zNotFound = 'Can''t extract file because 7za.exe not found!';
  RS_Unknown = '(unknown)';

implementation

uses
  uMessage;

{$R *.lfm}

function RemoveSymbols(const input: String): String;
var
  i: Integer;
begin
  Result := input;
  for i := Low(Symbols) to High(Symbols) do
  begin
    if Pos(Symbols[i], Result) > 0 then
      Result := StringReplace(Result, Symbols[i], '', [rfReplaceAll]);
  end;

  if (Length(Result) > 0) and
    (Result[Length(Result)] = '.') then
  begin
    Result[Length(Result)] := '-';
  end;
end;

procedure IncReadCount(const ACount: Int64);
begin
  EnterCriticalsection(CS_ReadCount);
  try
    Inc(ReadCount, ACount);
  finally
    LeaveCriticalsection(CS_ReadCount);
  end;
end;

{  TDownloadThread }

constructor TDownloadThread.Create;
begin
  inherited Create(True);
  isDownload := True;
  FHTTP := THTTPSendThread.Create(Self);
  FHTTP.Sock.OnStatus := @SockOnStatus;
  FHTTP.Timeout := 10000;
  FHTTP.SetProxy(ProxyType, ProxyHost, ProxyPort, ProxyUser, ProxyPass);
  FTotalSize := 0;
  FCurrentSize := 0;
  URL := '';
  FileName := '';
  DirPath := '';
  MaxRetry := 1;
  Extract := False;
end;

destructor TDownloadThread.Destroy;
begin
  if FHTTP <> nil then FHTTP.Free;
  isDownload := False;
  inherited Destroy;
end;

procedure TDownloadThread.MainThreadUpdateStatus;
begin
  frmMain.lbStatus.Caption := FStatus;
end;

procedure TDownloadThread.UpdateStatus(AStatus: String);
begin
  FStatus := AStatus;
  Synchronize(@MainThreadUpdateStatus);
end;

procedure TDownloadThread.ShowErrorMessage(AMessage: String);
begin
  FErrorMessage := AMessage;
  Synchronize(@MainThreadErrorGetting);
end;

procedure TDownloadThread.MainThreadUpdateProgress;
var
  barPos: Integer;
  prgText: String;
begin
  if Terminated then Exit;
  if FCurrentSize > 0 then
  begin
    if FTotalSize > 0 then
    begin
      barPos := Round(1000 * (FCurrentSize / FTotalSize));
      prgText := FormatByteSize(FCurrentSize) + ' / ' + FormatByteSize(FTotalSize);
    end
    else
    begin
      barPos := 0;
      prgText := FormatByteSize(FCurrentSize);
    end;
  end
  else
  begin
    barPos := 0;
    prgText := RS_Unknown;
  end;
  frmMain.pbDownload.Position := barPos;
  frmMain.lbFileSizeValue.Caption := prgText;
end;

procedure TDownloadThread.MainThreadErrorGetting;
begin
  if not _NoError then
  begin
    CenteredMessageDlg(MainForm, 'Error', FErrorMessage, mtError, [mbOK], 0);
  end;
end;

procedure TDownloadThread.SockOnStatus(Sender: TObject; Reason: THookSocketReason;
  const Value: String);
begin
  if Terminated then Exit;
  if FHTTP.Headers.IndexOfName('Content-Length') > -1 then
    FTotalSize := StrToIntDef(FHTTP.Headers.Values['Content-Length'], 0);
  case Reason of
    HR_Connect: FCurrentSize := 0;
    HR_ReadCount:
    begin
      Inc(FCurrentSize, StrToIntDef(Value, 0));
      IncReadCount(StrToIntDef(Value, 0));
    end;
  end;
  Synchronize(@MainThreadUpdateProgress);
end;

procedure TDownloadThread.MainThreadUpdateProgressLabel;
begin
  frmMain.lbFileSizeValue.Caption := FProgress;
end;

procedure TDownloadThread.UZipOnStartFile(Sender: TObject; const AFileName: String);
begin
  if FileExistsUTF8(AFileName) then
    DeleteFileUTF8(AFileName);
  UpdateStatus(Format(RS_UnpackFile, [ExtractFileName(AFileName)]));
end;

procedure TDownloadThread.Execute;
var
  sf: Boolean = False;
  regx: TRegExpr;
  i, ctry: Cardinal;
  csza,
  rurl,
  fname,
  sproject,
  sdir,
  s, sfile: String;
  st, HTTPHeaders: TStringList;
  filestream: TFileStreamUTF8;
begin
  URL := EncodeURL(DecodeURL(Trim(URL)));
  HTTPHeaders := TStringList.Create;
  regx := TRegExpr.Create;
  try
    HTTPHeaders.Assign(FHTTP.Headers);
    regx.ModifierI := True;
    s := LowerCase(URL);
    if (Pos('sourceforge.net/', s) <> 0) or (Pos('sf.net/', s) <> 0) then
    begin
      sf := True;
      FHTTP.UserAgent := UA_CURL;
      regx.Expression := '/download$';
      URL := Trim(regx.Replace(URL, '', False));
      //**parsing and fixing SF url
      if Pos('sf.net/', s) > 0 then begin
        URL := StringReplace(URL, 'sf.net/', 'sourceforge.net/', [rfIgnoreCase]);
        s := LowerCase(URL);
      end;
      if Pos('sourceforge.net/p/', s) > 0 then
        URL := StringReplace(URL, 'sourceforge.net/p/', 'sourceforge.net/projects/', [rfIgnoreCase]);
      regx.Expression := '^.*sourceforge.net/projects/(.+)/files/(.+)/([^/]+)$';
      if not regx.Exec(URL) then
      begin
        regx.Free;
        UpdateStatus(RS_InvalidURL);
        Exit;
      end;
      sproject := regx.Replace(URL, '$1', True);
      sdir := regx.Replace(URL, '$2', True);
      sfile := regx.Replace(URL, '$3', True);
      if FileName = '' then
        FileName := sfile;
      if Pos('https://', LowerCase(URL)) = 1 then
        rurl := 'https://'
      else
        rurl := 'http://';
      rurl := rurl + 'sourceforge.net/projects/' + sproject + '/files/' +
        sdir + '/' + sfile + '/download';
    end
    else
    begin
      rurl := URL;
      regx.ModifierG := True;
      regx.Expression := '^.*/([^/]*filename=)?([^/]*)$';
      FileName := regx.Replace(URL, '$2', True);
      regx.Expression := '(\.\w+)[\?\&].*$';
      FileName := regx.Replace(FileName, '$1', True);
      FileName := RemoveSymbols(FileName);
      if FileName = '' then
        FileName := 'new_version.7z';
    end;

    FHTTP.Headers.Assign(HTTPHeaders);
    //**loading page
    UpdateStatus(RS_LoadingPage);
    ctry := 0;
    while (not FHTTP.HTTPMethod('HEAD', Trim(rurl))) or
      (FHTTP.ResultCode >= 400) or (FHTTP.ResultCode < 100) do
    begin
      if Self.Terminated then Break;
      if (FHTTP.ResultCode >= 500) or (FHTTP.ResultCode < 100) then
      begin
        if ctry >= MaxRetry then
        begin
          UpdateStatus(RS_FailedLoadPage);
          ShowErrorMessage(RS_FailedLoadPage + LineEnding + LineEnding +
            RS_Response + ':' + LineEnding +
            IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString);
          Break;
        end;
        Inc(ctry);
        UpdateStatus(Format(RS_RetryLoadPage, [ctry]));
      end
      else
      if (FHTTP.ResultCode >= 400) and (FHTTP.ResultCode < 500) then
      begin
        UpdateStatus(RS_FileNotFound);
        ShowErrorMessage(RS_FileNotFound + LineEnding + LineEnding +
          RS_Response + ':' + LineEnding +
          IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString);
        Break;
      end;
      FHTTP.Clear;
    end;

    if (FHTTP.ResultCode >= 300) and (FHTTP.Headers.Values['Location'] <> '') then
    begin
      if not sf then
        HTTPHeaders.Values['Referer'] := ' ' + rurl;
      UpdateStatus(RS_Redirected);
      rurl := Trim(FHTTP.Headers.Values['Location']);
    end;

    //**download file
    if (FHTTP.ResultCode >= 100) and (FHTTP.ResultCode < 400) then
    begin
      UpdateStatus(Format(RS_Downloading, [FileName]));
      ctry := 0;
      FHTTP.Clear;
      FHTTP.Headers.Assign(HTTPHeaders);
      FHTTP.Cookies.Clear;
      while (not FHTTP.HTTPMethod('GET', Trim(rurl))) or
        (FHTTP.ResultCode >= 300) do
      begin
        if Self.Terminated then Break;
        if (FHTTP.ResultCode >= 500) or (FHTTP.ResultCode < 100) then
        begin
          if ctry >= MaxRetry then
          begin
            UpdateStatus(RS_FailedDownloadPage);
            ShowErrorMessage(RS_AnErrorOccured + LineEnding + LineEnding +
              RS_Response + ':' + LineEnding +
              IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString);
            Break;
          end;
          Inc(ctry);
          UpdateStatus(Format(RS_RetryDownloading, [ctry, FileName]));
        end
        else
        if (FHTTP.ResultCode >= 400) and (FHTTP.ResultCode < 500) then
        begin
          if ctry >= MaxRetry then
          begin
            UpdateStatus(RS_FileNotFound);
            ShowErrorMessage(RS_FileNotFound + LineEnding + LineEnding +
              RS_Response + ':' + LineEnding +
              IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString);
            Break;
          end;
          //try to load previous url, in case it was temporary url
          if ctry > 0 then begin
            rurl := URL;
            FHTTP.Cookies.Clear;
          end;
          Inc(ctry);
        end
        else
        if FHTTP.ResultCode >= 300 then
        begin
          HTTPHeaders.Values['Referer'] := ' ' + rurl;
          rurl := Trim(FHTTP.Headers.Values['location']);
        end;
        FHTTP.Clear;
        FHTTP.Headers.Assign(HTTPHeaders);
      end;
    end;

    //**save and unpack file
    if (not Self.Terminated) and
      (FHTTP.ResultCode >= 100) and (FHTTP.ResultCode < 300) and
      (FCurrentSize >= FTotalSize) then
    begin
      fname := DirPath + FileName;
      if FileExistsUTF8(fname) then
        DeleteFileUTF8(fname);

      if ForceDirectoriesUTF8(DirPath) then
      begin
        UpdateStatus(RS_SaveFile);
        filestream := TFileStreamUTF8.Create(fname, fmCreate);
        try
          FHTTP.Document.SaveToStream(filestream);
        finally
          filestream.Free;
        end;
      end;
      if not FileExistsUTF8(fname) then
        ShowErrorMessage(RS_ErrorCheckAntiVirus);

      if Extract and FileExistsUTF8(fname) then
      begin
        FHTTP.Free;
        FHTTP := nil;
        SSLImplementation := TSSLNone;
        ssl_openssl3_lib.DestroySSLInterface;

        UpdateStatus(Format(RS_UnpackFile, [fname]));
        if _UpdApp and
          FileExistsUTF8(SZA) then
        begin
          st := TStringList.Create;
          try
            FindAllFiles(st, FMD_DIR, '*.dbg', False);
            if st.Count > 0 then
              for i := 0 to st.Count - 1 do
                DeleteFileUTF8(st[i]);
          finally
            st.Free;
          end;
          if FileExistsUTF8(OSZA) then
            DeleteFileUTF8(OSZA);
          CopyFile(SZA, OSZA);
          csza := OSZA;
        end;
        if Pos('.zip', LowerCase(FileName)) <> 0 then
        begin
          UZip := TUnZipper.Create;
          UZip.OnStartFile := @UZipOnStartFile;
          try
            UZip.FileName := DirPath + FileName;
            UZip.OutputPath := Dirpath;
            UZip.Examine;
            UZip.UnZipAllFiles;
          finally
            UZip.Free;
            DeleteFileUTF8(DirPath + FileName);
          end;
        end
        else
        begin
          if FileExistsUTF8(csza) then
          begin
            RunExternalProcess(csza, ['x', fname, '-o' +
              AnsiQuotedStr(DirPath, '"'), '-aoa'], False, True);
            DeleteFileUTF8(fname);
          end
          else
            ShowErrorMessage(RS_7zNotFound);
        end;
        if FileExistsUTF8(OSZA) then
          if FileExistsUTF8(SZA) then
            DeleteFileUTF8(OSZA)
          else
            RenameFileUTF8(OSZA, SZA);
      end;
    end;
    UpdateStatus(RS_Finished);
    if (not Self.Terminated) and _UpdApp and (_LaunchApp <> '') then
      RunExternalProcess(_LaunchApp, [''], True, False);
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
  regx.Free;
  HTTPHeaders.Free;
end;

{ TfrmMain }

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
  if isDownload then
  begin
    dl.Terminate;
    dl.WaitFor;
  end;
  CloseAction := caFree;
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Randomize;
  FMD_DIR := CleanAndExpandDirectory(ExtractFilePath(Application.ExeName));
  CONFIG_DIR := FMD_DIR + 'config';
  LANGUAGES_DIR := FMD_DIR + 'languages';
  CONFIG_FILE := CONFIG_DIR + PathDelim + 'config.ini';
  SZA := FMD_DIR + '7za.exe';
  OSZA := FMD_DIR + 'old_7za.exe';

  InitSimpleExceptionHandler;
  try
  SimpleTranslator.LangDir := LANGUAGES_DIR;
  SimpleTranslator.LangAppName := 'updater';
  SimpleTranslator.CollectLanguagesFiles;
  InitCriticalSection(CS_ReadCount);
  //load proxy config from fmd
  if not FileExistsUTF8(CONFIG_FILE) then Exit;
  with TIniFile.Create(CONFIG_FILE) do
    try
      SimpleTranslator.SetLang(ReadString('languages', 'Selected', 'en'), 'updater');
      if ReadBool('connections', 'UseProxy', False) then
      begin
        ProxyType := ReadString('connections', 'ProxyType', 'HTTP');
        ProxyHost := ReadString('connections', 'Host', '');
        ProxyPort := ReadString('connections', 'Port', '');
        ProxyUser := ReadString('connections', 'User', '');
        ProxyPass := ReadString('connections', 'Pass', '');
      end
      else
      begin
        ProxyType := '';
        ProxyHost := '';
        ProxyPort := '';
        ProxyUser := '';
        ProxyPass := '';
      end;
    finally
      Free;
    end;
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DoneCriticalsection(CS_ReadCount);
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  s: String;
  i: Integer;
  sh: Boolean = False;
begin
  try
  if Paramcount > 0 then
  begin
    for i := 1 to Paramcount do
    begin
      s := LowerCase(ParamStrUTF8(i));
      //**Help
      if (s = '-h') or (s = '-help') or (s = '-?') or (s = '/?') then
      begin
        sh := True;
        Break;
      end
      else if s = '-q' then
      begin
        _NoError := True;
      end
      else if s = '-x' then
      begin
        _Extract := True;
      end
      else if i + 1 <= Paramcount then
      begin
        //**Update Mode
        if (s = '-a') or (s = '-d') then
        begin
          _UpdApp := (s = '-a');
          _URL := ParamStrUTF8(i + 1);
        end
        //**Max Retry
        else if s = '-r' then
        begin
          _MaxRetry := StrToIntDef(ParamStrUTF8(i + 1), 1);
        end
        else if s = '-l' then
        begin
          _LaunchApp := ParamStrUTF8(i + 1);
        end
        else if (LowerCase(s) = '--lang') then
        begin
          SimpleTranslator.SetLang(ParamStrUTF8(i + 1), 'updater');
        end;
      end;
    end;
  end;

  //**Show help
  if sh then
  begin
    frmMessage.Caption := Application.Title + ' - Help';
    frmMessage.ShowModal;
    Self.Close;
  end
  else if (_URL <> '') then
  begin
    if Pos('://', _URL) = 0 then
    begin
      _URL := 'http://' + _URL;
    end;
    if ExecRegExpr('^\w+?\://[^/]*\w+\.\w+(\:\d+)?(/|\Z).*$', _URL) then
    begin
      dl := TDownloadThread.Create;
      dl.URL := _URL;
      dl.MaxRetry := _MaxRetry;
      if _UpdApp then
      begin
        dl.DirPath := FMD_DIR;
      end
      else
      begin
        dl.DirPath := FMD_DIR + 'data' + PathDelim;
      end;

      dl.Extract := _Extract;
      dl.Start;
      itMonitor.Enabled := True;
    end
    else
    begin
      CenteredMessageDlg(Self, Application.Title, RS_InvalidURL, mtError, [mbOK], 0);
      Self.Close;
    end;
  end;
  except
    on E: Exception do
      ExceptionHandle(Self, E);
  end;
end;

procedure TfrmMain.itMonitorTimer(Sender: TObject);
begin
  if isDownload then
  begin
    EnterCriticalsection(CS_ReadCount);
    try
      lbTransferRateValue.Caption := FormatByteSize(ReadCount, True);
      ReadCount := 0;
    finally
      LeaveCriticalsection(CS_ReadCount);
    end;
  end
  else
  begin
    itMonitor.Enabled := False;
    Self.Close;
  end;
end;

end.
