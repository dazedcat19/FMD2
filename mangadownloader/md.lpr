program md;

{$mode objfpc}{$H+}

uses
  FMDOptions,
 {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,
 {$ENDIF} {$ENDIF}
 {$ifdef windows}
  windows,
 {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, LazFileUtils, jsonini, simpleipc, sqlite3dyn, uBaseUnit, FMDVars, webp,
  CheckUpdate, DBUpdater, SelfUpdater, uDownloadsManager, LuaWebsiteModules,
  LuaBase, SimpleException, Classes, sysutils, frmMain, frmCheckModules,
  uDarkStyle, uMetaDarkStyle, uDarkStyleSchemes, uDarkStyleParams, MultiLog,
  FileChannel, ssl_openssl3_lib, blcksock, ssl_openssl3, SQLiteData;

var
  CheckInstance: Boolean = True;
  AllowedToRun: Boolean = True;
  EnableLogging: Boolean = False;
  LogFileName: String = '';
  s: TStringList;
  iDarkMode: Integer = 0;
  i: Integer;
  v: Integer;
  {$IFDEF DEBUGLEAKS}
  trcfile: String;
  {$ENDIF DEBUGLEAKS}

{$ifdef windows}
  prevPID: Integer = -1;
  prevHandle: THandle;
  evpathlen: Integer;
  evpath: String;

const
  {$ifdef win64}
  OpenSSLDLLSSLName = 'libssl-3-x64.dll';
  OpenSSLDLLUtilName = 'libcrypto-3-x64.dll';
  {$else}
  OpenSSLDLLSSLName = 'libssl-3.dll';
  OpenSSLDLLUtilName = 'libcrypto-3.dll';
  {$endif}
{$endif}

{$R *.res}

begin
  {
    app params
    --lua-dofile: always load lua modules from file
    --dump-loaded-modules: dump loaded modules ("ID Name") to log
    --dorestart-pid=9999: windows only, handle used to restart app
  }

  {$ifdef windows}
  //wait for prev process from dorestart
  prevPID := StrToIntDef(AppParams.Values['--dorestart-pid'], -1);
  if prevPID <> -1 then
  begin
    // remove previous --dorestart-handle from params
    AppParams.Delete(AppParams.IndexOfName('--dorestart-pid'));
    prevHandle := OpenProcess(SYNCHRONIZE, False, prevPID);
    if prevHandle <> 0 then
    begin
      // if previous handle takes longer than 5s, we give up
      WaitForSingleObject(prevHandle, 5000);

      if IsWindow(prevHandle) then
      begin
        TerminateProcess(prevHandle, 0);
      end;

      CloseHandle(prevHandle);
    end;
  end;
  {$endif}

  for i := 0 to AppParams.Count - 1 do
  begin
    // always execute lua modules from file, for dev purpose
    if SameText(AppParams[i], '--lua-dofile') then
    begin
       AlwaysLoadLuaFromFile := True
    end
    // don't use commit queue, might be slow on large databases
    else if SameText(AppParams[i], '--no-commit-queue') then
    begin
      MAX_COMMIT_QUEUE := 0;
      MAX_SQL_FLUSH_QUEUE := 0;
    end
    // set max commit queue before writing it to disk
    else if SameText(AppParams[i], '--max-commit-queue') then
    begin
      v := StrToIntDef(AppParams.ValueFromIndex[i], -1);
      if v < 1 then
      begin
        v := 1;
      end;

      MAX_COMMIT_QUEUE := v;
    end
    // max sql lines before flush it to sqlite engine
    else if SameText(AppParams[i], '--max-flush-queue') then
    begin
      v := StrToIntDef(AppParams.ValueFromIndex[i], -1);
      if v < 1 then
      begin
        v := 1;
      end;

      MAX_SQL_FLUSH_QUEUE := v;
    end
    // max sql lines before flush it to sqlite engine, used in large iterations
    else if SameText(AppParams[i], '--max-big-flush-queue') then
    begin
      v := StrToIntDef(AppParams.ValueFromIndex[i],-1);
      if v < 1 then
      begin
        v := 1;
      end;

      MAX_BIG_SQL_FLUSH_QUEUE := v;
    end
    // timer backup interval, in minutes
    else if SameText(AppParams[i], '--backup-interval') then
    begin
      v := StrToIntDef(AppParams.ValueFromIndex[i], -1);
      if v < 1 then
      begin
        v := 1;
      end;

      TimerBackupInterval := v;
    end;
  end;

  with TJSONIniFile.Create(SETTINGS_FILE) do
  begin
    try
      iDarkMode := ReadInteger('darkmode', 'mode', 0);
      CheckInstance := ReadBool('general', 'OneInstanceOnly', True);
      EnableLogging := ReadBool('logger', 'Enabled', False);
      if EnableLogging then
      begin
        LogFileName := ExpandFileNameUTF8(ReadString('logger', 'LogFileName', DEFAULT_LOG_FILE), FMD_DIRECTORY);
      end;
    finally
      Free;
    end;
  end;

  if CheckInstance then
  begin
    with TSimpleIPCClient.Create(nil) do
    begin
      try
        ServerID := FMD_INSTANCE;

        if ServerRunning then
        begin
          AllowedToRun := False;
          Active := True;
          SendStringMessage('BringToFront');
        end;
      finally
        Free;
      end;
    end;
  end;

  if not AllowedToRun then
  begin
    Exit;
  end;

  {$IFDEF DEBUGLEAKS}
  trcfile := FMD_DIRECTORY + FMD_EXENAME + '.trc';
  if FileExistsUTF8(trcfile) then
  begin
    DeleteFileUTF8(trcfile);
  end;

  SetHeapTraceOutput(trcfile);
  {$ENDIF DEBUGLEAKS}

  {$ifdef windows}
  // set environment variables
  evpathlen := windows.GetEnvironmentVariable('PATH', nil, 0);
  setlength(evpath, evpathlen - 1);
  windows.GetEnvironmentVariable('PATH', pchar(evpath), evpathlen);
  evpath := FMD_DIRECTORY + ';' + evpath;
  windows.SetEnvironmentVariable('PATH', pchar(evpath));
  {$endif}

  Application.Title := 'Free Manga Downloader';
  RequireDerivedFormResource := True;
  //Logger.ThreadSafe := True; //Automatically uses safe thread code
  Logger.Enabled := EnableLogging;
  InitSimpleExceptionHandler(LogFileName);
  if EnableLogging then
  begin
    if MainExceptionHandler.LogFileOK then
    begin
      FileLogger := TFileChannel.Create(LogFileName, [fcoShowHeader, fcoShowPrefix, fcoShowTime]);
      Logger.Channels.Add(FileLogger);
      Logger.Send(QuotedStrd(Application.Title) + ' started [PID:' + IntToStr(GetProcessID) + ']');
    end;

    s := TStringList.Create;
    try
      s.AddText(SimpleException.GetApplicationInfo);
      Logger.Send('Application info', s);
    finally
      s.Free;
    end;
  end;

  //sqlite
  if FileExists(FMD_DIRECTORY + Sqlite3Lib) then
  begin
    SQLiteDefaultLibrary := FMD_DIRECTORY + Sqlite3Lib;
  end;

  {$ifdef windows}
  //openssl
  if IsSSLloaded then
  begin
    DestroySSLInterface;
  end;

  if FileExists(FMD_DIRECTORY + OpenSSLDLLSSLName) and FileExists(FMD_DIRECTORY + OpenSSLDLLUtilName) then
  begin
    DLLSSLName := FMD_DIRECTORY + OpenSSLDLLSSLName;
    DLLUtilName := FMD_DIRECTORY + OpenSSLDLLUtilName;

    if InitSSLInterface then
    begin
      SSLImplementation := TSSLOpenSSL3;
    end;
  end
  else if FileExists(FMD_DIRECTORY + DLLSSLName) and FileExists(FMD_DIRECTORY + DLLUtilName) then
  begin
    DLLSSLName := FMD_DIRECTORY + DLLSSLName;
    DLLUtilName := FMD_DIRECTORY + DLLUtilName;
  end;

  if not IsSSLloaded then
  begin
    InitSSLInterface;
  end;
  {$endif}

  //webp
  if FileExists(FMD_DIRECTORY + DLLWebPName) then
  begin
    DLLWebPName := FMD_DIRECTORY + DLLWebPName;
  end;

  Case iDarkMode of
     0:  PreferredAppMode := pamAllowDark;
     1:  PreferredAppMode := pamForceDark;
     2:  PreferredAppMode := pamForceLight;
  end;

  Application.Scaled := True;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.winBuildNumber := g_buildNumber;
  Application.CreateForm(TFormCheckModules, FormCheckModules);
  Application.Run;
end.
