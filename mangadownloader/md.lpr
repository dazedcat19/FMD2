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
  Forms, LazFileUtils, jsonini, simpleipc, sqlite3dyn, uBaseUnit,
  FMDVars, webp, CheckUpdate, DBUpdater, SelfUpdater, uDownloadsManager,
  LuaWebsiteModules, LuaBase, SimpleException, Classes, sysutils, frmMain,
  uMetaDarkStyle, uDarkStyleSchemes, uDarkStyleParams,
  MultiLog, FileChannel, ssl_openssl_lib, blcksock, ssl_openssl, SQLiteData;

var
  CheckInstance: Boolean = True;
  AllowedToRun: Boolean = True;
  EnableLogging: Boolean = False;
  LogFileName: String = '';
  s: TStringList;
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
  OpenSSLDLLSSLName='libssl-1_1-x64.dll';
  OpenSSLDLLUtilName='libcrypto-1_1-x64.dll';
  {$else}
  OpenSSLDLLSSLName='libssl-1_1.dll';
  OpenSSLDLLUtilName='libcrypto-1_1.dll';
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
  prevPID:=StrToIntDef(AppParams.Values['--dorestart-pid'],-1);
  if prevPID<>-1 then
  begin
    // remove previous --dorestart-handle from params
    AppParams.Delete(AppParams.IndexOfName('--dorestart-pid'));
    prevHandle:=OpenProcess(SYNCHRONIZE, False, prevPID);
    if prevHandle<>0 then
    begin
      // if previous handle takes longer than 5s, we give up
      WaitForSingleObject(prevHandle, 5000);
      if IsWindow(prevHandle) then
        TerminateProcess(prevHandle, 0);
      CloseHandle(prevHandle);
    end;
  end;
  {$endif}

  for i:=0 to AppParams.Count-1 do
  begin
    // always execute lua modules from file, for dev purpose
    if SameText(AppParams[i],'--lua-dofile') then
       AlwaysLoadLuaFromFile:=True
    else
    // don't use commit queue, might be slow on large databases
    if SameText(AppParams[i],'--no-commit-queue') then
    begin
      MAX_COMMIT_QUEUE:=0;
      MAX_SQL_FLUSH_QUEUE:=0;
    end
    else
    // set max commit queue before writing it to disk
    if SameText(AppParams[i],'--max-commit-queue') then
    begin
      v:=StrToIntDef(AppParams.ValueFromIndex[i],-1);
      if v<1 then v:=1;
      MAX_COMMIT_QUEUE:=v;
    end
    else
    // max sql lines before flush it to sqlite engine
    if SameText(AppParams[i],'--max-flush-queue') then
    begin
      v:=StrToIntDef(AppParams.ValueFromIndex[i],-1);
      if v<1 then v:=1;
      MAX_SQL_FLUSH_QUEUE:=v;
    end
    else
    // max sql lines before flush it to sqlite engine, used in large iterations
    if SameText(AppParams[i],'--max-big-flush-queue') then
    begin
      v:=StrToIntDef(AppParams.ValueFromIndex[i],-1);
      if v<1 then v:=1;
      MAX_BIG_SQL_FLUSH_QUEUE:=v;
    end
    else
    // timer backup interval, in minutes
    if SameText(AppParams[i],'--backup-interval') then
    begin
      v:=StrToIntDef(AppParams.ValueFromIndex[i],-1);
      if v<1 then v:=1;
      TimerBackupInterval:=v;
    end;
  end;

  with TJSONIniFile.Create(SETTINGS_FILE) do
    try
      CheckInstance := ReadBool('general', 'OneInstanceOnly', True);
      EnableLogging := ReadBool('logger', 'Enabled', False);
      if EnableLogging then
        LogFileName := ExpandFileNameUTF8(ReadString('logger', 'LogFileName', DEFAULT_LOG_FILE), FMD_DIRECTORY);
    finally
      Free;
    end;

  if CheckInstance then
  begin
    with TSimpleIPCClient.Create(nil) do
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
  if not AllowedToRun then Exit;

  {$IFDEF DEBUGLEAKS}
  trcfile := FMD_DIRECTORY + FMD_EXENAME + '.trc';
  if FileExistsUTF8(trcfile) then
    DeleteFileUTF8(trcfile);
  SetHeapTraceOutput(trcfile);
  {$ENDIF DEBUGLEAKS}

  {$ifdef windows}
  // set environment variables
  evpathlen:=windows.GetEnvironmentVariable('PATH',nil,0);
  setlength(evpath,evpathlen-1);
  windows.GetEnvironmentVariable('PATH',pchar(evpath),evpathlen);
  evpath:=FMD_DIRECTORY+';'+evpath;
  windows.SetEnvironmentVariable('PATH',pchar(evpath));
  {$endif}

  Application.Title:='Free Manga Downloader';
  RequireDerivedFormResource:=True;
  //Logger.ThreadSafe:=True;  --Automatically uses safe thread code
  Logger.Enabled:=EnableLogging;
  InitSimpleExceptionHandler(LogFileName);
  if EnableLogging then
  begin
    if MainExceptionHandler.LogFileOK then
    begin
      FileLogger := TFileChannel.Create(LogFileName, [fcoShowHeader, fcoShowPrefix, fcoShowTime]);
      Logger.Channels.Add(FileLogger);
      Logger.Send(QuotedStrd(Application.Title)+' started [PID:'+IntToStr(GetProcessID)+']');
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
    SQLiteDefaultLibrary := FMD_DIRECTORY + Sqlite3Lib;
  {$ifdef windows}
  //openssl
  if IsSSLloaded then
    DestroySSLInterface;
  if FileExists(FMD_DIRECTORY+OpenSSLDLLSSLName) and FileExists(FMD_DIRECTORY+OpenSSLDLLUtilName) then
  begin
    DLLSSLName:=FMD_DIRECTORY+OpenSSLDLLSSLName;
    DLLUtilName:=FMD_DIRECTORY+OpenSSLDLLUtilName;
    if InitSSLInterface then
      SSLImplementation := TSSLOpenSSL;
  end
  else if FileExists(FMD_DIRECTORY+DLLSSLName) and FileExists(FMD_DIRECTORY+DLLUtilName) then
  begin
    DLLSSLName:=FMD_DIRECTORY+DLLSSLName;
    DLLUtilName:=FMD_DIRECTORY+DLLUtilName;
  end;
  if not IsSSLloaded then
    InitSSLInterface;
  {$endif}
  //webp
  if FileExists(FMD_DIRECTORY + DLLWebPName) then
    DLLWebPName := FMD_DIRECTORY + DLLWebPName;

  Application.Scaled := True;
  PreferredAppMode := pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
