unit LuaWebsiteModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif}, LuaStringsStorage, WebsiteModules, syncobjs;

type
  TLuaWebsiteModulesContainer = class;

  { TLuaWebsiteModule }

  TLuaWebsiteModule = class
  private
  public
    Module: TModuleContainer;
    OnBeforeUpdateList: String;
    OnAfterUpdateList: String;
    OnGetDirectoryPageNumber: String;
    OnGetNameAndLink: String;
    OnGetInfo: String;
    OnTaskStart: String;
    OnGetPageNumber: String;
    OnGetImageURL: String;
    OnBeforeDownloadImage: String;
    OnDownloadImage: String;
    OnSaveImage: String;
    OnAfterImageSaved: String;
    OnLogin: String;
    OnAccountState: String;
    Storage: TStringsStorage;
    LastUpdated: String;
    Container: TLuaWebsiteModulesContainer;

    Options: TStringList;

    constructor Create;
    destructor Destroy; override;

    function AddOption(const AName, ACaption: String; const AClass: TClass): Integer;
    procedure AddOptionCheckBox(const AName, ACaption: String; const ADefault: Boolean);
    procedure AddOptionEdit(const AName, ACaption: String; const ADefault: String);
    procedure AddOptionSpinEdit(const AName, ACaption: String; const ADefault: Integer);
    procedure AddOptionComboBox(const AName, ACaption, AItems: String; const ADefault: Integer);

    procedure LuaPushMe(const L: Plua_State); inline;
    procedure LuaPushNetStatus(const L: Plua_State);
    procedure LuaPushAccountStatus(const L: Plua_State);

    procedure LuaDoMe(const L: Plua_State); inline;
  end;

  TLuaWebsiteModules = specialize TFPGList<TLuaWebsiteModule>;

  { TLuaWebsiteModulesContainer }

  TLuaWebsiteModulesContainer = class
  private
    FGuardian: TRTLCriticalSection;
    FByteCode: TMemoryStream;
  public
    Modules: TLuaWebsiteModules;
    FileName: String;
    constructor Create;
    destructor Destroy; override;
    function ByteCode: TMemoryStream;
  end;

  TLuaWebsiteModulesContainers = specialize TFPGList<TLuaWebsiteModulesContainer>;

  { TLuaWebsiteModulesManager }

  TLuaWebsiteModulesManager = class
  public
    Containers: TLuaWebsiteModulesContainers;
    constructor Create;
    destructor Destroy; override;
  end;

  { TLuaWebsiteModulesLoader }

  TLuaWebsiteModulesLoader = class
  private
    FFileList:TStringList;
    FThreadCount,
    FFileListIndex:Integer;
    FFileListGuardian,
    FMainGuardian:TCriticalSection;
  protected
    function GetFileName:String;
  public
    procedure ScanAndLoadFiles;
    constructor Create;
    destructor Destroy; override;
  end;

  { TLuaWebsiteModulesLoaderThread }

  TLuaWebsiteModulesLoaderThread = class(TThread)
  private
    FOwner: TLuaWebsiteModulesLoader;
  protected
    procedure LoadLuaWebsiteModule(const AFileName:String);
    procedure Execute; override;
  public
    constructor Create(const AOwner: TLuaWebsiteModulesLoader);
    destructor Destroy; override;
  end;

  TOptionItem = class
    Caption: String;
  end;

  TOptionItemCheckBox = class(TOptionItem)
    Value: Boolean;
  end;

  TOptionItemEdit = class(TOptionItem)
    Value: String;
  end;

  TOptionItemSpinEdit = class(TOptionItem)
    Value: Integer;
  end;

  TOptionItemComboBox = class(TOptionItem)
    Items: String;
    Value: Integer;
  end;

procedure ScanLuaWebsiteModulesFile;

procedure luaWebsiteModuleAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

var
  LuaWebsiteModulesManager: TLuaWebsiteModulesManager;

implementation

uses
  FMDOptions, FileUtil, MultiLog, LuaClass, LuaBase, LuaMangaInfo, LuaHTTPSend,
  LuaXQuery, LuaUtils, LuaDownloadTask, LuaUpdateListManager, LuaStrings,
  LuaCriticalSection, LuaPackage, uData,
  uDownloadsManager, xquery, httpsendthread, FMDVars, uBaseUnit, LuaWebsiteBypass,
  LuaWebsiteModuleHandler;

threadvar
  TempModules:TLuaWebsiteModules;

function DoBeforeUpdateList(const AModule: TModuleContainer): Boolean;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := False;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      L.LoadObject('UPDATELIST', updateList, @luaUpdateListManagerAddMetaTable);

      L.CallFunction(OnBeforeUpdateList);
      Result := lua_toboolean(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoBeforeUpdateList("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoAfterUpdateList(const AModule: TModuleContainer): Boolean;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := False;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      L.LoadObject('UPDATELIST', updateList, @luaUpdateListManagerAddMetaTable);

      L.CallFunction(OnAfterUpdateList);
      Result := lua_toboolean(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoAfterUpdateList("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoGetDirectoryPageNumber(const AUpdateListManager: TUpdateListManagerThread;
  const AHTTP: THTTPSendThread;
  var APage: Integer;
  const AWorkPtr: Integer;
  const AModule: TModuleContainer): Byte;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := INFORMATION_NOT_FOUND;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      LuaPushNetStatus(L.Handle);
      L.LoadObject('HTTP', AHTTP, @luaHTTPSendThreadAddMetaTable);
      L.LoadObject('UPDATELIST', AUpdateListManager, @luaUpdateListManagerAddMetaTable);
      luaPushIntegerGlobal(L.Handle, 'PAGENUMBER', APage);
      luaPushIntegerGlobal(L.Handle, 'WORKPTR', AWorkPtr);

      L.CallFunction(OnGetDirectoryPageNumber);
      Result := lua_tointeger(L.Handle, -1);
      lua_getglobal(L.Handle, 'PAGENUMBER');
      if not lua_isnoneornil(L.Handle, -1) then
        APage := lua_tointeger(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoGetDirectoryPageNumber("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoGetNameAndLink(const AUpdateListManager: TUpdateListManagerThread;
  const AHTTP: THTTPSendThread;
  const ANames, ALinks: TStringList; const AURL: String;
  const AModule: TModuleContainer): Byte;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := INFORMATION_NOT_FOUND;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      LuaPushNetStatus(L.Handle);
      L.LoadObject('HTTP', AHTTP, @luaHTTPSendThreadAddMetaTable);
      L.LoadObject('NAMES', ANames, @luaStringsAddMetaTable);
      L.LoadObject('LINKS', ALinks, @luaStringsAddMetaTable);
      L.LoadObject('UPDATELIST', AUpdateListManager, @luaUpdateListManagerAddMetaTable);
      luaPushStringGlobal(L.Handle, 'URL', AURL);

      L.CallFunction(OnGetNameAndLink);
      Result := lua_tointeger(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoGetNameAndLink("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoGetInfo(const AMangaInfo: TMangaInformation; const AURL: String;
  const AModule: TModuleContainer): Byte;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := INFORMATION_NOT_FOUND;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      LuaPushNetStatus(L.Handle);
      L.LoadObject('MANGAINFO', AMangaInfo.MangaInfo, @luaMangaInfoAddMetaTable);
      L.LoadObject('HTTP', AMangaInfo.HTTP, @luaHTTPSendThreadAddMetaTable);
      luaPushStringGlobal(L.Handle, 'URL', AURL);

      L.CallFunction(OnGetInfo);
      Result := lua_tointeger(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoGetInfo("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoTaskStart(const ATask: TTaskContainer; const AModule: TModuleContainer): Boolean;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := False;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      L.LoadObject('TASK', ATask, @luaDownloadTaskMetaTable);

      L.CallFunction(OnTaskStart);
      Result := lua_toboolean(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoTaskStart("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoGetPageNumber(const ATaskThread: TTaskThread;
  const AURL: String; const AModule: TModuleContainer): Boolean;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := False;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      L.LoadObject('TASK', ATaskThread.Container, @luaDownloadTaskMetaTable);
      L.LoadObject('HTTP', ATaskThread.HTTP, @luaHTTPSendThreadAddMetaTable);
      luaPushStringGlobal(L.Handle, 'URL', AURL);

      L.CallFunction(OnGetPageNumber);
      Result := lua_toboolean(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoGetPageNumber("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoGetImageURL(const ADownloadThread: TDownloadThread; const AURL: String;
  const AModule: TModuleContainer): Boolean;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := False;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      L.LoadObject('TASK', ADownloadThread.Task.Container, @luaDownloadTaskMetaTable);
      L.LoadObject('HTTP', ADownloadThread.HTTP, @luaHTTPSendThreadAddMetaTable);
      luaPushIntegerGlobal(L.Handle, 'WORKID', ADownloadThread.WorkId);
      luaPushStringGlobal(L.Handle, 'URL', AURL);

      L.CallFunction(OnGetImageURL);
      Result := lua_toboolean(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoGetImageURL("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoBeforeDownloadImage(const ADownloadThread: TDownloadThread;
  var AURL: String; const AModule: TModuleContainer): Boolean;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := False;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      L.LoadObject('TASK', ADownloadThread.Task.Container, @luaDownloadTaskMetaTable);
      L.LoadObject('HTTP', ADownloadThread.HTTP, @luaHTTPSendThreadAddMetaTable);
      luaPushIntegerGlobal(L.Handle, 'WORKID', ADownloadThread.WorkId);
      luaPushStringGlobal(L.Handle, 'URL', AURL);

      L.CallFunction(OnBeforeDownloadImage);
      Result := lua_toboolean(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoBeforeDownloadImage("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoDownloadImage(const ADownloadThread: TDownloadThread;
  const AURL: String; const AModule: TModuleContainer): Boolean;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := False;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      L.LoadObject('TASK', ADownloadThread.Task.Container, @luaDownloadTaskMetaTable);
      L.LoadObject('HTTP', ADownloadThread.HTTP, @luaHTTPSendThreadAddMetaTable);
      luaPushIntegerGlobal(L.Handle, 'WORKID', ADownloadThread.WorkId);
      luaPushStringGlobal(L.Handle, 'URL', AURL);

      L.CallFunction(OnDownloadImage);
      Result := lua_toboolean(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoDownloadImage("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoSaveImage(const ADownloadThread: TDownloadThread; const APath, AName: String;
  const AModule: TModuleContainer): String;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := '';
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      L.LoadObject('HTTP', ADownloadThread.HTTP, @luaHTTPSendThreadAddMetaTable);
      luaPushStringGlobal(L.Handle, 'PATH', APath);
      luaPushStringGlobal(L.Handle, 'FILENAME', AName);

      L.CallFunction(OnSaveImage);
      Result := luaToString(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoSaveImage("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoAfterImageSaved(const ADownloadThread: TDownloadThread; const AFileName: String;
  const AModule: TModuleContainer): Boolean;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := False;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      luaPushStringGlobal(L.Handle, 'FILENAME', AFileName);

      L.CallFunction(OnAfterImageSaved);
      Result := lua_toboolean(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoAfterImageSaved("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoLogin(const AHTTP: THTTPSendThread; const AModule: TModuleContainer): Boolean;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := False;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      LuaPushAccountStatus(L.Handle);
      L.LoadObject('HTTP', AHTTP, @luaHTTPSendThreadAddMetaTable);

      L.CallFunction(OnLogin);
      Result := lua_toboolean(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoLogin("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function DoAccountState(const AModule: TModuleContainer): Boolean;
var
  L: TLuaWebsiteModuleHandler;
begin
  Result := False;
  with TLuaWebsiteModule(AModule.LuaModule) do
    try
      L := GetLuaWebsiteModuleHandler(AModule);
      LuaPushAccountStatus(L.Handle);

      L.CallFunction(OnAccountState);
      Result := lua_toboolean(L.Handle, -1);
    except
      on E: Exception do
        SendLogException('LUA>DoAccountState("' + ExtractFileName(Container.FileName) + '")>', E);
    end;
end;

function _newwebsitemodule(L: Plua_State): Integer; cdecl;
begin
  luaClassPushObject(L, TLuaWebsiteModule.Create, '', False, @luaWebsiteModuleAddMetaTable);
  Result := 1;
end;

function DoInit(const AFileName: String): Boolean;
var
  L: Plua_State;
  i: Integer;
begin
  Result := False;
  L := LuaNewBaseState;
  try
    i := luaL_loadfile(L, PAnsiChar(AFileName));
    if i <> 0 then
      raise Exception.Create('luaL_loadfile ' + LuaGetReturnString(i) + ': ' + luaToString(L, -1));
    i := lua_pcall(L, 0, 0, 0);
    if i <> 0 then
      raise Exception.Create('lua_pcall ' + LuaGetReturnString(i) + ': ' + luaToString(L, -1));
    lua_getglobal(L, PAnsiChar('Init'));
    if lua_isfunction(L, -1) = False then
      raise Exception.Create('no function name "Init()"');
    luaPushFunctionGlobal(L, 'NewWebsiteModule', @_newwebsitemodule);
    i := lua_pcall(L, 0, 0, 0);
    if i <> 0 then
      raise Exception.Create('LuaCallFunction("Init()") ' + LuaGetReturnString(i) + ': ' + luaToString(L, -1));
    Result := True
  except
    on E: Exception do
      SendLogException('LUA>DoInit("' + ExtractFileName(AFileName) + '")>', E);
  end;
  lua_close(L);
end;

procedure ScanLuaWebsiteModulesFile;
begin
  with TLuaWebsiteModulesLoader.Create do
    try
      ScanAndLoadFiles;
    finally
      Free;
    end;
end;

{ TLuaWebsiteModulesLoaderThread }

procedure TLuaWebsiteModulesLoaderThread.LoadLuaWebsiteModule(
  const AFileName: String);
var
  c: TLuaWebsiteModulesContainer;
  i: Integer;
  md: TModuleContainer;
begin
  DoInit(AFileName);

  // remove modules without id or name. it's used in ui without validation (EAccessViolation)
  for i := TempModules.Count-1 downto 0 do
  begin
    md := TempModules[i].Module;
    if (md.ID = '') or (md.Name = '') then
    begin
      TempModules[i].Module.Free;
      TempModules[i].Free;
      TempModules.Delete(i);
    end
  end;

  if TempModules.Count <> 0 then
    begin
      c := TLuaWebsiteModulesContainer.Create;
      c.FileName := AFileName;
      FOwner.FMainGuardian.Enter;
      try
        LuaWebsiteModulesManager.Containers.Add(c);
      finally
        FOwner.FMainGuardian.Leave;
      end;
      Modules.Lock;
      try
        for i := 0 to TempModules.Count - 1 do
        with TempModules[i] do
        begin
          Modules.List.Add(Module);
          c.Modules.Add(TempModules[i]);
          Container := c;
          Module.RootURL := LowerCase(Module.RootURL);
          if OnBeforeUpdateList <> '' then
            Module.OnBeforeUpdateList := @DoBeforeUpdateList;
          if OnAfterUpdateList <> '' then
            Module.OnAfterUpdateList := @DoAfterUpdateList;
          if OnGetDirectoryPageNumber <> '' then
            Module.OnGetDirectoryPageNumber := @DoGetDirectoryPageNumber;
          if OnGetNameAndLink <> '' then
            Module.OnGetNameAndLink := @DoGetNameAndLink;
          if OnGetInfo <> '' then
            Module.OnGetInfo := @DoGetInfo;
          if OnTaskStart <> '' then
            Module.OnTaskStart := @DoTaskStart;
          if OnGetPageNumber <> '' then
            Module.OnGetPageNumber := @DoGetPageNumber;
          if OnGetImageURL <> '' then
            Module.OnGetImageURL := @DoGetImageURL;
          if OnBeforeDownloadImage <> '' then
            Module.OnBeforeDownloadImage := @DoBeforeDownloadImage;
          if OnDownloadImage <> '' then
            Module.OnDownloadImage := @DoDownloadImage;
          if OnSaveImage <> '' then
            Module.OnSaveImage := @DoSaveImage;
          if OnAfterImageSaved <> '' then
            Module.OnAfterImageSaved := @DoAfterImageSaved;
          if OnLogin <> '' then
            Module.OnLogin := @DoLogin;
          if OnAccountState <> '' then
            Module.OnAccountState := @DoAccountState;
          Module.Checksite := LowerCase(Module.Checksite);
          Module.Checkchapter := LowerCase(Module.Checkchapter);
        end;
      finally
        Modules.Unlock;
      end;
      TempModules.Clear;
    end;
end;

procedure TLuaWebsiteModulesLoaderThread.Execute;
var
  f:String;
begin
  TempModules:=TLuaWebsiteModules.Create;
  try
    f:=FOwner.GetFileName;
    while f<>'' do begin
      LoadLuaWebsiteModule(f);
      f:=FOwner.GetFileName;
    end;
  except
    on E: Exception do
      SendLogException(ClassName+'.Execute.Error>', E);
  end;
  TempModules.Free;
end;

constructor TLuaWebsiteModulesLoaderThread.Create(
  const AOwner: TLuaWebsiteModulesLoader);
begin
  FOwner:=AOwner;
  InterLockedIncrement(FOwner.FThreadCount);
  FreeOnTerminate:=True;
  inherited Create(False);
end;

destructor TLuaWebsiteModulesLoaderThread.Destroy;
begin
  InterLockedDecrement(FOwner.FThreadCount);
  inherited Destroy;
end;

{ TLuaWebsiteModulesLoader }

function TLuaWebsiteModulesLoader.GetFileName: String;
begin
  if FFileListIndex>FFileList.Count-1 then Exit('');
  FFileListGuardian.Enter;
  Result:=FFileList[FFileListIndex];
  Inc(FFileListIndex);
  FFileListGuardian.Leave;
end;

procedure TLuaWebsiteModulesLoader.ScanAndLoadFiles;
var
  i: Integer;
  cpu_count: LongWord;
begin
  LuaWebsiteBypass.doInitialization;
  LuaPackage.LuaLibDir := LUA_REPO_FOLDER;

  FindAllFiles(FFileList, LUA_WEBSITEMODULE_FOLDER, '*.lua;*.luac', False, faAnyFile);
  if FFileList.Count = 0 then Exit;
  cpu_count := GetCPUCount;
  if cpu_count > FFileList.Count then
    cpu_count := FFileList.Count;
  for i:=1 to cpu_count do
    TLuaWebsiteModulesLoaderThread.Create(Self);
  while FThreadCount <> 0 do
    Sleep(250);

  LuaPackage.ClearCache;
  Modules.Sort;
end;

constructor TLuaWebsiteModulesLoader.Create;
begin
  FFileList:=TStringList.Create;
  FThreadCount:=0;
  FFileListIndex:=0;
  FFileListGuardian:=TCriticalSection.Create;
  FMainGuardian:=TCriticalSection.Create;
end;

destructor TLuaWebsiteModulesLoader.Destroy;
begin
  FFileList.Free;
  FFileListGuardian.Free;
  FMainGuardian.Free;
  inherited Destroy;
end;

{ TLuaWebsiteModulesManager }

constructor TLuaWebsiteModulesManager.Create;
begin
  Containers := TLuaWebsiteModulesContainers.Create;
end;

destructor TLuaWebsiteModulesManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to Containers.Count - 1 do
    Containers[i].Free;
  Containers.Free;
  inherited Destroy;
end;

{ TLuaWebsiteModulesContainer }

constructor TLuaWebsiteModulesContainer.Create;
begin
  InitCriticalSection(FGuardian);
  Modules := TLuaWebsiteModules.Create;
end;

destructor TLuaWebsiteModulesContainer.Destroy;
var
  i: Integer;
begin
  for i := 0 to Modules.Count - 1 do
    Modules[i].Free;
  Modules.Free;
  if FByteCode<>nil then
    FByteCode.Free;
  DoneCriticalSection(FGuardian);
  inherited Destroy;
end;

function TLuaWebsiteModulesContainer.ByteCode: TMemoryStream;
begin
  // don't need to cache lua file on --lua-dofile
  if AlwaysLoadLuaFromFile then Exit(nil);
  if FByteCode <> nil then Exit(FByteCode);
  if TryEnterCriticalSection(FGuardian) <> 0 then
  begin
    try
      FByteCode := LuaDumpFileToStream(FileName);
    finally
      LeaveCriticalSection(FGuardian);
    end
  end
  else
  begin
    // wait for dumfiletostream
    EnterCriticalSection(FGuardian);
    LeaveCriticalSection(FGuardian);
  end;
  Result := FByteCode;
end;


{ TLuaWebsiteModule }

constructor TLuaWebsiteModule.Create;
begin
  TempModules.Add(Self);
  Storage := TStringsStorage.Create;
  Options := TStringList.Create;
  Options.OwnsObjects := True;
  Options.Duplicates := dupIgnore;
  Options.Sorted := True;
  Module := TModuleContainer.Create;
  Module.LuaModule := Self;
end;

destructor TLuaWebsiteModule.Destroy;
begin
  Options.Free;
  Storage.Free;
  inherited Destroy;
end;

function TLuaWebsiteModule.AddOption(const AName, ACaption: String; const AClass: TClass): Integer;
var
  o: TOptionItem;
begin
  Result := Options.Add(AName);
  if Result = -1 then Exit;
  o := TOptionItem(AClass.Create);
  o.Caption := ACaption;
  Options.Objects[Result] := o;
end;

procedure TLuaWebsiteModule.AddOptionCheckBox(const AName, ACaption: String;
  const ADefault: Boolean);
var
  o: TOptionItemCheckBox;
  i: Integer;
begin
  i := AddOption(AName, ACaption, TOptionItemCheckBox);
  if i = -1 then Exit;
  o := TOptionItemCheckBox(Options.Objects[i]);
  o.Value := ADefault;
  Module.AddOptionCheckBox(@o.Value, Options[i], @o.Caption);
end;

procedure TLuaWebsiteModule.AddOptionEdit(const AName, ACaption: String; const ADefault: String);
var
  o: TOptionItemEdit;
  i: Integer;
begin
  i := AddOption(AName, ACaption, TOptionItemEdit);
  if i = -1 then Exit;
  o := TOptionItemEdit(Options.Objects[i]);
  o.Value := ADefault;
  Module.AddOptionEdit(@o.Value, Options[i], @o.Caption);
end;

procedure TLuaWebsiteModule.AddOptionSpinEdit(const AName, ACaption: String;
  const ADefault: Integer);
var
  o: TOptionItemSpinEdit;
  i: Integer;
begin
  i := AddOption(AName, ACaption, TOptionItemSpinEdit);
  if i = -1 then Exit;
  o := TOptionItemSpinEdit(Options.Objects[i]);
  o.Value := ADefault;
  Module.AddOptionSpinEdit(@o.Value, Options[i], @o.Caption);
end;

procedure TLuaWebsiteModule.AddOptionComboBox(const AName, ACaption, AItems: String;
  const ADefault: Integer);
var
  o: TOptionItemComboBox;
  i: Integer;
begin
  i := AddOption(AName, ACaption, TOptionItemComboBox);
  if i = -1 then Exit;
  o := TOptionItemComboBox(Options.Objects[i]);
  o.Items := AItems;
  o.Value := ADefault;
  Module.AddOptionComboBox(@o.Value, Options[i], @o.Caption, @o.Items);
end;

procedure TLuaWebsiteModule.LuaPushMe(const L: Plua_State);
begin
  luaPushObjectGlobal(L, Self, 'MODULE', @luaWebsiteModuleAddMetaTable);
end;

procedure TLuaWebsiteModule.LuaPushNetStatus(const L: Plua_State);
begin
  luaPushIntegerGlobal(L, 'no_error', NO_ERROR);
  luaPushIntegerGlobal(L, 'net_problem', NET_PROBLEM);
  luaPushIntegerGlobal(L, 'information_not_found', INFORMATION_NOT_FOUND);
end;

procedure TLuaWebsiteModule.LuaPushAccountStatus(const L: Plua_State);
begin
  luaPushIntegerGlobal(L, 'asUnknown', Integer(asUnknown));
  luaPushIntegerGlobal(L, 'asChecking', Integer(asChecking));
  luaPushIntegerGlobal(L, 'asValid', Integer(asValid));
  luaPushIntegerGlobal(L, 'asInvalid', Integer(asInvalid));
end;

procedure TLuaWebsiteModule.LuaDoMe(const L: Plua_State);
begin
  LuaExecute(L, Container.ByteCode, Container.FileName);
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

function lua_addoptioncheckbox(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).AddOptionCheckBox(
    luaToString(L, 1), luaToString(L, 2), lua_toboolean(L, 3));
end;

function lua_addoptionedit(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).AddOptionEdit(
    luaToString(L, 1), luaToString(L, 2), luaToString(L, 3));
end;

function lua_addoptionspinedit(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).AddOptionSpinEdit(
    luaToString(L, 1), luaToString(L, 2), lua_tointeger(L, 3));
end;

function lua_addoptioncombobox(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).AddOptionComboBox(
    luaToString(L, 1), luaToString(L, 2), luaToString(L, 3), lua_tointeger(L, 4));
end;

function lua_gettotaldirectory(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TLuaWebsiteModule(luaClassGetObject(L)).Module.TotalDirectory);
  Result := 1;
end;

function lua_settotaldirectory(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).Module.TotalDirectory := lua_tointeger(L, 1);
end;

function lua_addservercookies(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  if lua_gettop(L) = 2 then
    TLuaWebsiteModule(luaClassGetObject(L)).Module.CookieManager.AddServerCookies(luaToString(L, 1), luaToString(L, 2), Now)
  else
    TLuaWebsiteModule(luaClassGetObject(L)).Module.CookieManager.AddServerCookies('', luaToString(L, 1), Now);
end;

function lua_getservercookies(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop(L) = 2 then
    lua_pushstring(L, TLuaWebsiteModule(luaClassGetObject(L)).Module.CookieManager.GetServerCookies(luaToString(L, 1), luaToString(L, 2)))
  else
    lua_pushstring(L, TLuaWebsiteModule(luaClassGetObject(L)).Module.CookieManager.GetServerCookies(luaToString(L, 1), ''));
  Result := 1;
end;

function lua_removecookies(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  if lua_gettop(L) = 2 then
    TLuaWebsiteModule(luaClassGetObject(L)).Module.CookieManager.RemoveCookies(luaToString(L, 1), luaToString(L, 2))
  else
    TLuaWebsiteModule(luaClassGetObject(L)).Module.CookieManager.RemoveCookies(luaToString(L, 1), '');
end;

function lua_clearcookies(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).Module.CookieManager.Clear;
end;

function lua_getoption(L: Plua_State): Integer; cdecl;
var
  m: TLuaWebsiteModule;
  i: Integer;
  o: TObject;
begin
  m := TLuaWebsiteModule(luaClassGetObject(L));
  i:=m.Options.IndexOf(luaToString(L, 1));
  Result := 1;
  if i = -1 then
    lua_pushnil(L)
  else
  begin
    o := m.Options.Objects[i];
    if o is TOptionItemCheckBox then
      lua_pushboolean(L, TOptionItemCheckBox(o).Value)
    else
    if o is TOptionItemEdit then
      lua_pushstring(L, TOptionItemEdit(o).Value)
    else
    if o is TOptionItemSpinEdit then
      lua_pushinteger(L, TOptionItemSpinEdit(o).Value)
    else
    if o is TOptionItemComboBox then
      lua_pushinteger(L, TOptionItemComboBox(o).Value)
    else
      lua_pushnil(L);
  end;
end;

function lua_getaccountsupport(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TLuaWebsiteModule(luaClassGetObject(L)).Module.AccountSupport);
  Result := 1;
end;

function lua_setaccountsupport(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TLuaWebsiteModule(luaClassGetObject(L)).Module.AccountSupport := lua_toboolean(L, 1);
end;

const
  methods: packed array [0..9] of luaL_Reg = (
    (name: 'AddOptionCheckBox'; func: @lua_addoptioncheckbox),
    (name: 'AddOptionEdit'; func: @lua_addoptionedit),
    (name: 'AddOptionSpinEdit'; func: @lua_addoptionspinedit),
    (name: 'AddOptionComboBox'; func: @lua_addoptioncombobox),
    (name: 'AddServerCookies'; func: @lua_addservercookies),
    (name: 'GetServerCookies'; func: @lua_getservercookies),
    (name: 'RemoveCookies'; func: @lua_removecookies),
    (name: 'ClearCookies'; func: @lua_clearcookies),
    (name: 'GetOption'; func: @lua_getoption),
    (name: nil; func: nil)
    );

procedure luaWebsiteModuleAccountAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  with TWebsiteModuleAccount(Obj) do
  begin
    luaClassAddBooleanProperty(L, MetaTable, 'Enabled', @Enabled);
    luaClassAddStringProperty(L, MetaTable, 'Username', @Username);
    luaClassAddStringProperty(L, MetaTable, 'Password', @Password);
    luaClassAddIntegerProperty(L, MetaTable, 'Status', @Status);
    luaClassAddStringProperty(L, MetaTable, 'Cookies', @Cookies);
    luaClassAddObject(L, MetaTable, Guardian, 'Guardian', @luaCriticalSectionAddMetaTable);
  end;
end;

procedure luaWebsiteModuleAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  with TLuaWebsiteModule(Obj) do
  begin
    luaClassAddObject(L, MetaTable, Module.Guardian, 'Guardian', @luaCriticalSectionAddMetaTable);
    luaClassAddStringProperty(L, MetaTable, 'ID', @Module.ID);
    luaClassAddStringProperty(L, MetaTable, 'Name', @Module.Name);
    luaClassAddStringProperty(L, MetaTable, 'RootURL', @Module.RootURL);
    luaClassAddStringProperty(L, MetaTable, 'Category', @Module.Category);
    luaClassAddIntegerProperty(L, MetaTable, 'MaxTaskLimit', @Module.MaxTaskLimit);
    luaClassAddIntegerProperty(L, MetaTable, 'MaxThreadPerTaskLimit', @Module.MaxThreadPerTaskLimit);
    luaClassAddIntegerProperty(L, MetaTable, 'MaxConnectionLimit', @Module.ConnectionsQueue.MaxConnections);
    luaClassAddIntegerProperty(L, MetaTable, 'ActiveTaskCount', @Module.ActiveTaskCount);
    luaClassAddIntegerProperty(L, MetaTable, 'ActiveConnectionCount', @Module.ConnectionsQueue.ActiveConnections);
    luaClassAddBooleanProperty(L, MetaTable, 'SortedList', @Module.SortedList);
    luaClassAddBooleanProperty(L, MetaTable, 'InformationAvailable', @Module.InformationAvailable);
    luaClassAddBooleanProperty(L, MetaTable, 'FavoriteAvailable', @Module.FavoriteAvailable);
    luaClassAddBooleanProperty(L, MetaTable, 'DynamicPageLink', @Module.DynamicPageLink);
    luaClassAddStringProperty(L, MetaTable, 'OnBeforeUpdateList', @OnBeforeUpdateList);
    luaClassAddStringProperty(L, MetaTable, 'OnAfterUpdateList', @OnAfterUpdateList);
    luaClassAddStringProperty(L, MetaTable, 'OnGetDirectoryPageNumber', @OnGetDirectoryPageNumber);
    luaClassAddStringProperty(L, MetaTable, 'OnGetNameAndLink', @OnGetNameAndLink);
    luaClassAddStringProperty(L, MetaTable, 'OnGetInfo', @OnGetInfo);
    luaClassAddStringProperty(L, MetaTable, 'OnTaskStart', @OnTaskStart);
    luaClassAddStringProperty(L, MetaTable, 'OnGetPageNumber', @OnGetPageNumber);
    luaClassAddStringProperty(L, MetaTable, 'OnGetImageURL', @OnGetImageURL);
    luaClassAddStringProperty(L, MetaTable, 'OnBeforeDownloadImage', @OnBeforeDownloadImage);
    luaClassAddStringProperty(L, MetaTable, 'OnDownloadImage', @OnDownloadImage);
    luaClassAddStringProperty(L, MetaTable, 'OnSaveImage', @OnSaveImage);
    luaClassAddStringProperty(L, MetaTable, 'OnAfterImageSaved', @OnAfterImageSaved);
    luaClassAddStringProperty(L, MetaTable, 'OnLogin', @OnLogin);
    luaClassAddStringProperty(L, MetaTable, 'OnAccountState', @OnAccountState);
    luaClassAddStringProperty(L, MetaTable, 'LastUpdated', @LastUpdated);
    luaClassAddIntegerProperty(L, MetaTable, 'CurrentDirectoryIndex', @Module.CurrentDirectoryIndex);
	luaClassAddStringProperty(L, MetaTable, 'Checksite', @Module.Checksite);
	luaClassAddStringProperty(L, MetaTable, 'Checkchapter', @Module.Checkchapter);

    luaClassAddProperty(L, MetaTable, UserData, 'TotalDirectory', @lua_gettotaldirectory, @lua_settotaldirectory);
    luaClassAddProperty(L, MetaTable, UserData, 'AccountSupport', @lua_getaccountsupport, @lua_setaccountsupport);

    luaClassAddFunction(L, MetaTable, UserData, methods);

    luaClassAddObject(L, MetaTable, Storage, 'Storage', @luaStringsStorageAddMetaTable);

    if Module.Account<>nil then
      luaClassAddObject(L, MetaTable, Module.Account, 'Account', @luaWebsiteModuleAccountAddMetaTable);

    luaClassAddIntegerProperty(L, MetaTable, 'Tag', @Module.Tag);
  end;
end;

initialization
  LuaWebsiteModulesManager := TLuaWebsiteModulesManager.Create;

finalization
  LuaWebsiteModulesManager.Free;

end.
