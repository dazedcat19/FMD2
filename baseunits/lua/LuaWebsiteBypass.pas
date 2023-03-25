unit LuaWebsiteBypass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsendthread;

type

  { TWebsiteBypass }

  TWebsiteBypass = class
  public
    Module: Pointer;
    Guardian: TRTLCriticalSection;
    constructor Create(const AWebsiteModule: Pointer);
    destructor Destroy; override;
  end;

function WebsiteBypassRequest(const AHTTP: THTTPSendThread; const AMethod, AURL: String; const Response: TObject; const AWebsiteBypass: TWebsiteBypass): Boolean;

procedure doInitialization;

implementation

uses
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif},
  LuaWebsiteModules, LuaHandler, LuaBase, LuaUtils, LuaHTTPSend, LuaClass,
  WebsiteModules, FMDOptions, uBaseUnit, MultiLog;

var
  checkantibot_dump,
  websitebypass_dump: TMemoryStream;

  checkantibot_file: String = 'checkantibot.lua';
  websitebypass_file: String = 'websitebypass.lua';

  checkantibot_state: Plua_State;
  checkantibot_cs: TRTLCriticalSection;
  checkantibot_ref: Integer;
  checkantibot_count: Cardinal;

procedure doInitialization;
begin
  checkantibot_file := LUA_WEBSITEBYPASS_FOLDER + checkantibot_file;
  websitebypass_file := LUA_WEBSITEBYPASS_FOLDER + websitebypass_file;

  if FileExists(websitebypass_file) then
    websitebypass_dump := LuaDumpFileToStream(websitebypass_file);
  if FileExists(checkantibot_file) then
  begin
    checkantibot_state := LuaNewBaseState;
    checkantibot_dump := LuaDumpFileToStream(checkantibot_state, checkantibot_file);
    if checkantibot_dump<>nil then
    begin
      if (lua_pcall(checkantibot_state, 0, 0, 0) = 0) and
        (lua_getglobal(checkantibot_state, '____CheckAntiBot') = LUA_TFUNCTION)then
      begin
        InitCriticalSection(checkantibot_cs);
        checkantibot_ref := luaL_ref(checkantibot_state, LUA_REGISTRYINDEX);
        checkantibot_count := 0;
      end
      else
      begin
        lua_close(checkantibot_state);
        checkantibot_dump.Free;
        checkantibot_dump:=nil;
      end;
    end
    else
      lua_close(checkantibot_state);
  end;
end;

procedure doFinalization;
begin
  if Assigned(checkantibot_dump) then
    checkantibot_dump.Free;
  if Assigned(websitebypass_dump) then
  begin
    DoneCriticalSection(checkantibot_cs);
    websitebypass_dump.Free;
    lua_close(checkantibot_state);
  end;
end;

function CheckAntiBotActive(const AHTTP: THTTPSendThread): Boolean;
var
  r: Integer;
begin
  Result := False;
  EnterCriticalSection(checkantibot_cs);
  try
    if lua_rawgeti(checkantibot_state, LUA_REGISTRYINDEX, checkantibot_ref) <> LUA_TFUNCTION then Exit;
    if checkantibot_count > 31 then
    begin
      lua_gc(checkantibot_state, LUA_GCCOLLECT, 0);
      lua_gc(checkantibot_state, LUA_GCCOLLECT, 0);
      checkantibot_count := 0;
    end;
    luaClassPushUserData(checkantibot_state, AHTTP, '', False, @luaHTTPSendThreadAddMetaTable);
    r := lua_pcall(checkantibot_state, 1, LUA_MULTRET, 0);
    if r <> 0 then
      raise Exception.Create(LuaGetReturnString(r)+': '+luaToString(checkantibot_state, -1));
    if lua_gettop(checkantibot_state) > 0 then
      Result := lua_toboolean(checkantibot_state, 1);
    lua_settop(checkantibot_state, 0);
    Inc(checkantibot_count);
  except
    on E: Exception do
      SendLogException('CheckAntiBot.Error', E);
  end;
  LeaveCriticalSection(checkantibot_cs);
end;

function WebsiteBypassGetAnswer(const L: TLuaHandler; const AMethod, AURL: String): Boolean;
var
  r: Integer;
begin
  Result := False;
  if L.LoadChunkExecute(websitebypass_file, websitebypass_dump) = 0 then
  try
    lua_getglobal(L.Handle, '____WebsiteBypass');
    if lua_isnoneornil(L.Handle, -1) then Exit;

    lua_pushstring(L.Handle, AMethod);
    lua_pushstring(L.Handle, AURL);

    r := lua_pcall(L.Handle, 2, LUA_MULTRET, 0); // call with all params
    if r <> 0 then
      raise Exception.Create(LuaGetReturnString(r)+': '+luaToString(L.Handle, -1));
    Result := lua_toboolean(L.Handle, 1);
  except
    on E: Exception do
      SendLogException('WebsiteBypass.Error', E);
  end;
  L.ClearStack;
end;

function WebsiteBypassRequest(const AHTTP: THTTPSendThread; const AMethod, AURL: String; const Response: TObject; const AWebsiteBypass: TWebsiteBypass): Boolean;
var
  localLuaHandler,
  L: TLuaHandler;
  host, link: String;
  m: TModuleContainer;
begin
  if (checkantibot_dump = nil) or (websitebypass_dump = nil) then
  begin
    Result := AHTTP.HTTPRequest(AMethod, AURL);
    Exit;
  end;

  Result := False;
  AHTTP.AllowServerErrorResponse := True;
  Result := AHTTP.HTTPRequest(AMethod, AURL);

  if CheckAntiBotActive(AHTTP) then
  begin
    if TryEnterCriticalsection(AWebsiteBypass.Guardian) > 0 then
    begin
      localLuaHandler := nil;
      if AHTTP.LuaHandler <> nil then
        L := TLuaHandler(AHTTP.LuaHandler)
      else
      begin
        localLuaHandler := TLuaHandler.Create;
        L := localLuaHandler;
      end;
      L.LoadObject('HTTP', AHTTP, @luaHTTPSendThreadAddMetaTable);

      try
        SplitURL(AURL, @host, @link);
        host := LowerCase(host);
        m := Modules.LocateModuleByHost(Host);
        m.Settings.HTTP.Cookies := '';
        Result := WebsiteBypassGetAnswer(L, AMethod, AURL);
        if Result then
        begin
          m.Settings.Enabled := True;
          m.Settings.HTTP.Cookies := StringReplace(AHTTP.Cookies.Text, #13#10, ';', [rfReplaceAll, rfIgnoreCase]);
          m.Settings.HTTP.UserAgent := AHTTP.UserAgent;
          if TLuaWebsiteModule(TModuleContainer(m).LuaModule).Storage['reload'].Contains('true') then
          begin
            AHTTP.Reset();
            Result := AHTTP.HTTPRequest(AMethod, AURL);
          end;
        end;
      finally
        LeaveCriticalsection(AWebsiteBypass.Guardian);
      end;

      if localLuaHandler <> nil then
        localLuaHandler.Free;
    end
    else begin
      if not AHTTP.ThreadTerminated then
        Result := AHTTP.HTTPRequest(AMethod, AURL);
    end;
  end;

  if Assigned(Response) then
    if Response is TStringList then
      TStringList(Response).LoadFromStream(AHTTP.Document)
    else
    if Response is TStream then
      AHTTP.Document.SaveToStream(TStream(Response));
end;

{ TWebsiteBypass }

constructor TWebsiteBypass.Create(const AWebsiteModule: Pointer);
begin
  Module:=AWebsiteModule;
  InitCriticalSection(Guardian);
end;

destructor TWebsiteBypass.Destroy;
begin
  DoneCriticalsection(Guardian);
  inherited Destroy;
end;

finalization
  doFinalization;

end.
