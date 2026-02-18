unit LuaBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure LuaBaseRegisterBasic(const L: Plua_State); inline;
procedure LuaBaseRegisterCreateObject(const L: Plua_State); inline;
procedure LuaBaseRegisterAll(const L: Plua_State);

function LuaDoFile(const AFileName: String; const AFuncName: String = ''): Plua_State;
function LuaNewBaseState: Plua_State;
procedure LuaCallFunction(const L: Plua_State; const AFuncName: String);
function LuaGetReturnString(const ReturnCode: Integer): String;

function LuaDumpFileToStream(const AFileName: String): TMemoryStream; overload;
function LuaDumpFileToStream(const L: Plua_State; const AFileName: String): TMemoryStream; overload;
function LuaLoadFromStream(const L: Plua_State; const AStream: TMemoryStream; const AName: String): Integer; inline;
function LuaLoadFromStreamOrFile(const L: Plua_State; const AStream: TMemoryStream; const AFileName: String): Integer; inline;

procedure LuaExecute(const L: Plua_State; const AStream: TMemoryStream; const AFileName: String; const NResult: Integer = 0);

function _luawriter(L: Plua_State; const p: Pointer; sz: size_t; ud: Pointer): Integer; cdecl;

var
  AlwaysLoadLuaFromFile: Boolean = {$ifdef DEVBUILD}True{$else}False{$endif};

implementation

uses
  uBaseUnit,
  LuaPackage, LuaClass, LuaUtils,
  LuaCriticalSection, LuaMemoryStream,
  LuaBaseUnit, LuaSynaUtil,
  // -- lua object create
  LuaXQuery,
  // -- in lua 'require "name"'
  LuaFMD,
  LuaPCRE2,
  LuaDuktape,
  LuaCrypto,
  LuaFileUtil,
  LuaStrings,
  LuaImagePuzzle,
  LuaMangaFox,
  LuaLogger,
  LuaSubprocess;

function luabase_print(L: Plua_State): Integer; cdecl;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to lua_gettop(L) do
    case lua_type(L, i) of
      LUA_TBOOLEAN:
        SendLog(BoolToStr(lua_toboolean(L, i), 'true', 'false'));
      else
        SendLog(luaToString(L, i));
    end;
end;

function luabase_sleep(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  Sleep(lua_tointeger(L, 1));
end;

procedure LuaBaseRegisterBasic(const L: Plua_State);
begin
  luaPushFunctionGlobal(L, 'print', @luabase_print);
  luaPushFunctionGlobal(L, 'sleep', @luabase_sleep);
end;

procedure LuaBaseRegisterCreateObject(const L: Plua_State);
begin
  luaXQueryRegister(L);
end;

procedure LuaBaseRegisterAll(const L: Plua_State);
begin
  LuaBaseRegisterBasic(L);
  LuaBaseRegisterCreateObject(L);
  //luaClassRegisterAll(L); // empty right now

  luaBaseUnitRegister(L);
  luaSynaUtilRegister(L);
end;

function LuaDoFile(const AFileName: String; const AFuncName: String
  ): Plua_State;
var
  r: Integer;
begin
  Result := nil;
  if not FileExists(AFileName) then Exit;
  Result := luaL_newstate;
  try
    luaL_openlibs(Result);
    LuaBaseRegisterAll(Result);
    r := luaL_loadfile(Result, PAnsiChar(AFileName));
    if r <> 0 then
      raise Exception.Create('luaL_loadfile ' + LuaGetReturnString(r) + ': ' + luaToString(Result, -1));
    r := lua_pcall(Result, 0, LUA_MULTRET, 0);
    if r <> 0 then
      raise Exception.Create('lua_pcall ' + LuaGetReturnString(r) + ': ' + luaToString(Result, -1));
    if AFuncName <> '' then
      LuaCallFunction(Result, AFuncName);
  except
    on E: Exception do
      SendLogException('LuaDoFile()>', E);
  end;
end;

function LuaNewBaseState: Plua_State;
begin
  Result := luaL_newstate;
  try
    luaL_openlibs(Result);
    LuaBaseRegisterAll(Result);
    LuaPackage.RegisterLoader(Result);
  except
    on E: Exception do
      SendLogException('LuaNewBaseState()>', E);
  end;
end;

procedure LuaCallFunction(const L: Plua_State; const AFuncName: String);
var
  r: Integer;
begin
  lua_getglobal(L, PAnsiChar(AFuncName));
  if lua_isnoneornil(L, -1) then
  begin
    lua_settop(L, 1);
    raise Exception.Create('No function name ' + AnsiQuotedStr(AFuncName, '"'));
  end;
  r := lua_pcall(L, 0, LUA_MULTRET, 0);
  if r <> 0 then
    raise Exception.Create(Format('LuaCallFunction("%s") %s: %s', [AFuncName, LuaGetReturnString(r), luaToString(L, -1)]));
end;

function LuaGetReturnString(const ReturnCode: Integer): String;
begin
  case ReturnCode of
    0:             Result := 'LUA_OK';
    LUA_YIELD_:    Result := 'LUA_YIELD';
    LUA_ERRRUN:    Result := 'LUA_ERRRUN';
    LUA_ERRSYNTAX: Result := 'LUA_ERRSYNTAX';
    LUA_ERRMEM:    Result := 'LUA_ERRMEM';
    LUA_ERRERR:    Result := 'LUA_ERRERR';
    {$ifndef luajit}
    //LUA_ERRGCMM:   Result := 'LUA_ERRGCMM';
    LUA_ERRFILE:   Result := 'LUA_ERRFILE';
    {$endif}
    else
      Result := IntToStr(ReturnCode);
  end;
end;

function _luawriter(L: Plua_State; const p: Pointer; sz: size_t; ud: Pointer): Integer; cdecl;
begin
  if TMemoryStream(ud).Write(p^, sz) <> sz then
    Result := 1
  else
    Result := 0;
end;

function LuaDumpFileToStream(const AFileName: String): TMemoryStream;
var
  L: Plua_State;
begin
  if not FileExists(AFileName) then Exit;
  L := luaL_newstate;
  try
    luaL_openlibs(L);
    Result := LuaDumpFileToStream(L, AFileName);
  finally
    lua_close(L);
  end;
end;

function LuaDumpFileToStream(const L: Plua_State; const AFileName: String
  ): TMemoryStream;
var
  r: Integer;
begin
  Result := nil;
  if not FileExists(AFileName) then Exit;
  try
    r := luaL_loadfile(L, PAnsiChar(AFileName));
    if r <> 0 then
      raise Exception.Create('luaL_loadfile ' + LuaGetReturnString(r) + ': ' + luaToString(L, -1));
    // disabled strip to print line info
    Result := TMemoryStream.Create;
    r := lua_dump(L, @_luawriter, Result{$ifndef luajit}, 0{$endif});
    if r <> 0 then
    begin
      FreeAndNil(Result);
      raise Exception.Create('lua_dump ' + LuaGetReturnString(r) + ': ' + luaToString(L, -1));
    end;
  except
    on E: Exception do
      SendLogException('LuaDumpFileToStream()>', E);
  end;
end;

function LuaLoadFromStream(const L: Plua_State; const AStream: TMemoryStream;
  const AName: String): Integer;
begin
  Result := luaL_loadbuffer(L, AStream.Memory, AStream.Size, PAnsiChar(AName));
end;

function LuaLoadFromStreamOrFile(const L: Plua_State;
  const AStream: TMemoryStream; const AFileName: String): Integer;
begin
  if (not AlwaysLoadLuaFromFile) and (AStream <> nil) then
  begin
    Result := luaL_loadbuffer(L, AStream.Memory, AStream.Size, PAnsiChar(AFileName));
  end
  else
  begin
    Result := luaL_loadfile(L, PAnsiChar(AFileName));
  end;
end;

procedure LuaExecute(const L: Plua_State; const AStream: TMemoryStream;
  const AFileName: String; const NResult: Integer);
var
  r: Integer;
begin
  r := LuaLoadFromStreamOrFile(L, AStream, AFileName);
  if r <> 0 then
    raise Exception.Create(Format('luaL_loadfile() %s: %s', [LuaGetReturnString(r), luaToString(L, -1)]));
  r := lua_pcall(L, 0, NResult, 0);
  if r <> 0 then
    raise Exception.Create(Format('lua_pcall %s: %s', [LuaGetReturnString(r), luaToString(L, -1)]));
end;

end.
