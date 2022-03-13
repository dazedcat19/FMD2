unit LuaUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif}, LuaClass;

procedure luaNewLib(const L: Plua_State; const lr: PluaL_Reg); inline;
function luaNewLibTable(const L: Plua_State; const lr: PluaL_Reg; const Table: Integer = -1): Integer; overload; inline;
function luaNewLibTable(const L: Plua_State; const alr: array of PluaL_Reg): Integer; overload; inline;

function luaNewTable(const L: Plua_State): Integer; inline;
procedure luaAddCFunctionToTable(const L: Plua_State; const Table: Integer;
  const Name: String; const Func: lua_CFunction); inline;
procedure luaAddCClosureToTable(const L: Plua_State; const Table, Value: Integer;
  const Name: String; const Func: lua_CFunction); overload; inline;
procedure luaAddCClosureToTable(const L: Plua_State; const Table: Integer; const Value: Pointer;
  const Name: String; const Func: lua_CFunction); overload; inline;
procedure luaAddStringToTable(const L: Plua_State; const Table: Integer; const Name, Value: String); inline;
procedure luaAddIntegerToTable(const L: Plua_State; const Table: Integer; const Name: String; const Value: lua_Integer); inline;
procedure luaAddBooleanToTable(const L: Plua_State; const Table: Integer; const Name: String; const Value: Boolean); inline;

procedure luaPushFunctionGlobal(const L: Plua_State; const Name: String; const Func: lua_CFunction); inline;

procedure luaPushStringGlobal(const L: Plua_State; const Name, Value: String); inline;
procedure luaPushIntegerGlobal(const L: Plua_State; const Name: String; const Value: lua_Integer); inline;
procedure luaPushBooleanGlobal(const L: Plua_State; const Name: String; const Value: Boolean); inline;
procedure luaPushObjectGlobal(const L: Plua_State; const AObj: TObject; const AName: String;
  const AddMetaTable: TluaClassAddMetaTable = nil; const AutoFree: Boolean = False); inline;

procedure luaPushUserData(const L: Plua_State; const Value: Pointer); overload; inline;
procedure luaPushUserData(const L: Plua_State; const Value: Pointer; var UIndex: Integer); overload; inline;
function luaToUserData(const L: Plua_State; const idx: Integer): Pointer; inline;
function luaToString(const L: Plua_State; const idx: Integer): String; inline;

function LuaToTypeString(const L: Plua_State; const idx: Integer): String;
function LuaStackToString(const L: Plua_State): String;

procedure luaL_newlib(const L: Plua_State; const Name: String; const lr: PluaL_Reg); overload; inline;

// deprecated since 5.2
procedure luaL_openlib(const L: Plua_State; const Name: String; const lr: PluaL_Reg;
  const {%H-}nup: Integer); inline;
procedure luaL_register(const L: Plua_State; const Name: String; const lr: PluaL_Reg); inline;

implementation

uses MultiLog;

procedure luaNewLib(const L: Plua_State; const lr: PluaL_Reg);
var
  p: PluaL_Reg;
begin
  p := lr;
  while p^.name <> nil do
  begin
    luaPushFunctionGlobal(L, P^.name, p^.func);
    Inc(p);
  end;
end;

function luaNewLibTable(const L: Plua_State; const lr: PluaL_Reg; const Table: Integer): Integer;
var
  p: PluaL_Reg;
begin
  if Table = -1 then
    Result := luaNewTable(L)
  else
    Result := Table;
  p := lr;
  while p^.name <> nil do
  begin
    luaAddCFunctionToTable(L, Result, p^.name, p^.func);
    Inc(p);
  end;
end;

function luaNewLibTable(const L: Plua_State; const alr: array of PluaL_Reg): Integer;
var
  i: PluaL_Reg;
begin
  Result := luaNewTable(L);
  for i in alr do
    luaNewLibTable(L, i, Result);
end;

function luaNewTable(const L: Plua_State): Integer;
begin
  lua_newtable(L);
  Result := lua_gettop(L);
end;

procedure luaAddCFunctionToTable(const L: Plua_State; const Table: Integer;
  const Name: String; const Func: lua_CFunction);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushcfunction(L, Func);
  lua_rawset(L, Table);
end;

procedure luaAddCClosureToTable(const L: Plua_State; const Table,
  Value: Integer; const Name: String; const Func: lua_CFunction);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushvalue(L, Value);
  lua_pushcclosure(L, Func, 1);
  lua_rawset(L, Table);
end;

procedure luaAddCClosureToTable(const L: Plua_State; const Table: Integer;
  const Value: Pointer; const Name: String; const Func: lua_CFunction);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushlightuserdata(L, Value);
  lua_pushcclosure(L, Func, 1);
  lua_rawset(L, Table);
end;

procedure luaAddStringToTable(const L: Plua_State; const Table: Integer;
  const Name, Value: String);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushstring(L, Value);
  lua_rawset(L, Table);
end;

procedure luaAddIntegerToTable(const L: Plua_State; const Table: Integer;
  const Name: String; const Value: lua_Integer);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushinteger(L, Value);
  lua_rawset(L, Table);
end;

procedure luaAddBooleanToTable(const L: Plua_State; const Table: Integer;
  const Name: String; const Value: Boolean);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Name);
  lua_pushboolean(L, Value);
  lua_rawset(L, Table);
end;

procedure luaPushFunctionGlobal(const L: Plua_State; const Name: String;
  const Func: lua_CFunction);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushcfunction(L, Func);
  lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaPushStringGlobal(const L: Plua_State; const Name, Value: String);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushstring(L, Value);
  lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaPushIntegerGlobal(const L: Plua_State; const Name: String;
  const Value: lua_Integer);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushinteger(L, Value);
  lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaPushBooleanGlobal(const L: Plua_State; const Name: String;
  const Value: Boolean);
begin
  {$ifdef dump_lua_api}Logger.Send(Name);{$endif}
  lua_pushboolean(L, Value);
  lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaPushObjectGlobal(const L: Plua_State; const AObj: TObject;
  const AName: String; const AddMetaTable: TluaClassAddMetaTable;
  const AutoFree: Boolean);
begin
  luaClassPushObject(L, AObj, AName, AutoFree, AddMetaTable);
end;

procedure luaPushUserData(const L: Plua_State; const Value: Pointer);
begin
  PPointer(lua_newuserdata(L, SizeOf(PPointer)))^ := Value;
end;

procedure luaPushUserData(const L: Plua_State; const Value: Pointer;
  var UIndex: Integer);
begin
  luaPushUserData(L, Value);
  UIndex := lua_gettop(L);
end;

function luaToUserData(const L: Plua_State; const idx: Integer): Pointer;
begin
  Result := PPointer(lua_touserdata(L, idx))^;
end;

function luaToString(const L: Plua_State; const idx: Integer): String;
var
  slen: size_t;
  P: PAnsiChar;
begin
  P := lua_tolstring(L, idx, @slen);
  Result := P;
end;

function LuaToTypeString(const L: Plua_State; const idx: Integer): String;
var
  t:Integer;
begin
  if lua_iscfunction(L, idx) then
    Result:='cfunction'#32#9 + HexStr(lua_topointer(L,idx))
  else
  begin
    t:=lua_type(L, idx);
    case t of
      LUA_TNONE          : Result := 'none';
      LUA_TNIL           : Result := 'nil';
      LUA_TBOOLEAN       : Result := 'boolean'#32#9 + BoolToStr(lua_toboolean(L, idx), True);
      LUA_TLIGHTUSERDATA : Result := 'lightsuserdata'#32#9 + HexStr(lua_topointer(L, idx));
      LUA_TNUMBER        : Result := 'number'#32#9 + FloatToStr(lua_tonumber(L, idx));
      LUA_TSTRING        : Result := 'string'#32#9 + AnsiQuotedStr(lua_tostring(L, idx),'"');
      LUA_TTABLE         : Result := 'table'#32#9 + HexStr(lua_topointer(L, idx));
      LUA_TFUNCTION      : Result := 'function'#32#9 + HexStr(lua_topointer(L, idx));
      LUA_TUSERDATA      : Result := 'userdata'#32#9 + HexStr(lua_topointer(L, idx));
      LUA_TTHREAD        : Result := 'thread'#32#9 + HexStr(lua_topointer(L, idx));
      {$ifndef luajit}LUA_NUMTAGS        : Result := 'numtags'#32#9 + HexStr(lua_topointer(L, idx));{$endif}
      else
                           Result := 'else'#32#9 + IntToStr(t) + ' = ' + HexStr(lua_topointer(L, idx));
    end;
  end;
end;

function LuaStackToString(const L: Plua_State): String;
var
  i: Integer;
begin
  Result := '';
  i := lua_gettop(L);
  if i = 0 then
    Exit('[empty stack]');
  for i := 1 to i do
    Result += Format('%.3d[%s]: %s'#13#10,[i, HexStr(lua_topointer(L, i)), LuaToTypeString(L,i)]);
  SetLength(Result, Length(Result) - 2);
end;

{$ifdef luajit}
procedure luaL_newlibtable(L: Plua_State; lr: array of luaL_Reg);
begin
   lua_createtable(L, 0, High(lr));
end;

procedure luaL_setfuncs(L: Plua_State; lr: PluaL_Reg; nup: Integer); cdecl; external LUA_LIB_NAME;

procedure luaL_newlibtable(L: Plua_State; lr: PluaL_Reg);
var
  n: Integer;
begin
  n := 0;
  while lr^.name <> nil do begin
     inc(n);
     inc(lr);
  end;
  lua_createtable(L, 0, n);
end;

procedure luaL_newlib(L: Plua_State; lr: PluaL_Reg);
begin
   luaL_newlibtable(L, lr);
   luaL_setfuncs(L, lr, 0);
end;
{$endif}

procedure luaL_newlib(const L: Plua_State; const Name: String;
  const lr: PluaL_Reg);
begin
  luaL_newlib(L, lr);
  if Name <> '' then
    lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaL_openlib(const L: Plua_State; const Name: String;
  const lr: PluaL_Reg; const nup: Integer);
begin
  luaL_setfuncs(L, lr, 0);
  if Name <> '' then
    lua_setglobal(L, PAnsiChar(Name));
end;

procedure luaL_register(const L: Plua_State; const Name: String;
  const lr: PluaL_Reg);
begin
  luaL_openlib(L, Name, lr, 0);
end;

end.
