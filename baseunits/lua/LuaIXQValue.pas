unit LuaIXQValue;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, xquery, LuaClass,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

type
  { TLuaIXQValue }

  TLuaIXQValue = class
  public
    Current: Cardinal;
    FIXQValue: IXQValue;
    constructor Create(const AIX: IXQValue);
  end;

procedure luaIXQValuePush(const L: Plua_State; const Obj: IXQValue); inline;

implementation

uses
  LuaUtils;

type
  TUserData = TLuaIXQValue;

{ TLuaIXQValue }

constructor TLuaIXQValue.Create(const AIX: IXQValue);
begin
  FIXQValue := AIX;
end;

function ixqvalue_tostring(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).FIXQValue.toString);
  Result := 1;
end;

function ixqvalue_getattribute(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).FIXQValue.toNode.getAttribute(
    luaToString(L, 1)));
  Result := 1;
end;

function ixqvalue_getproperty(L: Plua_State): Integer; cdecl;
begin
  luaIXQValuePush(L, TUserData(luaClassGetObject(L)).FIXQValue.getProperty(luaToString(L, 1)));
  Result := 1;
end;

function ixqvalue_innerHTML(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).FIXQValue.toNode.innerHTML());
  Result := 1;
end;

function ixqvalue_outerHTML(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).FIXQValue.toNode.outerHTML());
  Result := 1;
end;

function ixqvalue_innerText(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).FIXQValue.toNode.innerText());
  Result := 1;
end;

function ixqvalue_count(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).FIXQValue.Count);
  Result := 1;
end;

function ixqvalue_getnil(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
end;

function ixqvalue_geti(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
  v: IXQValue;
begin
  Result := 0;
  //lua_rawgeti(L, LUA_REGISTRYINDEX, lua_tointeger(L, lua_upvalueindex(1)));
  lua_rawgeti(L, LUA_REGISTRYINDEX, lua_tointeger(L, 1));
  u := TUserData(luaToUserData(L, -1));
  Inc(u.Current);
  if u.Current > u.FIXQValue.Count then
  begin
    luaL_unref(L, LUA_REGISTRYINDEX, lua_tointeger(L, 1));
    Exit;
  end;
  v := u.FIXQValue.get(u.Current);
  if v.kind = pvkUndefined then
  begin
    luaL_unref(L, LUA_REGISTRYINDEX, lua_tointeger(L, 1));
    Exit;
  end;
  luaIXQValuePush(L, v);
  Result := 1;
end;

function ixqvalue_get(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
begin
  if lua_gettop(L) = 0 then // return iterator
  begin
     u := TUserData(luaClassGetObject(L));
     if u.FIXQValue.Count = 0 then
     begin
       lua_pushcfunction(L, @ixqvalue_getnil);
       Exit(1);
     end
     else
     begin
       lua_pushcfunction(L, @ixqvalue_geti);
       lua_pushvalue(L, lua_upvalueindex(1)); // userdata
       lua_pushinteger(L, luaL_ref(L, LUA_REGISTRYINDEX));
       Exit(2);
     end;
  end;
  luaIXQValuePush(L, TUserData(luaClassGetObject(L)).FIXQValue.get(lua_tointeger(L, 1)));
  Result := 1;
end;

const
  methods: packed array [0..7] of luaL_Reg = (
    (name: 'Get'; func: @ixqvalue_get),
    (name: 'GetAttribute'; func: @ixqvalue_getattribute),
    (name: 'GetProperty'; func: @ixqvalue_getproperty),
    (name: 'InnerHTML'; func: @ixqvalue_innerHTML),
    (name: 'OuterHTML'; func: @ixqvalue_outerHTML),
    (name: 'InnerText'; func: @ixqvalue_innerText),
    (name: 'ToString'; func: @ixqvalue_tostring),
    (name: nil; func: nil)
    );

  props: packed array[0..1] of lual_Reg_prop = (
    (name: 'Count'; funcget: @ixqvalue_count; funcset: nil),
    (name: nil; funcget: nil; funcset: nil)
    );

procedure luaIXQValueAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
  luaClassAddProperty(L, MetaTable, UserData, props);
end;

procedure luaIXQValuePush(const L: Plua_State; const Obj: IXQValue);
begin
  luaClassPushObject(L, TLuaIXQValue.Create(Obj), '', True, @luaIXQValueAddMetaTable);
end;

initialization
  luaClassRegister(TLuaIXQValue, @luaIXQValueAddMetaTable);

end.
