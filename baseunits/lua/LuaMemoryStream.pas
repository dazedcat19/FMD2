unit LuaMemoryStream;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaClass,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaMemoryStreamAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses
  uBaseUnit, LuaUtils;

type
  TUserData = TMemoryStream;

function mem_toString(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, StreamToString(TUserData(luaClassGetObject(L))));
  Result := 1;
end;

function mem_writeString(L: Plua_State): Integer; cdecl;
var
  slen: size_t;
  s: PAnsiChar;
begin
  Result := 0;
  s := lua_tolstring(L, 1, @slen);
  TUserData(luaClassGetObject(L)).Write(s^, slen);
end;

function mem_loadFromFile(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).LoadFromFile(luaToString(L, 1));
end;

function mem_saveToFile(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).SaveToFile(luaToString(L, 1));
end;

function mem_getSize(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).Size);
  Result := 1;
end;

function mem_setSize(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Size := lua_tointeger(L, 1);
end;

function mem_clear(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Clear;
end;

const
  methods: packed array [0..6] of luaL_Reg = (
    (name: 'ToString'; func: @mem_tostring),
    (name: 'ReadString'; func: @mem_toString),
    (name: 'WriteString'; func: @mem_writeString),
    (name: 'LoadFromFile'; func: @mem_loadFromFile),
    (name: 'SaveToFile'; func: @mem_saveToFile),
    (name: 'Clear'; func: @mem_Clear),
    (name: nil; func: nil)
    );
  props: packed array[0..1] of luaL_Reg_prop = (
    (name: 'Size'; funcget: @mem_getSize; funcset: @mem_setSize),
    (name: nil; funcget: nil; funcset: nil)
    );

procedure luaMemoryStreamAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  with TUserData(Obj) do
  begin
    luaClassAddFunction(L, MetaTable, UserData, methods);
    luaClassAddProperty(L, MetaTable, UserData, props);
  end;
end;

initialization
  luaClassRegister(TMemoryStream, @luaMemoryStreamAddMetaTable);

end.
