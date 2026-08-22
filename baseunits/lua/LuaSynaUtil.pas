unit LuaSynaUtil;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, synautil,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaSynaUtilRegister(L: Plua_State); inline;

implementation

uses
  LuaUtils;

function lua_getbetween(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, GetBetween(luaToString(L, 1), luaToString(L, 2), luaToString(L, 3)));
  Result := 1;
end;

function lua_separateleft(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SeparateLeft(luaToString(L, 1), luaToString(L, 2)));
  Result := 1;
end;

function lua_separateright(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SeparateRight(luaToString(L, 1), luaToString(L, 2)));
  Result := 1;
end;

const
  methods: packed array [0..3] of luaL_Reg = (
    (name: 'GetBetween'; func: @lua_getbetween),
    (name: 'SeparateLeft'; func: @lua_separateleft),
    (name: 'SeparateRight'; func: @lua_separateright),
    (name: nil; func: nil)
    );

procedure luaSynaUtilRegister(L: Plua_State);
begin
  luaNewLib(L, methods);
end;

end.

