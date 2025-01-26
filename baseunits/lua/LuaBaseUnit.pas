unit LuaBaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaBaseUnitRegister(L: Plua_State); inline;

implementation

uses
  LuaUtils, uBaseUnit, httpsendthread, LazFileUtils, dateutils;

function lua_trim(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop(L) > 1 then
    lua_pushstring(L, luaToString(L, 1).Trim(luaToString(L, 2).ToCharArray))
  else
    lua_pushstring(L, Trim(luaToString(L, 1)));
  Result := 1;
end;

function lua_maybefillhost(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MaybeFillHost(luaToString(L, 1), luaToString(L, 2)));
  Result := 1;
end;

function lua_mangainfostatusifpos(L: Plua_State): Integer; cdecl;
begin
  case lua_gettop(L) of
    5: lua_pushstring(L, MangaInfoStatusIfPos(luaToString(L, 1),
        luaToString(L, 2), luaToString(L, 3), luaToString(L, 4),
        luaToString(L, 5)));
    4: lua_pushstring(L, MangaInfoStatusIfPos(luaToString(L, 1),
        luaToString(L, 2), luaToString(L, 3), luaToString(L, 4)));
    3: lua_pushstring(L, MangaInfoStatusIfPos(luaToString(L, 1),
        luaToString(L, 2), luaToString(L, 3)));
    2: lua_pushstring(L, MangaInfoStatusIfPos(luaToString(L, 1), luaToString(L, 2)));
    1: lua_pushstring(L, MangaInfoStatusIfPos(luaToString(L, 1)));
    else
      Exit(0);
  end;
  Result := 1;
end;

const
  methods: packed array [0..3] of luaL_Reg = (
    (name: 'Trim'; func: @lua_trim),
    (name: 'MaybeFillHost'; func: @lua_maybefillhost),
    (name: 'MangaInfoStatusIfPos'; func: @lua_mangainfostatusifpos),
    (name: nil; func: nil)
    );

procedure luaBaseUnitRegister(L: Plua_State);
begin
  luaNewLib(L, methods);
end;

end.
