unit LuaMangaFox;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaPackage,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses
  LuaUtils, MangaFoxWatermark;

function mf_loadtemplate(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, MangaFoxWatermark.LoadTemplate(luaToString(L, 1)));
  Result := 1;
end;

function mf_removewatermark(L: Plua_State): Integer; cdecl;
var
  ASaveAsPNG: Boolean;
begin
  ASaveAsPNG := False;
  if lua_gettop(L) = 2 then
    ASaveAsPNG := lua_toboolean(L, 2);
  lua_pushboolean(L, MangaFoxWatermark.RemoveWatermark(luaToString(L, 1), ASaveAsPNG));
  Result := 1;
end;

const
  methods: packed array [0..2] of luaL_Reg = (
    (name: 'LoadTemplate'; func: @mf_loadtemplate),
    (name: 'RemoveWatermark'; func: @mf_removewatermark),
    (name: nil; func: nil)
    );

function luaopen_mangafoxwatermark(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, methods);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('mangafoxwatermark', @luaopen_mangafoxwatermark);

end.
