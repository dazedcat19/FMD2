unit LuaFileUtil;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaPackage,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses
  LuaUtils, LazFileUtils, uBaseUnit;

function lua_extractfilename(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, ExtractFileName(luaToString(L, 1)));
  Result := 1;
end;

function lua_extractfilenameonly(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, ExtractFileNameOnly(luaToString(L, 1)));
  Result := 1;
end;

function lua_SerializeAndMaintainNames(L: Plua_State): Integer; cdecl;
begin
  if lua_isuserdata(L, 1) then
    SerializeAndMaintainNames(TStrings(luaToUserData(L, 1)));
  Result := 0;
end;

const
  methods: packed array [0..3] of luaL_Reg = (
    (name: 'ExtractFileName'; func: @lua_extractfilename),
    (name: 'ExtractFileNameOnly'; func: @lua_extractfilenameonly),
    (name: 'SerializeAndMaintainNames'; func: @lua_SerializeAndMaintainNames),
    (name: nil; func: nil)
    );

function luaopen_fileutil(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, methods);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('fileutil', @luaopen_fileutil);

end.

