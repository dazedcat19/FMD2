unit LuaFileUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses LuaUtils, LuaPackage, LazFileUtils, uBaseUnit;

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

function lua_Merge2Image(L: Plua_State): Integer; cdecl;
var
  Directory, ImgName1, ImgName2, FinalName: String;
  Landscape: Boolean;
begin
  // Retrieve string arguments
  Directory := luaToString(L, 1);
  ImgName1 := luaToString(L, 2);
  ImgName2 := luaToString(L, 3);
  FinalName := luaToString(L, 4);
  
  // Retrieve optional boolean argument (default to false if not provided)
  Landscape := lua_toboolean(L, 5); 

  // Call the uBaseUnit function and push the boolean result back to Lua
  lua_pushboolean(L, Merge2Image(Directory, ImgName1, ImgName2, FinalName, Landscape));
  Result := 1;
end;

const
  methods: packed array [0..4] of luaL_Reg = (
    (name: 'ExtractFileName'; func: @lua_extractfilename),
    (name: 'ExtractFileNameOnly'; func: @lua_extractfilenameonly),
    (name: 'SerializeAndMaintainNames'; func: @lua_SerializeAndMaintainNames),
    (name: 'Merge2Image'; func: @lua_Merge2Image),
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