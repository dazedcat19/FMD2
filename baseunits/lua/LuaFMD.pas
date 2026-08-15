unit LuaFMD;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaPackage,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses
  LuaUtils, uOptions, SimpleTranslator;

function luaopen_fmd(L: Plua_State): Integer; cdecl;
var
  t: Integer;
begin
  t := luaNewTable(L);
  luaAddStringToTable(L, t, 'Directory', FMD_DIRECTORY);
  luaAddStringToTable(L, t, 'ExeName', FMD_EXENAME);
  luaAddStringToTable(L, t, 'Version', FMD_VERSION_STRING);
  luaAddStringToTable(L, t, 'Revision', REVISION_NUMBER);
  luaAddStringToTable(L, t, 'LuaDirectory', LUA_REPO_FOLDER);
  luaAddStringToTable(L, t, 'SelectedLanguage', SimpleTranslator.LastSelected);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('env', @luaopen_fmd);

end.
