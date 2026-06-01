unit LuaQuickJS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses QuickJS, MultiLog, LuaUtils, LuaPackage;

function lua_execjs(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  try
    lua_pushstring(L, QuickJS.ExecJS(luaToString(L, 1)));
    Result := 1;
  except
    on E: Exception do
      Logger.SendError('QuickJS.ExecJS() ' + E.Message);
  end;
end;

const
  methods: packed array [0..1] of luaL_Reg = (
    (name: 'ExecJS'; func: @lua_execjs),
    (name: nil; func: nil)
    );

function luaopen_quickjs(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, methods);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('quickjs', @luaopen_quickjs); 

end.