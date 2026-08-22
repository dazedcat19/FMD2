unit LuaDuktape;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaPackage,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses
  Duktape, MultiLog, LuaUtils;

function lua_execjs(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  try
    lua_pushstring(L, Duktape.ExecJS(luaToString(L, 1)));
    Result := 1;
  except
    on E: Exception do
      Logger.SendError('Duktape.ExecJS() ' + E.Message);
  end;
end;

const
  methods: packed array [0..1] of luaL_Reg = (
    (name: 'ExecJS'; func: @lua_execjs),
    (name: nil; func: nil)
    );

function luaopen_duktape(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, methods);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('duktape', @luaopen_duktape);

end.

