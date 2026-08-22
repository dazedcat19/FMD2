unit LuaLogger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaPackage,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses
  MultiLog, LuaUtils;

function logger_send(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  Logger.Send(luaToString(L, 1));
end;

function logger_sendwarning(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  Logger.SendWarning(luaToString(L, 1));
end;

function logger_senderror(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  Logger.SendError(luaToString(L, 1));
end;

const
  methods: packed array [0..3] of luaL_Reg = (
    (name: 'Send'; func: @logger_send),
    (name: 'SendWarning'; func: @logger_sendwarning),
    (name: 'SendError'; func: @logger_senderror),
    (name: nil; func: nil)
    );

function luaopen_logger(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, methods);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('logger', @luaopen_logger);

end.
