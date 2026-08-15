unit LuaSubprocess;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Process, LuaPackage,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaSubprocessAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses
  LuaClass, LuaUtils;

type
  TUserData = TProcess;

function subprocess_create(L: Plua_State): Integer; cdecl;
begin
  luaClassPushObject(L, TProcess.Create(nil), '', True, @luaSubprocessAddMetaTable);
  Result := 1;
end;

Const
  ForbiddenOptions = [poRunSuspended,poWaitOnExit];

function _runcommand(L:Plua_State;Options:TProcessOptions=[];SWOptions:TShowWindowOptions=swoNone): Integer;
var
  p : TProcess;
  i,
  ExitStatus : integer;
  OutputString : String;
  ErrorString : String;
  presult: Boolean;
begin
  OutputString:='';
  ErrorString:='';
  ExitCode:=0;
  presult:=False;

  p:=DefaultTProcess.create(nil);
  if Options<>[] then
    P.Options:=Options - ForbiddenOptions;
  P.ShowWindow:=SwOptions;
  p.Executable:=luaToString(L,1);
  if lua_gettop(L)>1 then
    for i:=2 to lua_gettop(L) do
      p.Parameters.Add(luaToString(L,i));
  try
    presult:=p.RunCommandLoop(OutputString,ErrorString,ExitStatus)=0;
  finally
    p.free;
  end;
  if exitstatus<>0 then presult:=false;
  lua_pushboolean(L,presult);
  lua_pushstring(L,OutputString);
  lua_pushstring(L,ErrorString);
  lua_pushinteger(L,ExitStatus);
  Result:=4;
end;

function subprocess_runcommand(L: Plua_State): Integer; cdecl;
begin
  Result:=_runcommand(L);
end;

function subprocess_runcommandhide(L: Plua_State): Integer; cdecl;
begin
  Result:=_runcommand(L, [], swoHIDE);
end;

const
  constructs: packed array [0..4] of luaL_Reg = (
    (name: 'New'; func: @subprocess_create),
    (name: 'Create'; func: @subprocess_create),
    (name: 'RunCommand'; func: @subprocess_runcommand),
    (name: 'RunCommandHide'; func: @subprocess_runcommandhide),
    (name: nil; func: nil)
    );

procedure luaSubprocessAddMetaTable(const L: Plua_State; const Obj: Pointer; const MetaTable,
  UserData: Integer);
begin

end;

function luaopen_subprocess(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, constructs);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('subprocess', @luaopen_subprocess);

end.
