unit LuaCriticalSection;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, syncobjs, LuaClass,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaCriticalSectionAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

type
  TUserData = TCriticalSection;

function lua_tryenter(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).TryEnter);
  Result := 1;
end;

function lua_enter(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Enter;
end;

function lua_Leave(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Leave;
end;

const
  methods: packed array [0..3] of luaL_Reg = (
    (name: 'TryEnter'; func: @lua_tryenter),
    (name: 'Enter'; func: @lua_enter),
    (name: 'Leave'; func: @lua_Leave),
    (name: nil; func: nil)
    );

procedure luaCriticalSectionAddMetaTable(const L: Plua_State;
  const Obj: Pointer; const MetaTable, UserData: Integer);
begin
  with TUserData(Obj) do
  begin
    luaClassAddFunction(L, MetaTable, UserData, methods);
  end;
end;

initialization
  luaClassRegister(TCriticalSection, @luaCriticalSectionAddMetaTable);

end.
