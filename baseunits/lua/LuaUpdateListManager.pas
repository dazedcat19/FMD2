unit LuaUpdateListManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua55;

procedure luaUpdateListManagerAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses
  LuaClass, LuaUtils, uUpdateThread;

function lua_GetCurrentDirectoryPageNumber(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUpdateListManagerThread(luaClassGetObject(L)).CurrentDirectoryPageNumber);
  Result := 1;
end;

function lua_SetCurrentDirectoryPageNumber(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUpdateListManagerThread(luaClassGetObject(L)).CurrentDirectoryPageNumber := lua_tointeger(L, 1);
end;

function lua_updateStatusText(L: Plua_State): Integer; cdecl;
begin
  Result:=0;
  TUpdateListManagerThread(luaClassGetObject(L)).UpdateStatusFormatted(luaToString(L, 1));
end;

procedure luaUpdateListManagerAddMetaTable(const L: Plua_State;
  const Obj: Pointer; const MetaTable, UserData: Integer);
begin
  luaClassAddProperty(L, MetaTable, UserData, 'CurrentDirectoryPageNumber', @lua_GetCurrentDirectoryPageNumber, @lua_SetCurrentDirectoryPageNumber);
  luaClassAddFunction(L, MetaTable, UserData, 'UpdateStatusText', @lua_updateStatusText);
end;

initialization
  luaClassRegister(TUpdateListManagerThread, @luaUpdateListManagerAddMetaTable);

end.

