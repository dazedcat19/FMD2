unit LuaImagePuzzle;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaPackage,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaImagePuzzleAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses
  LuaClass, LuaUtils, ImagePuzzle;

type
  TUserData = TImagePuzzle;


function imagepuzzle_create(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop(L) = 2 then
    luaClassPushObject(L, TImagePuzzle.Create(lua_tointeger(L, 1), lua_tointeger(L, 2)), '', True, @luaImagePuzzleAddMetaTable);
  Result := 1;
end;

function imagepuzzle_descramble(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).DeScramble(
    TStream(luaToUserData(L, 1)),
    TStream(luaToUserData(L, 2)));
end;

function imagepuzzle_matrixget(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).Matrix[lua_tointeger(L, 1)]);
  Result := 1;
end;

function imagepuzzle_matrixset(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Matrix[lua_tointeger(L, 1)] := lua_tointeger(L, 2);
end;

function imagepuzzle_gethorblock(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).HorBlock);
  Result := 1;
end;

function imagepuzzle_getverblock(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).VerBlock);
  Result := 1;
end;

const
  constructs: packed array [0..2] of luaL_Reg = (
    (name: 'New'; func: @imagepuzzle_create),
    (name: 'Create'; func: @imagepuzzle_create),
    (name: nil; func: nil)
    );
  props: packed array[0..2] of lual_Reg_prop = (
    (name: 'HorBlock'; funcget: @imagepuzzle_gethorblock; funcset: nil),
    (name: 'VerBlock'; funcget: @imagepuzzle_getverblock; funcset: nil),
    (name: nil; funcget: nil; funcset: nil)
    );
  arrprops: packed array[0..1] of luaL_Reg_prop = (
    (name: 'Matrix'; funcget: @imagepuzzle_matrixget; funcset: @imagepuzzle_matrixset),
    (name: nil; funcget: nil; funcset: nil)
    );
  methods: packed array [0..1] of luaL_Reg = (
    (name: 'DeScramble'; func: @imagepuzzle_descramble),
    (name: nil; func: nil)
    );

procedure luaImagePuzzleAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
  luaClassAddProperty(L, MetaTable, UserData, props);
  luaClassAddArrayProperty(L, MetaTable, UserData, arrprops);
  luaClassAddIntegerProperty(L, MetaTable, 'Multiply', @TUserData(Obj).Multiply);
end;

function luaopen_imagepuzzle(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, constructs);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('imagepuzzle', @luaopen_imagepuzzle);

end.

