unit LuaImagePatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaImagePatchAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses LuaClass, LuaUtils, LuaPackage, ImagePatch;

type
  TUserData = TImagePatch;

function imagepatch_create(L: Plua_State): Integer; cdecl;
begin
  luaClassPushObject(L, TImagePatch.Create, '', True, @luaImagePatchAddMetaTable);
  Result := 1;
end;

function imagepatch_add(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Add(
    lua_tonumber(L, 1), lua_tonumber(L, 2), lua_tonumber(L, 3), lua_tonumber(L, 4),
    luaToString(L, 5));
end;

function imagepatch_clear(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Clear;
end;

function imagepatch_apply(L: Plua_State): Integer; cdecl;
begin
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).Apply(
    TStream(luaToUserData(L, 1)),
    TStream(luaToUserData(L, 2))));
  Result := 1;
end;

function imagepatch_getcount(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).Count);
  Result := 1;
end;

const
  constructs: packed array [0..2] of luaL_Reg = (
    (name: 'New'; func: @imagepatch_create),
    (name: 'Create'; func: @imagepatch_create),
    (name: nil; func: nil)
    );
  props: packed array[0..1] of luaL_Reg_prop = (
    (name: 'Count'; funcget: @imagepatch_getcount; funcset: nil),
    (name: nil; funcget: nil; funcset: nil)
    );
  methods: packed array [0..3] of luaL_Reg = (
    (name: 'Add'; func: @imagepatch_add),
    (name: 'Clear'; func: @imagepatch_clear),
    (name: 'Apply'; func: @imagepatch_apply),
    (name: nil; func: nil)
    );

procedure luaImagePatchAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
  luaClassAddProperty(L, MetaTable, UserData, props);
  luaClassAddIntegerProperty(L, MetaTable, 'JpegQuality', @TUserData(Obj).JpegQuality);
end;

function luaopen_imagepatch(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, constructs);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('imagepatch', @luaopen_imagepatch);

end.