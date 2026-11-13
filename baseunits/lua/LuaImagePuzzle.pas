unit LuaImagePuzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaImagePuzzleAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses LuaClass, LuaUtils, LuaPackage, Types, ImagePuzzle, ImgInfos;

type
  TUserData = TImagePuzzle;

function luaTableToIntArray(L: Plua_State; const idx: Integer;
  out AValues: TIntegerDynArray): Boolean;
var
  i, n: Integer;
begin
  Result := False;
  SetLength(AValues, 0);
  if not lua_istable(L, idx) then
    Exit;
  n := 0;
  i := 1;
  while True do
  begin
    lua_rawgeti(L, idx, i);
    if lua_isnil(L, -1) then
    begin
      lua_pop(L, 1);
      Break;
    end;
    Inc(n);
    SetLength(AValues, n);
    AValues[n - 1] := lua_tointeger(L, -1);
    lua_pop(L, 1);
    Inc(i);
  end;
  Result := n > 0;
end;

function imagepuzzle_create(L: Plua_State): Integer; cdecl;
var
  ColWidths, RowHeights: TIntegerDynArray;
begin
  Result := 0;
  if lua_gettop(L) <> 2 then
    Exit;
  if lua_istable(L, 1) and lua_istable(L, 2) then
  begin
    if not luaTableToIntArray(L, 1, ColWidths) or
       not luaTableToIntArray(L, 2, RowHeights) then
      Exit;
    luaClassPushObject(L, TImagePuzzle.Create(ColWidths, RowHeights), '', True, @luaImagePuzzleAddMetaTable);
  end
  else
  begin
    luaClassPushObject(L, TImagePuzzle.Create(lua_tointeger(L, 1), lua_tointeger(L, 2)), '', True, @luaImagePuzzleAddMetaTable);
  end;
  Result := 1;
end;

function imagepuzzle_descramble(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).DeScramble(
    TStream(luaToUserData(L, 1)),
    TStream(luaToUserData(L, 2)));
end;

function imagepuzzle_setunevengrid(L: Plua_State): Integer; cdecl;
var
  ColWidths, RowHeights: TIntegerDynArray;
  base: Integer;
begin
  Result := 0;
  base := 1;
  if not luaTableToIntArray(L, base, ColWidths) or
     not luaTableToIntArray(L, base + 1, RowHeights) then
    Exit;
  lua_pushboolean(L, TUserData(luaClassGetObject(L)).SetUnevenGrid(ColWidths, RowHeights));
  Result := 1;
end;

function imagepuzzle_imagesize(L: Plua_State): Integer; cdecl;
var
  Stream: TStream;
  OldPos: Int64;
  AWidth, AHeight: Integer;
begin
  Result := 0;
  if (lua_gettop(L) < 1) or not lua_isuserdata(L, 1) then
    Exit;
  Stream := TStream(luaToUserData(L, 1));
  if Stream = nil then
    Exit;
  AWidth := 0;
  AHeight := 0;
  OldPos := Stream.Position;
  try
    GetImageStreamSize(Stream, AWidth, AHeight);
  finally
    Stream.Position := OldPos;
  end;
  if (AWidth <= 0) or (AHeight <= 0) then
    Exit;
  lua_pushinteger(L, AWidth);
  lua_pushinteger(L, AHeight);
  Result := 2;
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

function imagepuzzle_flipsget(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).Flips[lua_tointeger(L, 1)]);
  Result := 1;
end;

function imagepuzzle_flipsset(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Flips[lua_tointeger(L, 1)] := lua_tointeger(L, 2);
end;

function imagepuzzle_rotationsget(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).Rotations[lua_tointeger(L, 1)]);
  Result := 1;
end;

function imagepuzzle_rotationsset(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Rotations[lua_tointeger(L, 1)] := lua_tointeger(L, 2);
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
  constructs: packed array [0..3] of luaL_Reg = (
    (name: 'New'; func: @imagepuzzle_create),
    (name: 'Create'; func: @imagepuzzle_create),
    (name: 'ImageSize'; func: @imagepuzzle_imagesize),
    (name: nil; func: nil)
    );
  props: packed array[0..2] of luaL_Reg_prop = (
    (name: 'HorBlock'; funcget: @imagepuzzle_gethorblock; funcset: nil),
    (name: 'VerBlock'; funcget: @imagepuzzle_getverblock; funcset: nil),
    (name: nil; funcget: nil; funcset: nil)
    );
  arrprops: packed array[0..3] of luaL_Reg_prop = (
    (name: 'Matrix'; funcget: @imagepuzzle_matrixget; funcset: @imagepuzzle_matrixset),
    (name: 'Flips'; funcget: @imagepuzzle_flipsget; funcset: @imagepuzzle_flipsset),
    (name: 'Rotations'; funcget: @imagepuzzle_rotationsget; funcset: @imagepuzzle_rotationsset),
    (name: nil; funcget: nil; funcset: nil)
    );
  methods: packed array [0..2] of luaL_Reg = (
    (name: 'DeScramble'; func: @imagepuzzle_descramble),
    (name: 'SetUnevenGrid'; func: @imagepuzzle_setunevengrid),
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
