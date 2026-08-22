unit LuaStrings;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaPackage,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaStringsAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses
  LuaClass, LuaUtils;

type
  TUserData = TStrings;

function strings_create(L: Plua_State): Integer; cdecl;
begin
  luaClassPushObject(L, TStringList.Create, '', True, @luaStringsAddMetaTable);
  Result := 1;
end;

function strings_loadfromfile(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).LoadFromFile(luaToString(L, 1));
end;

function strings_loadfromstream(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).LoadFromStream(TStream(luaToUserData(L, 1)));
end;

function strings_savetofile(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).SaveToFile(luaToString(L, 1));
end;

function strings_savetostream(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).SaveToStream(TStream(luaToUserData(L, 1)));
end;

function strings_settext(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Text := luaToString(L, 1);
end;

function strings_gettext(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).Text);
  Result := 1;
end;

function strings_setcommatext(L: Plua_State): Integer; cdecl;
begin
  TUserData(luaClassGetObject(L)).CommaText := luaToString(L, 1);
  Result := 0;
end;

function strings_getcommatext(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).CommaText);
  Result := 1;
end;

function strings_add(L: Plua_State): Integer; cdecl;
begin
  TUserData(luaClassGetObject(L)).Add(luaToString(L, 1));
  Result := 0;
end;

function strings_addtext(L: Plua_State): Integer; cdecl;
begin
  TUserData(luaClassGetObject(L)).AddText(luaToString(L, 1));
  Result := 0;
end;

function strings_get(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).Strings[lua_tointeger(L, 1)]);
  Result := 1;
end;

function strings_set(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Strings[lua_tointeger(L, 1)] := luaToString(L, 2);
end;

function strings_getdelimitedtext(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).DelimitedText);
  Result := 1;
end;

function strings_setdelimitedtext(L: Plua_State): Integer; cdecl;
begin
  TUserData(luaClassGetObject(L)).DelimitedText := luaToString(L, 1);
  Result := 0;
end;

function strings_getdelimiter(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).Delimiter);
  Result := 1;
end;

function strings_setdelimiter(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Delimiter := String(luaToString(L, 1))[1];
end;

function strings_namevalueseparatorget(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).NameValueSeparator);
  Result := 1;
end;

function strings_namevalueseparatorset(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).NameValueSeparator := String(luaToString(L, 1))[1];
end;

function strings_valuesget(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).Values[luaToString(L, 1)]);
  Result := 1;
end;

function strings_valuesset(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Values[luaToString(L, 1)] := luaToString(L, 2);
end;

function strings_getcount(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).Count);
  Result := 1;
end;

function strings_sort(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TStringList(TUserData(luaClassGetObject(L))).Sort;
end;

function strings_clear(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Clear;
end;

function strings_delete(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Delete(lua_tointeger(L, 1));
end;

function strings_indexof(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).IndexOf(luaToString(L, 1)));
  Result := 1;
end;

function strings_indexofname(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, TUserData(luaClassGetObject(L)).IndexOfName(luaToString(L, 1)));
  Result := 1;
end;

function strings_reverse(L: Plua_State): Integer; cdecl;
var
  u: TUserData;
  i: Integer;

  Procedure _exchange(Index1, Index2: Integer);
  Var
    Obj : TObject;  Str : String;
  begin
    with u do
    begin
      Obj:=Objects[Index1];
      Str:=Strings[Index1];
      Objects[Index1]:=Objects[Index2];
      Strings[Index1]:=Strings[Index2];
      Objects[Index2]:=Obj;
      Strings[Index2]:=Str;
    end;
  end;

begin
  Result := 0;
  u := TUserData(luaClassGetObject(L));
  if u.Count > 1 then
    try
      u.BeginUpdate;
      for i := 0 to ((u.Count - 1) div 2) do
        _exchange(i, u.Count - 1 - i);
    finally
      u.EndUpdate;
    end;
end;

const
  constructs: packed array [0..2] of luaL_Reg = (
    (name: 'New'; func: @strings_create),
    (name: 'Create'; func: @strings_create),
    (name: nil; func: nil)
    );

  methods: packed array [0..17] of luaL_Reg = (
    (name: 'LoadFromFile'; func: @strings_loadfromfile),
    (name: 'LoadFromStream'; func: @strings_loadfromstream),
    (name: 'SaveToFile'; func: @strings_savetofile),
    (name: 'SaveToStream'; func: @strings_savetostream),
    (name: 'SetText'; func: @strings_settext),
    (name: 'GetText'; func: @strings_gettext),
    (name: 'Add'; func: @strings_add),
    (name: 'AddText'; func: @strings_addtext),
    (name: 'Get'; func: @strings_get),
    (name: 'Set'; func: @strings_set),
    (name: 'GetCount'; func: @strings_getcount),
    (name: 'Sort'; func: @strings_sort),
    (name: 'Clear'; func: @strings_clear),
    (name: 'Delete'; func: @strings_delete),
    (name: 'IndexOf'; func: @strings_indexof),
    (name: 'IndexOfName'; func: @strings_indexofname),
    (name: 'Reverse'; func: @strings_reverse),
    (name: nil; func: nil)
    );
  props: packed array[0..6] of lual_Reg_prop = (
    (name: 'Count'; funcget: @strings_getcount; funcset: nil),
    (name: 'Text'; funcget: @strings_gettext; funcset: @strings_settext),
    (name: 'CommaText'; funcget: @strings_getcommatext; funcset: @strings_setcommatext),
    (name: 'DelimitedText'; funcget: @strings_getdelimitedtext; funcset: @strings_setdelimitedtext),
    (name: 'Delimiter'; funcget: @strings_getdelimiter; funcset: @strings_setdelimiter),
    (name: 'NameValueSeparator'; funcget: @strings_namevalueseparatorget; funcset: @strings_namevalueseparatorset),
    (name: nil; funcget: nil; funcset: nil)
    );
  arrprops: packed array[0..2] of lual_Reg_prop = (
    (name: 'Strings'; funcget: @strings_get; funcset: @strings_set),
    (name: 'Values'; funcget: @strings_valuesget; funcset: @strings_valuesset),
    (name: nil; funcget: nil; funcset: nil)
    );

procedure luaStringsAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
  luaClassAddProperty(L, MetaTable, UserData, props);
  luaClassAddArrayProperty(L, MetaTable, UserData, arrprops);
  luaClassAddDefaultArrayProperty(L, MetaTable, UserData, @strings_get, @strings_set);
end;

function luaopen_strings(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, constructs);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('strings', @luaopen_strings);

end.
