unit LuaMangaCheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaMangaCheckAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses
  uBaseUnit, LuaClass, LuaStrings;

procedure luaMangaCheckAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  with TMangaCheck(Obj) do
  begin
    luaClassAddStringProperty(L, MetaTable, 'MangaURL', @MangaURL);
    luaClassAddStringProperty(L, MetaTable, 'MangaTitle', @MangaTitle);
    luaClassAddStringProperty(L, MetaTable, 'ChapterURL', @ChapterURL);
    luaClassAddStringProperty(L, MetaTable, 'ChapterTitle', @ChapterTitle);
    luaClassAddBooleanProperty(L, MetaTable, 'AddRootHost', @AddRootHost);
  end;
end;

initialization
  luaClassRegister(TMangaCheck, @luaMangaCheckAddMetaTable);

end.

