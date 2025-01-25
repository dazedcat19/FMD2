unit LuaMangaInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaMangaInfoAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses
  uBaseUnit, LuaClass, LuaStrings;

procedure luaMangaInfoAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  with TMangaInfo(Obj) do
  begin
    luaClassAddStringProperty(L, MetaTable, 'URL', @URL);
    luaClassAddStringProperty(L, MetaTable, 'Title', @Title);
    luaClassAddStringProperty(L, MetaTable, 'AltTitles', @AltTitles);
    luaClassAddStringProperty(L, MetaTable, 'Link', @Link);
    luaClassAddStringProperty(L, MetaTable, 'CoverLink', @CoverLink);
    luaClassAddStringProperty(L, MetaTable, 'Authors', @Authors);
    luaClassAddStringProperty(L, MetaTable, 'Artists', @Artists);
    luaClassAddStringProperty(L, MetaTable, 'Genres', @Genres);
    luaClassAddStringProperty(L, MetaTable, 'Status', @Status);
    luaClassAddStringProperty(L, MetaTable, 'Summary', @Summary);
    luaClassAddObject(L, MetaTable, ChapterNames, 'ChapterNames', @luaStringsAddMetaTable);
    luaClassAddObject(L, MetaTable, ChapterLinks, 'ChapterLinks', @luaStringsAddMetaTable);
  end;
end;

initialization
  luaClassRegister(TMangaInfo, @luaMangaInfoAddMetaTable);

end.

