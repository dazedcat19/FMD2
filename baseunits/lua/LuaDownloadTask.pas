unit LuaDownloadTask;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaClass, uDownloadsManager,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure luaDownloadTaskMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses
  LuaStrings;

procedure luaDownloadTaskMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  with TTaskContainer(Obj) do
  begin
    luaClassAddObject(L, MetaTable, PageLinks, 'PageLinks', @luaStringsAddMetaTable);
    luaClassAddObject(L, MetaTable, ChapterLinks, 'ChapterLinks', @luaStringsAddMetaTable);
    luaClassAddObject(L, MetaTable, ChapterNames, 'ChapterNames', @luaStringsAddMetaTable);
    luaClassAddObject(L, MetaTable, PageContainerLinks, 'PageContainerLinks', @luaStringsAddMetaTable);
    luaClassAddObject(L, MetaTable, FileNames, 'FileNames', @luaStringsAddMetaTable);
    luaClassAddIntegerProperty(L, MetaTable, 'CurrentDownloadChapterPtr', @CurrentDownloadChapterPtr);
    luaClassAddIntegerProperty(L, MetaTable, 'PageNumber', @PageNumber);
    luaClassAddIntegerProperty(L, MetaTable, 'CurrentMaxFileNameLength', @TaskThread.CurrentMaxFileNameLength);
    luaClassAddStringProperty(L, MetaTable, 'Link', @DownloadInfo.Link);
  end;
end;

initialization
  luaClassRegister(TTaskContainer, @luaDownloadTaskMetaTable);

end.

