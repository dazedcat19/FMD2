program converter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, jsonini, SQLite3Dyn, SQLite3Conn, SQLDB;

var
  modules: TStringList;
  i: Integer;
  m: TStringArray;
  s, si: String;
  dbcon: TSQLite3Connection;
  dbquery: TSQLQuery;
  dbtrans: TSQLTransaction;
  datenow, FTableName, FCreateParams, FFieldsParams, FSelectParams: String;

function copyfile(fromfile, tofile:string):boolean;
begin
  result:=false;
  if not fileexists(fromfile) then exit;
  result:=true;
  if fileexists(tofile) then result:=DeleteFile(tofile);
  if result then
    with tmemorystream.create do
    try
      loadfromfile(fromfile);
      savetofile(tofile);
      result:=fileexists(tofile);
    finally
      free;
    end;
end;

function ToStrZeroPad(const i, len: Word): String;
begin
  Result:=IntToStr(i);
  if Length(Result)<len then
    Result:=StringOfChar('0',len-Length(Result))+Result;
end;

function DateTimeToSQLiteDateTime(const D: TDateTime): String;
var
  Year, Month, Day, Hour, Minute, Second, MiliSecond: word;
begin
  DecodeDate(D, Year, Month, Day);
  DecodeTime(D, Hour, Minute, Second, MiliSecond);
  Result := ToStrZeroPad(Year,4)+'-'+ToStrZeroPad(Month,2)+'-'+ToStrZeroPad(Day,2)+' '+
            ToStrZeroPad(Hour,2)+':'+ToStrZeroPad(Minute,2)+':'+ToStrZeroPad(Second,2)+'.'+ToStrZeroPad(MiliSecond,3);
end;

function QuotedStr(const S: TDateTime): String;
begin
  Result := AnsiQuotedStr(DateTimeToSQLiteDateTime(S), '''');
end;

function QuotedStrD(const S: String): String;
begin
  Result := AnsiQuotedStr(S, '"');
end;

begin
  ForceDirectories('converted\userdata');
  if FileExists('config\config.ini') then
    TJSONIniFile.ConvertIni('config\config.ini', 'converted\userdata\settings.json', True);

  modules:=TStringList.Create;
  if FileExists('modules.txt') then
  begin
    modules.Sorted := false;
    modules.LoadFromFile('modules.txt');
    for i:=0 to modules.Count-1 do
      begin
      m:=modules[i].Split(' ');
      modules[i] := m[1]+'='+m[0];
      end;
    modules.Sorted := true;
    modules.CaseSensitive := false;
  end
  else
    begin
    writeln('modules.txt not found! Exiting... ');
    exit;
  end;


  if FileExists('converted\userdata\settings.json') then
    with TJSONIniFile.Create('converted\userdata\settings.json') do
    try
      s:='';
      for si in ReadString('general', 'MangaListSelect', '').Split(',') do
      begin
        i := modules.IndexOfName(si);
        if i <> -1 then
          s += modules.ValueFromIndex[i] + ',';
      end;
      WriteString('General', 'MangaListSelect', s.TrimRight(','));
      UpdateFile;
    finally
      free;
    end;

  datenow := QuotedStr(Now);

  s:=IncludeTrailingPathDelimiter(GetCurrentDir)+Sqlite3Lib;
  if FileExists(s) then
    SQLiteDefaultLibrary := s;
  dbcon:=TSQLite3Connection.Create(nil);
  dbtrans:=TSQLTransaction.Create(nil);
  dbquery:=TSQLQuery.Create(nil);
  try
    dbcon.Transaction:=dbtrans;
    dbquery.DataBase:=dbcon;
    dbquery.Transaction:=dbtrans;
    dbquery.Options := dbquery.Options-[sqoAutoApplyUpdates];

    if copyfile('works\downloadedchapters.db', 'converted\userdata\downloadedchapters.db') then
    begin
      dbcon.DatabaseName := 'converted\userdata\downloadedchapters.db';
      dbcon.Connected := true;
      dbcon.ExecuteDirect('ALTER TABLE "downloadedchapters" RENAME COLUMN "websitelink" TO "id"');
      dbquery.SQL.Text := 'SELECT "id" FROM "downloadedchapters"';
      dbquery.ExecSQL;
      dbquery.Active := true;
      dbquery.First;
      while dbquery.EOF=false do
      begin
        s:=dbquery.Fields[0].AsString;
        i:=Pos('/',s);
        si:=copy(s,i,Length(s));
        s:=copy(s,1,i-1);
        i:=modules.IndexOfName(s);
        if i<>-1 then
        begin
          dbquery.Edit;
          dbquery.Fields[0].AsString := modules.ValueFromIndex[i]+si;
          dbquery.Post;
        end;
        dbquery.Next;
      end;
      dbquery.ApplyUpdates;
      dbcon.ExecuteDirect('END TRANSACTION');
      dbcon.ExecuteDirect('VACUUM');
      dbcon.ExecuteDirect('BEGIN TRANSACTION');
      dbtrans.Commit;
      dbcon.Connected := false;
    end;

    if copyfile('works\favorites.db', 'converted\userdata\favorites.db') then
    begin
      dbcon.DatabaseName := 'converted\userdata\favorites.db';
      dbcon.Connected := true;
      dbcon.ExecuteDirect('ALTER TABLE "favorites" RENAME COLUMN "websitelink" TO "id"');
      dbcon.ExecuteDirect('ALTER TABLE "favorites" RENAME COLUMN "website" TO "moduleid"');

      FTableName := 'favorites';
      FCreateParams :=
        '"id" VARCHAR(3000) NOT NULL PRIMARY KEY,' +
        '"order" INTEGER,' +
        '"enabled" BOOLEAN,' +
        '"moduleid" TEXT,' +
        '"link" TEXT,' +
        '"title" TEXT,' +
        '"status" TEXT,' +
        '"currentchapter" TEXT,' +
        '"downloadedchapterlist" TEXT,' +
        '"saveto" TEXT,' +
        '"dateadded" DATETIME,' +
        '"datelastchecked" DATETIME,' +
        '"datelastupdated" DATETIME';
      FFieldsParams := '"id","order","enabled","moduleid","link","title","status","currentchapter","downloadedchapterlist","saveto","dateadded","datelastchecked","datelastupdated"';
      FSelectParams := 'SELECT ' + FFieldsParams + ' FROM ' + QuotedStrD(FTableName) + ' ORDER BY "order"';
      with dbcon do
      begin
        ExecuteDirect('DROP TABLE IF EXISTS ' + QuotedStrD('temp' + FTableName));
        ExecuteDirect('CREATE TABLE ' + QuotedStrD('temp' + FTableName) + ' (' + FCreateParams + ')');
        ExecuteDirect('INSERT INTO ' + QuotedStrD('temp' + FTableName) + ' (' + FFieldsParams + ') SELECT ' + FFieldsParams + ' FROM "' + FTableName + '"');
        ExecuteDirect('DROP TABLE ' + QuotedStrD(FTableName));
        ExecuteDirect('ALTER TABLE ' + QuotedStrD('temp' + FTableName) + ' RENAME TO ' + QuotedStrD(FTableName));
      end;

      dbcon.ExecuteDirect('UPDATE "favorites" SET "status"=''1''');
      dbcon.ExecuteDirect('UPDATE "favorites" SET "dateadded"='+datenow);
      dbcon.ExecuteDirect('UPDATE "favorites" SET "datelastchecked"='+datenow);
      dbcon.ExecuteDirect('UPDATE "favorites" SET "datelastupdated"='+datenow);
      dbquery.SQL.Text := 'SELECT "id", "moduleid" FROM "favorites"';
      dbquery.ExecSQL;
      dbquery.Active := true;
      dbquery.First;
      while dbquery.EOF=false do
      begin
        s:=dbquery.Fields[0].AsString;
        i:=Pos('/',s);
        si:=copy(s,i,Length(s));
        s:=copy(s,1,i-1);
        i:=modules.IndexOfName(s);
        if i<>-1 then
        begin
          dbquery.Edit;
          s:=modules.ValueFromIndex[i];
          dbquery.Fields[0].AsString := s+si;
          dbquery.Fields[1].AsString := s;
          dbquery.Post;
        end;
        dbquery.Next;
      end;
      dbquery.ApplyUpdates;
      dbcon.ExecuteDirect('END TRANSACTION');
      dbcon.ExecuteDirect('VACUUM');
      dbcon.ExecuteDirect('BEGIN TRANSACTION');
      dbtrans.Commit;
      dbcon.Connected := false;
    end;

    if copyfile('works\downloads.db', 'converted\userdata\downloads.db') then
    begin
      dbcon.DatabaseName := 'converted\userdata\downloads.db';
      dbcon.Connected := true;
      dbcon.ExecuteDirect('ALTER TABLE "downloads" RENAME TO "olddownloads"');
      dbcon.ExecuteDirect('CREATE TABLE "downloads" ('+
                          '"id" INTEGER PRIMARY KEY,' +
                          '"enabled" BOOLEAN,' +
                          '"order" INTEGER,' +
                          '"taskstatus" INTEGER,' +
                          '"chapterptr" INTEGER,' +
                          '"numberofpages" INTEGER,' +
                          '"currentpage" INTEGER,' +
                          '"moduleid" TEXT,' +
                          '"link" TEXT,' +
                          '"title" TEXT,' +
                          '"status" TEXT,' +
                          '"progress" TEXT,' +
                          '"saveto" TEXT,' +
                          '"dateadded" DATETIME,' +
                          '"datelastdownloaded" DATETIME,' +
                          '"chapterslinks" TEXT,' +
                          '"chaptersnames" TEXT,' +
                          '"pagelinks" TEXT,' +
                          '"pagecontainerlinks" TEXT,' +
                          '"filenames" TEXT,' +
                          '"customfilenames" TEXT,' +
                          '"chaptersstatus" TEXT)');

      dbcon.ExecuteDirect('INSERT INTO "downloads" (' +
                          '"enabled",' +
                          '"order",' +
                          '"taskstatus",' +
                          '"chapterptr",' +
                          '"numberofpages",' +
                          '"currentpage",' +
                          '"moduleid",' +
                          '"link",' +
                          '"title",' +
                          '"status",' +
                          '"progress",' +
                          '"saveto",' +
                          '"dateadded",' +
                          '"datelastdownloaded",' +
                          '"chapterslinks",' +
                          '"chaptersnames",' +
                          '"pagelinks",' +
                          '"pagecontainerlinks",' +
                          '"filenames",' +
                          '"customfilenames",' +
                          '"chaptersstatus")' +
                          ' SELECT ' +
                          '"enabled",' +
                          '"order",' +
                          '"taskstatus",' +
                          '"chapterptr",' +
                          '"numberofpages",' +
                          '"currentpage",' +
                          '"website",' +
                          '"link",' +
                          '"title",' +
                          '"status",' +
                          '"progress",' +
                          '"saveto",' +
                          '"datetime",' +
                          '"datetime",' +
                          '"chapterslinks",' +
                          '"chaptersnames",' +
                          '"pagelinks",' +
                          '"pagecontainerlinks",' +
                          '"filenames",' +
                          '"customfilenames",' +
                          '"chaptersstatus"' +
                          ' FROM "olddownloads"');
      dbcon.ExecuteDirect('DROP TABLE "olddownloads"');
      dbquery.SQL.Text := 'SELECT "id","moduleid" FROM "downloads"';
      dbquery.ExecSQL;
      dbquery.Active := true;
      dbquery.First;
      while dbquery.EOF=false do
      begin
        s:=dbquery.Fields[1].AsString;
        i:=modules.IndexOfName(s);
        if i<>-1 then
        begin
          dbquery.Edit;
          dbquery.Fields[1].AsString := modules.ValueFromIndex[i];
          dbquery.Post;
        end;
        dbquery.Next;
      end;
      dbquery.ApplyUpdates;
      dbcon.ExecuteDirect('END TRANSACTION');
      dbcon.ExecuteDirect('VACUUM');
      dbcon.ExecuteDirect('BEGIN TRANSACTION');
      dbtrans.Commit;
      dbcon.Connected := false;
    end;

  except
    on e:Exception do
      writeln('error: '+e.Message);
  end;
  dbquery.free;
  dbtrans.free;
  dbcon.free;

  ForceDirectories('converted\data');
  for i:=0 to modules.Count-1 do
    copyfile('data\'+modules.Names[i]+'.db', 'converted\data\'+modules.ValueFromIndex[i]+'.db');

  modules.Free;
end.

