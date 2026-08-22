unit DownloadsDB;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SQLiteData, sqlite3dyn;

type

  { TDownloadsDB }

  TDownloadsDB = class(TSQliteData)
  public
    constructor Create(const AFilename: String);
    function Add(
      const Aenabled: Boolean;
      const Aorder, Ataskstatus, Achapterptr, Anumberofpages, Acurrentpage: Integer;
      const Amoduleid, Alink, Atitle, Astatus, Aprogress, Asaveto: String;
      const Adateadded, Adatelastdownloaded: TDateTime;
      const Achapterslinks, Achaptersnames, Apagelinks, Apagecontainerlinks, Afilenames, Acustomfilenames, Achaptersstatus: String
      ): String; inline;
    procedure Update(
      const Aid: String;
      const Ataskstatus, Achapterptr, Anumberofpages, Acurrentpage: Integer;
      const Astatus, Aprogress: String;
      const Adatelastdownloaded: TDateTime;
      const Apagelinks, Apagecontainerlinks, Afilenames, Achaptersstatus: String
      ); inline;
    procedure UpdateEnabled(const Aid: String; const Aenabled: Boolean); inline;
    procedure UpdateStatus(const Aid: String; const Ataskstatus: Integer; const Astatus: String); inline;
    procedure Delete(const Aid:String); inline;
  end;

const
  f_id                 = 0;
  f_enabled            = 1;
  f_order              = 2;
  f_taskstatus         = 3;
  f_chapterptr         = 4;
  f_numberofpages      = 5;
  f_currentpage        = 6;
  f_moduleid           = 7;
  f_link               = 8;
  f_title              = 9;
  f_status             = 10;
  f_progress           = 11;
  f_saveto             = 12;
  f_dateadded          = 13;
  f_datelastdownloaded = 14;
  f_chapterslinks      = 15;
  f_chaptersnames      = 16;
  f_pagelinks          = 17;
  f_pagecontainerlinks = 18;
  f_filenames          = 19;
  f_customfilenames    = 20;
  f_chaptersstatus     = 21;

implementation

uses
  uBaseUnit;

{ TDownloadsDB }

constructor TDownloadsDB.Create(const AFilename: String);
begin
  inherited Create;
  Filename := AFilename;
  TableName := 'downloads';
  CreateParams :=
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
    '"chaptersstatus" TEXT';
  FieldsParams := '"id","enabled","order","taskstatus","chapterptr","numberofpages","currentpage","moduleid","link","title","status","progress","saveto","dateadded","datelastdownloaded","chapterslinks","chaptersnames","pagelinks","pagecontainerlinks","filenames","customfilenames","chaptersstatus"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM ' + QuotedStrD(TableName) + ' ORDER BY "order"';
end;

function TDownloadsDB.Add(
  const Aenabled: Boolean; const Aorder, Ataskstatus, Achapterptr, Anumberofpages, Acurrentpage: Integer;
  const Amoduleid, Alink, Atitle, Astatus, Aprogress, Asaveto: String;
  const Adateadded, Adatelastdownloaded: TDateTime;
  const Achapterslinks, Achaptersnames, Apagelinks, Apagecontainerlinks, Afilenames, Acustomfilenames, Achaptersstatus: String
  ): String;
var
  SQL: String;
begin
  SQL := 'INSERT INTO "downloads" ("enabled","order","taskstatus","chapterptr","numberofpages","currentpage","moduleid","link","title","status","progress","saveto","dateadded","datelastdownloaded","chapterslinks","chaptersnames","pagelinks","pagecontainerlinks","filenames","customfilenames","chaptersstatus") VALUES (' +
    PrepSQLValue(Aenabled) + ',' +
    PrepSQLValue(Aorder) + ',' +
    PrepSQLValue(Ataskstatus) + ',' +
    PrepSQLValue(Achapterptr) + ',' +
    PrepSQLValue(Anumberofpages) + ',' +
    PrepSQLValue(Acurrentpage) + ',' +
    PrepSQLValue(Amoduleid) + ',' +
    PrepSQLValue(Alink) + ',' +
    PrepSQLValue(Atitle) + ',' +
    PrepSQLValue(Astatus) + ',' +
    PrepSQLValue(Aprogress) + ',' +
    PrepSQLValue(Asaveto) + ',' +
    PrepSQLValue(Adateadded) + ',' +
    PrepSQLValue(Adatelastdownloaded) + ',' +
    PrepSQLValue(Achapterslinks) + ',' +
    PrepSQLValue(Achaptersnames) + ',' +
    PrepSQLValue(Apagelinks) + ',' +
    PrepSQLValue(Apagecontainerlinks) + ',' +
    PrepSQLValue(Afilenames) + ',' +
    PrepSQLValue(Acustomfilenames) + ',' +
    PrepSQLValue(Achaptersstatus) + ');';

  AddSQL(SQL);
  Result := IntToStr(sqlite3_last_insert_rowid(Connection.Handle));
end;

procedure TDownloadsDB.Update(
  const Aid: String;
  const Ataskstatus, Achapterptr, Anumberofpages, Acurrentpage: Integer;
  const Astatus, Aprogress: String;
  const Adatelastdownloaded: TDateTime;
  const Apagelinks, Apagecontainerlinks, Afilenames, Achaptersstatus: String
  );
begin
  AddSQL('UPDATE "downloads" SET "taskstatus"=' +
    PrepSQLValue(Ataskstatus) +
    ',"chapterptr"=' + PrepSQLValue(Achapterptr) +
    ',"numberofpages"=' + PrepSQLValue(Anumberofpages) +
    ',"currentpage"=' + PrepSQLValue(Acurrentpage) +
    ',"status"=' + PrepSQLValue(Astatus) +
    ',"progress"=' + PrepSQLValue(Aprogress) +
    ',"datelastdownloaded"=' + PrepSQLValue(Adatelastdownloaded) +
    ',"pagelinks"=' + PrepSQLValue(Apagelinks) +
    ',"pagecontainerlinks"=' + PrepSQLValue(Apagecontainerlinks) +
    ',"filenames"=' + PrepSQLValue(Afilenames) +
    ',"chaptersstatus"=' + PrepSQLValue(Achaptersstatus) +
    ' WHERE "id"=''' + Aid + ''';');
end;

procedure TDownloadsDB.UpdateEnabled(const Aid:String;const Aenabled:Boolean);
begin
  AddSQL('UPDATE "downloads" SET "enabled"=' + PrepSQLValue(Aenabled) + ' WHERE "id"=''' + Aid + ''';');
end;

procedure TDownloadsDB.UpdateStatus(const Aid:String;const Ataskstatus:Integer;const Astatus:String);
begin
  AddSQL('UPDATE "downloads" SET "taskstatus"=' + PrepSQLValue(Ataskstatus) + ',"status"=' + PrepSQLValue(Astatus) + ' WHERE "id"=''' + Aid + ''';');
end;

procedure TDownloadsDB.Delete(const Aid:String);
begin
  AddSQL('DELETE FROM "downloads" WHERE "id"=''' + Aid + ''';');
end;

end.

