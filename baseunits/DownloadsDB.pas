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
      const AEnabled: Boolean;
      const AOrder, ATaskStatus, AChapterPtr, ANumberOfPages, ACurrentPage: Integer;
      const AModuleID, ALink, ATitle, AStatus, AProgress, ASaveTo: String;
      const ADateAdded, ADateLastDownloaded: TDateTime;
      const AChaptersLinks, AChaptersNames, APageLinks, APageContainerLinks, AFileNames, ACustomFileNames, AChaptersStatus: String
      ): String; inline;
    procedure Update(
      const Aid: String;
      const ATaskStatus, AChapterPtr, ANumberOfPages, ACurrentPage: Integer;
      const AStatus, AProgress: String;
      const ADateLastDownloaded: TDateTime;
      const APageLinks, APageContainerLinks, AFileNames, AChaptersStatus: String
      ); inline;
    procedure UpdateEnabled(const AID: String; const AEnabled: Boolean); inline;
    procedure UpdateStatus(const AID: String; const ATaskStatus: Integer; const AStatus: String); inline;
    procedure Delete(const AID: String); inline;
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
  const AEnabled: Boolean;
  const AOrder, ATaskStatus, AChapterPtr, ANumberOfPages, ACurrentPage: Integer;
  const AModuleID, ALink, ATitle, AStatus, AProgress, ASaveTo: String;
  const ADateAdded, ADateLastDownloaded: TDateTime;
  const AChaptersLinks, AChaptersNames, APageLinks, APageContainerLinks, AFileNames, ACustomFileNames, AChaptersStatus: String
  ): String;
var
  SQL: String;
begin
  SQL := 'INSERT INTO "downloads" ("enabled","order","taskstatus","chapterptr","numberofpages","currentpage","moduleid","link","title","status","progress","saveto","dateadded","datelastdownloaded","chapterslinks","chaptersnames","pagelinks","pagecontainerlinks","filenames","customfilenames","chaptersstatus") VALUES (' +
    PrepSQLValue(AEnabled) + ',' +
    PrepSQLValue(AOrder) + ',' +
    PrepSQLValue(ATaskStatus) + ',' +
    PrepSQLValue(AChapterPtr) + ',' +
    PrepSQLValue(ANumberOfPages) + ',' +
    PrepSQLValue(ACurrentPage) + ',' +
    PrepSQLValue(AModuleID) + ',' +
    PrepSQLValue(ALink) + ',' +
    PrepSQLValue(ATitle) + ',' +
    PrepSQLValue(AStatus) + ',' +
    PrepSQLValue(AProgress) + ',' +
    PrepSQLValue(ASaveTo) + ',' +
    PrepSQLValue(ADateAdded) + ',' +
    PrepSQLValue(ADateLastDownloaded) + ',' +
    PrepSQLValue(AChaptersLinks) + ',' +
    PrepSQLValue(AChaptersNames) + ',' +
    PrepSQLValue(APageLinks) + ',' +
    PrepSQLValue(APageContainerLinks) + ',' +
    PrepSQLValue(AFileNames) + ',' +
    PrepSQLValue(ACustomFileNames) + ',' +
    PrepSQLValue(AChaptersStatus) + ');';

  AddSQL(SQL);
  Result := IntToStr(sqlite3_last_insert_rowid(Connection.Handle));
end;

procedure TDownloadsDB.Update(
  const AID: String;
  const ATaskStatus, AChapterPtr, ANumberOfPages, ACurrentPage: Integer;
  const AStatus, AProgress: String;
  const ADateLastDownloaded: TDateTime;
  const APageLinks, APageContainerLinks, AFileNames, AChaptersStatus: String
  );
begin
  AddSQL('UPDATE ' + QuotedStrD(TableName) +
    ' SET "taskstatus"=' + PrepSQLValue(ATaskStatus) +
    ',"chapterptr"=' + PrepSQLValue(AChapterPtr) +
    ',"numberofpages"=' + PrepSQLValue(ANumberOfPages) +
    ',"currentpage"=' + PrepSQLValue(ACurrentPage) +
    ',"status"=' + PrepSQLValue(AStatus) +
    ',"progress"=' + PrepSQLValue(AProgress) +
    ',"datelastdownloaded"=' + PrepSQLValue(ADateLastDownloaded) +
    ',"pagelinks"=' + PrepSQLValue(APageLinks) +
    ',"pagecontainerlinks"=' + PrepSQLValue(APageContainerLinks) +
    ',"filenames"=' + PrepSQLValue(AFileNames) +
    ',"chaptersstatus"=' + PrepSQLValue(AChaptersStatus) +
    ' WHERE "id"=''' + AID + ''';');
end;

procedure TDownloadsDB.UpdateEnabled(const AID: String; const AEnabled: Boolean);
begin
  AddSQL('UPDATE ' + QuotedStrD(TableName) + ' SET "enabled"=' + PrepSQLValue(AEnabled) + ' WHERE "id"=''' + AID + ''';');
end;

procedure TDownloadsDB.UpdateStatus(const AID: String; const ATaskStatus: Integer; const AStatus: String);
begin
  AddSQL('UPDATE ' + QuotedStrD(TableName) + ' SET "taskstatus"=' + PrepSQLValue(ATaskStatus) + ',"status"=' + PrepSQLValue(AStatus) + ' WHERE "id"=''' + AID + ''';');
end;

procedure TDownloadsDB.Delete(const AID: String);
begin
  AddSQL('DELETE FROM ' + QuotedStrD(TableName) + ' WHERE "id"=''' + AID + ''';');
end;

end.

