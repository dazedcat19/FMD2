unit FavoritesDB;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SQLiteData;

type

  { TFavoritesDB }

  TFavoritesDB = class(TSQLiteData)
  public
    constructor Create(const AFilename: String);
    procedure Add(const AID: String; const AOrder: Integer; const AEnabled: Boolean;
      const AModuleID, ALink, ATitle, AStatus, ACurrentChapter, ADownloadedChapterList, ASaveTo: String;
      const ADateAdded: TDateTime); inline;
    procedure Replace(const OldID, AID: String; const AOrder: Integer; const AEnabled: Boolean;
      const AModuleID, ALink, ATitle, AStatus, ACurrentChapter, ADownloadedChapterList, ASaveTo: String;
      const ADateAdded: TDateTime); inline;
    procedure UpdateLastUpdated(const AID, ADownloadedChapterList: String; const ADateLastUpdated: TDateTime); inline;
    procedure UpdateOrder(const AID: String; const AOrder: Integer); inline;
    procedure UpdateTitle(const AID, ATitle:String); inline;
    procedure UpdateEnabled(const AID: String; const AEnabled: Boolean); inline;
    procedure UpdateLastChecked(const AID, AStatus, ACurrentChapter: String; const ADateLastChecked: TDateTime);
    procedure UpdateSaveTo(const AID, ASaveTo: String);
    procedure Delete(const AID: String); inline;
  end;

const
  f_id                     = 0;
  f_order                  = 1;
  f_enabled                = 2;
  f_moduleid               = 3;
  f_link                   = 4;
  f_title                  = 5;
  f_status                 = 6;
  f_currentchapter         = 7;
  f_downloadedchapterlist  = 8;
  f_saveto                 = 9;
  f_dateadded              = 10;
  f_datelastchecked        = 11;
  f_datelastupdated        = 12;

implementation

uses
  uBaseUnit;

{ TFavoritesDB }

constructor TFavoritesDB.Create(const AFilename: String);
begin
  inherited Create;

  Filename := AFilename;
  TableName := 'favorites';
  CreateParams :=
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
  FieldsParams := '"id","order","enabled","moduleid","link","title","status","currentchapter","downloadedchapterlist","saveto","dateadded","datelastchecked","datelastupdated"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM ' + QuotedStrD(TableName) + ' ORDER BY "order"';
end;

procedure TFavoritesDB.Add(const AID: String; const AOrder: Integer; const AEnabled: Boolean;
  const AModuleID, ALink, ATitle, AStatus, ACurrentChapter, ADownloadedChapterList,
  ASaveTo: String; const ADateAdded: TDateTime);
var
  SQL: String;
begin
  SQL := 'INSERT OR REPLACE INTO ' + QuotedStrD(TableName) + ' (' + FieldsParams + ') VALUES (' +
    PrepSQLValue(AID) + ',' +
    PrepSQLValue(AOrder) + ',' +
    PrepSQLValue(AEnabled) + ',' +
    PrepSQLValue(AModuleID) + ',' +
    PrepSQLValue(ALink) + ',' +
    PrepSQLValue(ATitle) + ',' +
    PrepSQLValue(AStatus) + ',' +
    PrepSQLValue(ACurrentChapter)  + ',' +
    PrepSQLValue(ADownloadedChapterList) + ',' +
    PrepSQLValue(ASaveTo) + ',' +
    PrepSQLValue(ADateAdded) + ',' +
    PrepSQLValue(ADateAdded) + ',' +
    PrepSQLValue(ADateAdded) + ');';

  AddSQL(SQL);
end;

procedure TFavoritesDB.Replace(const OldID, AID: String; const AOrder: Integer; const AEnabled: Boolean;
  const AModuleID, ALink, ATitle, AStatus, ACurrentChapter, ADownloadedChapterList,
  ASaveTo: String; const ADateAdded: TDateTime);
begin
  if OldID <> Aid then
  begin
    Delete(OldID);
  end;

  Add(
    AID,
    AOrder,
    AEnabled,
    AModuleID,
    ALink,
    ATitle,
    AStatus,
    ACurrentChapter,
    ADownloadedChapterList,
    ASaveTo,
    ADateAdded
    );
end;

procedure TFavoritesDB.UpdateLastUpdated(const AID, ADownloadedChapterList: String; const ADateLastUpdated: TDateTime);
begin
  AddSQL('UPDATE ' + QuotedStrD(TableName) +
    ' SET "downloadedchapterlist"=' + PrepSQLValue(ADownloadedChapterList) +
    ',"datelastupdated"=' + PrepSQLValue(ADateLastUpdated) +
    ' WHERE "id"=' + PrepSQLValue(AID) + ';');
end;

procedure TFavoritesDB.UpdateOrder(const AID: String; const AOrder: Integer);
begin
  AddSQL('UPDATE ' + QuotedStrD(TableName) + ' SET "Order"=' + PrepSQLValue(AOrder) + ' WHERE "id"=' + PrepSQLValue(AID) + ';');
end;

procedure TFavoritesDB.UpdateTitle(const AID, ATitle: String);
begin
  AddSQL('UPDATE ' + QuotedStrD(TableName) + ' SET "title"=' + PrepSQLValue(ATitle) + ' WHERE "id"=' + PrepSQLValue(AID) + ';');
end;

procedure TFavoritesDB.UpdateEnabled(const AID: String; const AEnabled: Boolean);
begin
  AddSQL('UPDATE ' + QuotedStrD(TableName) + ' SET "enabled"=' + PrepSQLValue(AEnabled) + ' WHERE "id"=' + PrepSQLValue(AID) + ';');
end;

procedure TFavoritesDB.UpdateLastChecked(const AID, AStatus, ACurrentChapter: String; const ADateLastChecked: TDateTime);
begin
  AddSQL('UPDATE ' + QuotedStrD(TableName) + ' SET "status"=' + PrepSQLValue(AStatus) +
   ',"currentchapter"=' + PrepSQLValue(ACurrentChapter) +
   ',"datelastchecked"=' + PrepSQLValue(ADateLastChecked) +
   ' WHERE "id"=' + PrepSQLValue(AID) + ';');
end;

procedure TFavoritesDB.UpdateSaveTo(const AID, ASaveTo: String);
begin
  AddSQL('UPDATE ' + QuotedStrD(TableName) + ' SET "saveto"=' + PrepSQLValue(ASaveTo) + ' WHERE "id"=' + PrepSQLValue(AID) + ';');
end;

procedure TFavoritesDB.Delete(const AID: String);
begin
  AddSQL('DELETE FROM ' + QuotedStrD(TableName) + ' WHERE "id"=' + PrepSQLValue(AID) + ';');
end;

end.

