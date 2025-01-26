unit FavoritesDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLiteData, uBaseUnit;

type

  { TFavoritesDB }

  TFavoritesDB = class(TSQLiteDataWA)
  public
    constructor Create(const AFilename: String);
    procedure Add(const Aid:String;const AOrder:Integer;const AEnabled:Boolean;
      const AModuleID,ALink,ATitle,AStatus,ACurrentChapter,ADownloadedChapterList,ASaveTo:String;
      const ADateAdded:TDateTime);
    procedure Replace(const OldId, Aid:String;const AOrder:Integer;const AEnabled:Boolean;
      const AModuleID,ALink,ATitle,AStatus,ACurrentChapter,ADownloadedChapterList,ASaveTo:String;
      const ADateAdded:TDateTime);
    procedure UpdateLastUpdated(const Aid,ADownloadedChapterList:String;const ADateLastUpdated:TDateTime); inline;
    procedure UpdateOrder(const Aid:String;const Aorder:Integer); inline;
    procedure UpdateTitle(const Aid,Atitle:String); inline;
    procedure UpdateEnabled(const Aid:String;const AEnabled:Boolean); inline;
    procedure UpdateLastChecked(const Aid,Astatus,AcurrentChapter:String;const ADateLastChecked:TDateTime);
    procedure UpdateSaveTo(const Aid,ASaveTo:String);
    procedure Delete(const Aid:String); inline;
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

procedure TFavoritesDB.Add(const Aid: String; const AOrder: Integer; const AEnabled: Boolean;
  const AModuleID, ALink, ATitle, AStatus, ACurrentChapter, ADownloadedChapterList,
  ASaveTo: String; const ADateAdded: TDateTime);
var
  SQL: String;
begin
  SQL := 'INSERT OR REPLACE INTO "favorites" ('+FieldsParams+') VALUES ('+
    PrepSQLValue(Aid) + ',' +
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

  AppendSQL(SQL);
end;

procedure TFavoritesDB.Replace(const OldId, Aid: String; const AOrder: Integer; const AEnabled: Boolean;
  const AModuleID, ALink, ATitle, AStatus, ACurrentChapter, ADownloadedChapterList,
  ASaveTo: String; const ADateAdded: TDateTime);
var
  SQL: String;
begin
  if OldId <> Aid then
  begin
    Delete(OldId);
  end;

  Add(
    Aid,
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

procedure TFavoritesDB.UpdateLastUpdated(const Aid,ADownloadedChapterList:String;const ADateLastUpdated:TDateTime);
begin
  AppendSQL('UPDATE "favorites" SET "downloadedchapterlist"='+PrepSQLValue(ADownloadedChapterList)+
    ',"datelastupdated"='+PrepSQLValue(ADateLastUpdated)+
    ' WHERE "id"='+PrepSQLValue(Aid)+';');
end;

procedure TFavoritesDB.UpdateOrder(const Aid:String;const Aorder:Integer);
begin
  AppendSQL('UPDATE "favorites" SET "Order"='+PrepSQLValue(Aorder)+' WHERE "id"='+PrepSQLValue(Aid)+';');
end;

procedure TFavoritesDB.UpdateTitle(const Aid, Atitle: String);
begin
  AppendSQLSafe('UPDATE "favorites" SET "title"='+PrepSQLValue(Atitle)+' WHERE "id"='+PrepSQLValue(Aid)+';');
end;

procedure TFavoritesDB.UpdateEnabled(const Aid: String; const AEnabled: Boolean);
begin
  AppendSQL('UPDATE "favorites" SET "enabled"='+PrepSQLValue(AEnabled)+' WHERE "id"='+PrepSQLValue(Aid)+';');
end;

procedure TFavoritesDB.UpdateLastChecked(const Aid,Astatus,AcurrentChapter:String;const ADateLastChecked:TDateTime);
begin
  AppendSQL('UPDATE "favorites" SET "status"='+PrepSQLValue(Astatus)+
   ',"currentchapter"='+PrepSQLValue(AcurrentChapter)+
   ',"datelastchecked"='+PrepSQLValue(ADateLastChecked)+
   ' WHERE "id"='+PrepSQLValue(Aid)+';');
end;

procedure TFavoritesDB.UpdateSaveTo(const Aid, ASaveTo: String);
begin
  AppendSQL('UPDATE "favorites" SET "saveto"='+PrepSQLValue(ASaveTo)+' WHERE "id"='+PrepSQLValue(Aid)+';');
end;

procedure TFavoritesDB.Delete(const Aid: String);
begin
  AppendSQL('DELETE FROM "favorites" WHERE "id"='+PrepSQLValue(aid)+';');
end;

end.

