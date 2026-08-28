{
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit DBDataProcess;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FileUtil, LazFileUtils, LazUTF8, Generics.Defaults, Generics.Collections,
  sqlite3conn, sqlite3backup, sqlite3dyn, sqldb, DB, RegExpr, SQLiteData, VirtualTrees;

type
  TFieldValuePair = record
    Field: String;
    Value: String;
  end;

  TDBFieldDef = record
    Name: String;
    TypeAndConstraints: String;
  end;

  { TMangaInfoData }

  PMangaInfoData = ^TMangaInfoData;

  TMangaInfoData = record
    Module: Pointer;
    Link,
    Title,
    AltTitles,
    TitleFormat,
    Authors,
    Artists,
    Genres,
    Status,
    Summary: String;
    NumChapter,
    JDN: Integer;
  end;

  PMangaInfoCache = ^TMangaInfoCache;

  TMangaInfoCache = record
    SearchKey: String;
    Data: TMangaInfoData;
  end;

  TMangaCacheList = specialize TList<TMangaInfoCache>;
  TIntegerList = specialize TList<Integer>;

  { TDBDataProcess }

  TDBDataProcess = class(TObject)
  private
    FGuardian: TRTLCriticalSection;
    FConn: TSQLite3ConnectionH;
    FTrans: TSQLTransaction;
    FReadQuery: TSQLQuery;
    FWriteQuery: TSQLQuery;
    FRegxp: TRegExpr;
    FModule: Pointer;
    FWebsite: String;
    FTableName: String;
    FRecordCount: Integer;
    FDBRecordCount: Integer;
    FSorted: Boolean;
    FFiltered: Boolean;
    FSearched: Boolean; 
    FSearchedTitle: String;
    FFilterAllSites: Boolean;
    FSitesList: TStringList;
    FAttachedSites: TStringList;
    FSQLSelect: String;
    FSQLOrderBy: String;
    FSQLSelectOrderBy: String;
    FRecNo: Integer;
    FDBCache: TMangaCacheList;
    FSearchedCacheIndices: TIntegerList;
    FConnectedWebsite,
    FCacheWebsite: String;
    procedure ResetRecNo(Dataset: TDataSet);

  protected
    procedure CreateTable;
    procedure CreateIndexes;
    procedure CreateFTSTable;
    procedure CreateField(const FieldName, FieldType: String);
    procedure RenameField(const FieldOldName, FieldNewName: String);
    procedure DeleteField(const FieldName: String);
    procedure CheckFieldsExist;
    procedure ConvertNewTable(const Fields: TStringList);
    procedure GetRecordCount;
    function GetConnected: Boolean;
    function GetFiltered: Boolean;
    function InternalOpen(const FilePath: String = ''): Boolean;
    function CheckAndRepairDatabase(const AModuleID: String): Boolean;
    function ExecuteDirect(SQL: String): Boolean;
    function CheckWebsiteAndFilePath(const AWebsite: String; var AFilePath: String): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock;

    function Connect(const AWebsite: String): Boolean;
    function ConnectFile(const AFile: String): Boolean; 
    procedure ConnectCacheDB(const AIndex: Integer);
    procedure DisconnectCacheDB;

    function Open(const AWebsite: String = ''): Boolean;
    function OpenTable(const ATableName: String = '';
      CheckRecordCount: Boolean = False): Boolean;
    function TableExist(const ATableName: String): Boolean;

    procedure SetDBCache(AClearCache: Boolean = True);
    procedure SetCacheTitleFormat(var AItem: TMangaInfoData);
    procedure SortCache;
    function FindCacheIndex(ALink: String): Integer;
    function GetCacheNode(ATree: TVirtualStringTree; ALink: String): PVirtualNode;
    procedure AddCacheItem(const ALink, ATitle, AAltTitles, AAuthors, AArtists,
      AGenres, AStatus, ASummary: String; ANumChapter, AJDN: Integer);
    procedure UpdateCacheItem(const ALink, ATitle, AAltTitles, AAuthors, AArtists,
      AGenres, AStatus, ASummary: String; ANumChapter, AJDN: Integer);
    procedure DeleteCacheItem(const ALink: String);

    function RegexEscapeInput(const Input: String): String;
    function RegexEscapeAltTitles(const ATitle: String): String;

    function Search(ATitle: String): Boolean;
    function CanFilter(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const {%H-}minusDay: Integer;
      const haveAllChecked, searchNewManga: Boolean): Boolean;
    function BuildFTSMatchString(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stSummary: String;
      const haveAllChecked: Boolean): String;
    procedure GenerateSQLFilterFTS(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const minusDay: Integer; const haveAllChecked, searchNewManga: Boolean);
    procedure GenerateSQLFilterREGEX(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const minusDay: Integer; const haveAllChecked, searchNewManga: Boolean);
    procedure GenerateSQLFilter(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const minusDay: Integer; const haveAllChecked, searchNewManga, useRegExpr: Boolean);
    function Filter(const checkedGenres, uncheckedGenres: TStringList;
      const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
      const minusDay: Integer; const haveAllChecked, searchNewManga: Boolean;
      const useRegExpr: Boolean = False): Boolean;

    function WebsiteLoaded(const AWebsite: String): Boolean;

    procedure CreateDatabase(const AWebsite: String = '');
    procedure GetFieldNames(const List: TStringList);
    procedure Close;
    procedure CloseTable;
    procedure Save;
    procedure Backup(const AWebsite: String);
    procedure Refresh(RecheckDataCount: Boolean = False);

    function CheckData(const ALink: String; const AFieldIndex: Integer): TField;
    function ExistsData(const ALink: String): Boolean;
    function AddData(const Title, AltTitles, Link, Authors, Artists, Genres, Status, Summary: String;
      NumChapter, JDN: Integer; ExitExistsCheck: Boolean): Boolean; overload;
    function AddData(const Title, AltTitles, Link, Authors, Artists, Genres, Status, Summary: String;
      NumChapter: Integer; JDN: TDateTime): Boolean; overload; inline;
    function AddData(const Title, AltTitles, Link, Authors, Artists, Genres, Status, Summary: String;
      NumChapter: Integer; JDN: TDateTime; ExitExistsCheck: Boolean): Boolean; overload; inline;
    function UpdateData(const Title, AltTitles, Link, Authors, Artists, Genres, Status, Summary: String;
      NumChapter: Integer): Boolean;
    function DeleteData(const RecIndex: Integer): Boolean;

    procedure Commit;
    procedure Rollback;
    procedure RemoveFilter;
    procedure Sort; 
                                                  
    function GoToRecNo(const ARecIndex: Integer): Boolean;
    procedure GetCurrentRecordValues(const DataIndex: Integer; out Data: TMangaInfoData);
    function GetSQLFieldValue(const AIndex, AFieldIndex: Integer): String;
    function GetSQLFieldValueInt(const AIndex, AFieldIndex: Integer): Integer;
    function GetFieldValue(const AIndex, AFieldIndex: Integer): String;
    function GetFieldValueInt(const AIndex, AFieldIndex: Integer): Integer;

    property Module: Pointer read FModule;
    property Website: String read FWebsite write FWebsite;
    property TableName: String read FTableName write FTableName;
    property Connected: Boolean read GetConnected;
    property RecordCount: Integer read FRecordCount;
    property DBRecordCount: Integer read FDBRecordCount;
    property Filtered: Boolean read GetFiltered;
    property FilterAllSites: Boolean read FFilterAllSites write FFilterAllSites;
    property SitesList: TStringList read FSitesList write FSitesList;
    property Value[const RecIndex, FieldIndex: Integer]: String read GetSQLFieldValue; default;
    property ValueInt[const RecIndex, FieldIndex: Integer]: Integer read GetSQLFieldValueInt;
    property Connection: TSQLite3ConnectionH read FConn;
    property Transaction: TSQLTransaction read FTrans;
    property Table: TSQLQuery read FReadQuery;
  end;

const
  DB_FIELDS: array[0..9] of TDBFieldDef = (
    (Name: 'link';       TypeAndConstraints: 'TEXT NOT NULL PRIMARY KEY'),
    (Name: 'title';      TypeAndConstraints: 'TEXT'),
    (Name: 'alttitles';  TypeAndConstraints: 'TEXT'),
    (Name: 'authors';    TypeAndConstraints: 'TEXT'),
    (Name: 'artists';    TypeAndConstraints: 'TEXT'),
    (Name: 'genres';     TypeAndConstraints: 'TEXT'),
    (Name: 'status';     TypeAndConstraints: 'TEXT'),
    (Name: 'summary';    TypeAndConstraints: 'TEXT'),
    (Name: 'numchapter'; TypeAndConstraints: 'INTEGER'),
    (Name: 'jdn';        TypeAndConstraints: 'INTEGER')
  );

  DBTempFieldWebsiteIndex = Length(DB_FIELDS);

var             
  DBDataProcessParams: array[0..High(DB_FIELDS)] of String;

  DBDataProcessParam: String;
  DBDataProcessParamInsert: String;
  DBDataProccesCreateParam: String;
                                   
  DBDataProcessParamNoKey: String;
  DBDataProcessParamFTS: String;
  DBDataProcessParamFTSOld: String;
  DBDataProcessParamFTSNew: String;

function DBDataFilePath(const AModuleID: String): String;
function DBDataFileExist(const AModuleID: String): Boolean;
procedure CopyDBDataProcess(const AWebsite, NWebsite: String);
function DeleteDBDataProcess(const AWebsite: String): Boolean;
procedure OverwriteDBDataProcess(const AWebsite, NWebsite: String); 
procedure InitDBConstants;
function NormalizeSearchText(const AInput: String): String;
function NaturalCompareCacheTitles(constref Left, Right: TMangaInfoCache): Integer;

implementation

uses
  uBaseUnit, uOptions, MultiLog, WebsiteModules;

function NaturalCompareCallback({%H-}user: pointer; len1: longint;
  data1: pointer; len2: longint; data2: pointer): longint; cdecl;
var
  s1, s2: String;
begin
  SetString(s1, data1, len1);
  SetString(s2, data2, len2);
  Result := NaturalCompareStr(s1, s2);
end;

procedure RegexCallback(context: PSqlite3_Context; argc: longint;
  argv: PPSqlite3_Value); cdecl;
var
  regexp, Text: PChar;
  regex: TRegExpr;
begin
  if sqlite3_user_data(context) = nil then
  begin
    sqlite3_result_int64(context, 0);
    Exit;
  end;

  if argc <> 2 then
  begin
    sqlite3_result_int64(context, 0);
    Exit;
  end;

  regexp := sqlite3_value_text(argv[0]);
  Text := sqlite3_value_text(argv[1]);

  if (regexp = nil) or (Text = nil) then
  begin
    sqlite3_result_int64(context, 0);
    Exit;
  end;

  try
    regex := TRegExpr(sqlite3_user_data(context));
    regex.Expression := regexp;
    sqlite3_result_int64(context, int64(regex.Exec(Text)));
  except
    sqlite3_result_int64(context, 0);
  end;
end;

function QuotedLike(const S: String): String;
begin
  Result := QuotedStr('%' + S + '%');
end;

function DBDataFilePath(const AModuleID: String): String;
begin
  Result := DATA_FOLDER + AModuleID + DBDATA_EXT;
end;

function DBDataFileExist(const AModuleID: String): Boolean;
begin
  Result := FileExists(DATA_FOLDER + AModuleID + DBDATA_EXT);
end;

procedure CopyDBDataProcess(const AWebsite, NWebsite: String);
begin
  if NWebsite = '' then
  begin
    Exit;
  end;

  if not DBDataFileExist(AWebsite) then
  begin
    Exit;
  end;

  try
    CopyFile(DATA_FOLDER + AWebsite + DBDATA_EXT,
      DATA_FOLDER + NWebsite + DBDATA_EXT,
      [cffPreserveTime, cffOverwriteFile], True);
  except
    on E: Exception do
      SendLogException('CopyDBDataProcess.Error!', E);
  end;
end;

function DeleteDBDataProcess(const AWebsite: String): Boolean;
var
  tryc: Integer;
begin
  Result := True;

  if not FileExists(DATA_FOLDER + AWebsite + DBDATA_EXT) then
  begin
    Exit;
  end;

  tryc := 0;
  while not DeleteFile(DATA_FOLDER + AWebsite + DBDATA_EXT) do
  begin 
    Inc(tryc);
    if tryc > 3 then
    begin
      Break;
    end;

    Sleep(250);
  end;

  Result := not FileExists(DATA_FOLDER + AWebsite + DBDATA_EXT);
end;

procedure OverwriteDBDataProcess(const AWebsite, NWebsite: String);
var
  OldFile, NewFile: String;
  Retries, MaxRetries, SleepMS: Integer;
begin
  SleepMS := 250;
  MaxRetries := 3;
  OldFile := DATA_FOLDER + AWebsite + DBDATA_EXT;
  NewFile := DATA_FOLDER + NWebsite + DBDATA_EXT;

  if not FileExists(DATA_FOLDER + NWebsite + DBDATA_EXT) then
  begin
    Exit;
  end;

  if not DeleteDBDataProcess(AWebsite) then
  begin
    Exit;
  end;

  Retries := 0;
  while not RenameFile(NewFile, OldFile) do
  begin
    Inc(Retries);
    if Retries > MaxRetries then
    begin
      SendLogError('OverwriteDBDataProcess[' + AWebsite + '] Failed to rename temp DB to original path.');
      Exit;
    end;

    Sleep(SleepMS);
  end;
end;

procedure InitDBConstants;
var
  i: Integer;
  FieldName, FieldType: String;
begin
  DBDataProcessParam := '';
  DBDataProcessParamInsert := '';
  DBDataProccesCreateParam := '';

  for i := Low(DB_FIELDS) to High(DB_FIELDS) do
  begin
    FieldName := DB_FIELDS[i].Name;
    FieldType := DB_FIELDS[i].TypeAndConstraints;

    DBDataProcessParams[i] := FieldName;

    if i > Low(DB_FIELDS) then
    begin
      DBDataProcessParam := DBDataProcessParam + ',';
      DBDataProcessParamInsert := DBDataProcessParamInsert + ',';
      DBDataProccesCreateParam := DBDataProccesCreateParam + ',';
    end;

    DBDataProcessParam := DBDataProcessParam + '"' + FieldName + '"';
    DBDataProcessParamInsert := DBDataProcessParamInsert + ':' + FieldName;
    DBDataProccesCreateParam := DBDataProccesCreateParam + '"' + FieldName + '" ' + FieldType;

    if Pos('PRIMARY KEY', FieldType) > 0 then
    begin
      Continue; 
    end;

    if DBDataProcessParamNoKey <> '' then
    begin
      DBDataProcessParamNoKey := DBDataProcessParamNoKey + ',';
    end;

    DBDataProcessParamNoKey := DBDataProcessParamNoKey + '"' + FieldName + '"';

    if (Pos('TEXT', FieldType) = 0) or (FieldName = 'status') then
    begin
      Continue; 
    end;

    if DBDataProcessParamFTS <> '' then
    begin
      DBDataProcessParamFTS := DBDataProcessParamFTS + ',';
      DBDataProcessParamFTSOld := DBDataProcessParamFTSOld + ',';
      DBDataProcessParamFTSNew := DBDataProcessParamFTSNew + ',';
    end;

    DBDataProcessParamFTS := DBDataProcessParamFTS + '"' + FieldName + '"';
    DBDataProcessParamFTSOld := DBDataProcessParamFTSOld + 'old.' + FieldName;
    DBDataProcessParamFTSNew := DBDataProcessParamFTSNew + 'new.' + FieldName;
  end;
end;

function NormalizeSearchText(const AInput: String): String;
begin
  Result := AInput;

  Result := StringReplace(Result, '—', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '–', ' ', [rfReplaceAll]);

  Result := StringReplace(Result, '’', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '‘', '''', [rfReplaceAll]);

  Result := UTF8LowerCase(Result);
end;

function NaturalCompareCacheTitles(constref Left, Right: TMangaInfoCache): Integer;
begin
  Result := NaturalCompareStr(Left.Data.Title, Right.Data.Title);
end;

{ TDBDataProcess }

procedure TDBDataProcess.ResetRecNo(Dataset: TDataSet);
begin
  FRecNo := -1;
end;

function TDBDataProcess.GoToRecNo(const ARecIndex: Integer): Boolean;
var
  DistFirst, DistLast, DistCurrent: Integer;
  LastIndex, RecDelta: Integer;
begin
  Result := False;

  if not FReadQuery.Active or (FDBRecordCount <= 0) then
  begin
    Exit;
  end;

  if (ARecIndex < 0) or (ARecIndex >= FDBRecordCount) then
  begin
    Exit;
  end;

  if FRecNo = ARecIndex then
  begin 
    Result := True;
    Exit;
  end;

  LastIndex := FDBRecordCount - 1;
  DistFirst := ARecIndex;
  DistLast  := LastIndex - ARecIndex;

  if (FRecNo >= 0) and (FRecNo <= LastIndex) then
  begin
    DistCurrent := Abs(ARecIndex - FRecNo);
  end
  else
  begin
    // set high integer if invalid FRecNo
    DistCurrent := High(Integer);
  end;

  try
    if (DistFirst < DistCurrent) and (DistFirst <= DistLast) then
    begin
      FReadQuery.First;
      FRecNo := 0;
    end
    else if (DistLast < DistCurrent) and (DistLast < DistFirst) then
    begin
      FReadQuery.Last;
      FRecNo := LastIndex;
    end;

    RecDelta := ARecIndex - FRecNo;
    if RecDelta <> 0 then
    begin
      FReadQuery.MoveBy(RecDelta);
    end;

    FRecNo := ARecIndex;
    Result := not FReadQuery.Eof;
  except
    on E: Exception do
    begin
      FRecNo := -1;
      SendLogException(Self.ClassName + '[' + Website + '].GoToRecNo.Error!', E);
    end;
  end;
end;

procedure TDBDataProcess.GetCurrentRecordValues(const DataIndex: Integer; out Data: TMangaInfoData);
var
  DBCacheIndex: Integer;
begin
  if FSearched then
  begin   
    if (DataIndex < 0) or (DataIndex >= FSearchedCacheIndices.Count) then
    begin
      Exit;
    end;

    DBCacheIndex := FSearchedCacheIndices[DataIndex];
  end
  else
  begin
    if (DataIndex < 0) or (DataIndex >= FDBCache.Count) then
    begin
      Exit;
    end;

    DBCacheIndex := DataIndex;
  end;

  Data := FDBCache[DBCacheIndex].Data;
end;

procedure TDBDataProcess.CreateTable;
begin
  if not FConn.Connected then
  begin
    Exit;
  end;

  Lock;
  try
    try 
      FConn.ExecuteDirect('DROP TABLE IF EXISTS "' + FTableName + '"');
      FConn.ExecuteDirect('CREATE TABLE "' + FTableName + '" (' + DBDataProccesCreateParam + ');');

      FTrans.CommitRetaining;
    except
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '[' + Website + '].CreateTable.Error!', E);
        FTrans.Rollback;
      end;
    end;
  finally
    Unlock;
  end;
end;  

procedure TDBDataProcess.CreateIndexes;
begin
  if not FConn.Connected then
  begin
    Exit;
  end;

  Lock;
  try
    try
      FConn.ExecuteDirect('CREATE INDEX IF NOT EXISTS "idx_' + FTableName +
        '_title" ON "' + FTableName + '" (title COLLATE NATCMP ASC);');

      FConn.ExecuteDirect('CREATE INDEX IF NOT EXISTS "idx_' + FTableName +
        '_jdn" ON "' + FTableName + '" (jdn);');

      FTrans.CommitRetaining;
    except
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '.CreateIndexes.Error!', E);
        FTrans.Rollback;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TDBDataProcess.CreateFTSTable;
var
  FTSTableExists: Boolean;
begin
  if not FConn.Connected then
  begin
    Exit;
  end;

  Lock;
  try
    try
      FTSTableExists := TableExist(FTableName + '_fts');

      // Create virtual fts table for faster filtering
      FConn.ExecuteDirect('CREATE VIRTUAL TABLE IF NOT EXISTS "' + FTableName +
        '_fts" USING fts5(' + DBDataProcessParamFTS + ', content="' + FTableName + '", content_rowid="_rowid_");');

      // Create insert fts trigger for table inserts
      FConn.ExecuteDirect('CREATE TRIGGER IF NOT EXISTS "' + FTableName + '_ai" AFTER INSERT ON "' +
        FTableName + '" BEGIN INSERT INTO "' + FTableName + '_fts" (rowid, ' + DBDataProcessParamFTS +
        ') VALUES (new._rowid_, ' + DBDataProcessParamFTSNew + '); END;');

      // Create delete fts trigger for table deletes
      FConn.ExecuteDirect('CREATE TRIGGER IF NOT EXISTS "' + FTableName + '_ad" AFTER DELETE ON "' +
        FTableName + '" BEGIN INSERT INTO "' + FTableName + '_fts" ("' + FTableName + '_fts", rowid, ' +
        DBDataProcessParamFTS + ') VALUES (''delete'', old._rowid_, ' + DBDataProcessParamFTSOld + '); END;');

      // Create update fts trigger for table updates
      FConn.ExecuteDirect('CREATE TRIGGER IF NOT EXISTS "' + FTableName + '_au" AFTER UPDATE ON "' + FTableName +
        '" BEGIN INSERT INTO "' + FTableName + '_fts"("' + FTableName + '_fts", rowid, ' +
        DBDataProcessParamFTS + ') VALUES (''delete'', old._rowid_, ' + DBDataProcessParamFTSOld +
        '); INSERT INTO "' + FTableName + '_fts"(rowid, ' + DBDataProcessParamFTS +
        ') VALUES (new._rowid_, ' + DBDataProcessParamFTSNew + '); END;');

      if not FTSTableExists then
      begin
        // Populate search indexes for fts table
        FConn.ExecuteDirect('INSERT INTO "' + FTableName + '_fts"("' + FTableName + '_fts") VALUES(''rebuild'');');
      end;

      FTrans.CommitRetaining;
    except
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '.CreateFTSTable.Error!', E);
        FTrans.Rollback;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TDBDataProcess.CreateField(const FieldName, FieldType: String);
begin
  if not FConn.Connected then
  begin
    Exit;
  end;
     
  Lock;
  try
    try  
      FConn.ExecuteDirect('ALTER TABLE "' + FTableName + '" ADD COLUMN "' + FieldName + '" ' + FieldType + ';');

      FTrans.CommitRetaining;
    except
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '[' + Website + '].CreateField.Error!', E);
        FTrans.Rollback;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TDBDataProcess.RenameField(const FieldOldName, FieldNewName: String);
begin
  if not FConn.Connected then
  begin
    Exit;
  end;

  Lock;
  try
    try
      FConn.ExecuteDirect('ALTER TABLE "' + FTableName + '" RENAME COLUMN "' + FieldOldName + '" TO "' + FieldNewName + '";');

      FTrans.CommitRetaining;
    except
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '[' + Website + '].RenameField.Error!', E);
        FTrans.Rollback;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TDBDataProcess.DeleteField(const FieldName: String);
begin
  if not FConn.Connected then
  begin
    Exit;
  end;

  Lock;
  try
    try
      FConn.ExecuteDirect('ALTER TABLE "' + FTableName + '" DROP COLUMN "' + FieldName + '";');

      FTrans.CommitRetaining;
    except
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '[' + Website + '].DeleteField.Error!', E);
        FTrans.Rollback;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TDBDataProcess.ConvertNewTable(const Fields: TStringList);
var
  SharedParams, FieldName: String;
  i, idx: Integer;
begin
  if not FConn.Connected then
  begin
    Exit;
  end;

  SharedParams := '';
  for i := Low(DB_FIELDS) to High(DB_FIELDS) do
  begin
    FieldName := DB_FIELDS[i].Name;
    if Fields.Find(FieldName, idx) then
    begin
      if i > Low(DB_FIELDS) then
      begin
        SharedParams := SharedParams + ',';
      end;

      SharedParams := SharedParams + '"' + FieldName + '"';
    end;
  end;

  Lock;
  try
    try
      FConn.ExecuteDirect('ALTER TABLE "' + FTableName + '" RENAME TO "' + FTableName + '_old"');
      CreateTable;
      FConn.ExecuteDirect('INSERT INTO "' + FTableName + '" (' + SharedParams + ') SELECT ' + SharedParams + ' FROM "' + FTableName + '_old"');
      CreateIndexes;
      CreateFTSTable;
      FConn.ExecuteDirect('DROP TABLE "' + FTableName + '_old"');

      FTrans.CommitRetaining;
    except  
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '[' + Website + '].ConvertNewTable.Error!', E);
        FTrans.Rollback;
      end;
    end;    
  finally
    Unlock;
  end;
end;

procedure TDBDataProcess.GetRecordCount;
var
  baseSQL, countSQL: String;
begin
  FDBRecordCount := 0;
  baseSQL := Trim(FReadQuery.SQL.Text);

  while (Length(baseSQL) > 0) and (baseSQL[Length(baseSQL)] in [';', ' ', #13, #10, #9]) do
  begin
    Delete(baseSQL, Length(baseSQL), 1);
  end;

  if UpperCase(LeftStr(baseSQL, 6)) <> 'SELECT' then
  begin
    Exit;
  end;

  countSQL := 'SELECT COUNT(*) FROM (' + baseSQL + ')';

  Lock;
  try
    FDBRecordCount := StrToIntDef(FConn.ExecuteQuery(countSQL), 0);
  finally
    Unlock;
  end;
end;

function TDBDataProcess.GetConnected: Boolean;
begin
  Result := FConn.Connected;
end;

function TDBDataProcess.GetFiltered: Boolean;
begin
  if FFiltered or FSearched then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TDBDataProcess.InternalOpen(const FilePath: String): Boolean;
begin
  Result := False;

  if FilePath = '' then
  begin
    Exit;
  end;

  try
    if FTrans.Active then
    begin
      FTrans.Active := False;
    end;

    if FConn.Connected then
    begin
      FConn.Connected := False;
    end;

    FConn.DatabaseName := FilePath;
    FConn.CharSet := 'UTF8';
    FConn.Connected := True;

    sqlite3_create_collation(FConn.Handle, PAnsiChar('NATCMP'), SQLITE_UTF8, nil,
      @NaturalCompareCallback);
    sqlite3_create_function(FConn.Handle, PAnsiChar('REGEXP'), 2, SQLITE_UTF8, FRegxp,
      @RegexCallback, nil, nil);
    FTrans.Active := True;
  except
    on E: Exception do
    begin
      SendLogException(Self.ClassName + '[' + Website + '].InternalOpen.Error!', E);
      Result := False;
    end;
  end;

  Result := FConn.Connected;
end;

function TDBDataProcess.CheckAndRepairDatabase(const AModuleID: String): Boolean;
var
  DBPath: String;
begin
  Result := False;
  DBPath := DBDataFilePath(AModuleID);

  if not FileExists(DBPath) then
  begin
    Exit;
  end;

  CheckFieldsExist;
  CreateIndexes;
  CreateFTSTable;

  Result := True;
end;

function TDBDataProcess.ExecuteDirect(SQL: String): Boolean;
begin
  Result := False;

  if not FConn.Connected then
  begin
    Exit;
  end;
     
  Lock;
  try
    try
      FConn.ExecuteDirect(SQL);
      Result := True;
    except
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '[' + Website + '].ExecuteDirect.Error!'#13#10 +
          'SQL: ' + SQL, E);
      end;
    end;
  finally
    Unlock;
  end;
end;

function TDBDataProcess.CheckWebsiteAndFilePath(const AWebsite: String;
  var AFilePath: String): Boolean;
begin
  if FWebsite <> AWebsite then
  begin
    FWebsite := AWebsite;
  end;

  if FWebsite <> '' then
  begin
    FModule := Modules.LocateModule(AWebsite);
    AFilePath := DATA_FOLDER + FWebsite + DBDATA_EXT;
    Result := FileExists(AFilePath);
  end
  else
  begin
    FModule := nil;
    AFilePath := '';
    Result := False;
  end;
end;

constructor TDBDataProcess.Create;
begin
  inherited Create;

  InitCriticalSection(FGuardian);
         
  FTrans := TSQLTransaction.Create(nil);

  FConn := TSQLite3ConnectionH.Create(nil);
  FConn.Transaction := FTrans;

  FDBCache := TMangaCacheList.Create;
  FSearchedCacheIndices := TIntegerList.Create;
  FRegxp := TRegExpr.Create;
  FRegxp.ModifierI := True;
  FSitesList := TStringList.Create;
  FAttachedSites := TStringList.Create;
  FTableName := 'masterlist';
  FSQLSelect := 'SELECT * FROM "' + FTableName + '"';
  FSQLOrderBy := ' ORDER BY "title" COLLATE NATCMP ASC';
  FSQLSelectOrderBy := FSQLSelect + FSQLOrderBy;
  FRecordCount := 0;
  FDBRecordCount := 0;
  FSorted := False;
  FFiltered := False;
  FFilterAllSites := False;

  ResetRecNo(nil); 

  FReadQuery := TSQLQuery.Create(nil);
  FReadQuery.SQL.Text := FSQLSelectOrderBy;
  FReadQuery.PacketRecords := 25;
  FReadQuery.DataBase := FTrans.DataBase;
  FReadQuery.Transaction := FTrans;
  FReadQuery.ReadOnly := True;

  FReadQuery.AfterOpen := @ResetRecNo;
  FReadQuery.AfterInsert := @ResetRecNo;
  FReadQuery.AfterDelete := @ResetRecNo;
  FReadQuery.AfterEdit := @ResetRecNo;
  FReadQuery.AfterRefresh := @ResetRecNo;
                                         

  FWriteQuery := TSQLQuery.Create(nil);
  FWriteQuery.PacketRecords := 25;
  FWriteQuery.DataBase := FTrans.DataBase;
  FWriteQuery.Transaction := FTrans;
  FWriteQuery.ReadOnly := False;

  FWriteQuery.AfterOpen := @ResetRecNo;
  FWriteQuery.AfterInsert := @ResetRecNo;
  FWriteQuery.AfterDelete := @ResetRecNo;
  FWriteQuery.AfterEdit := @ResetRecNo;
  FWriteQuery.AfterRefresh := @ResetRecNo;
end;

destructor TDBDataProcess.Destroy;
begin
  try
    if FConn.Connected then
    begin      
      FWriteQuery.Close;
      FReadQuery.Close;

      Commit;
      Close;
    end;
  except
    on E: Exception do
    begin
      SendLogException(Self.ClassName + '[' + Website + '].Destroy.Error!', E);
    end;
  end;

  FDBCache.Free;
  FSearchedCacheIndices.Free;
  FAttachedSites.Free;
  FSitesList.Free;
  FReadQuery.Free;
  FWriteQuery.Free;
  FTrans.Free;
  FConn.Free;
  FRegxp.Free;
  Finalize(FGuardian);

  inherited Destroy;
end;

procedure TDBDataProcess.Lock;
begin
  EnterCriticalSection(FGuardian);

  if Assigned(FWriteQuery) then
  begin
    FWriteQuery.DisableControls;
  end;
end;

procedure TDBDataProcess.Unlock;
begin
  try
    if Assigned(FWriteQuery) then
    begin
      FWriteQuery.EnableControls;
    end;
  finally  
    LeaveCriticalSection(FGuardian);
  end;
end;

function TDBDataProcess.Connect(const AWebsite: String): Boolean;
var
  filepath: String = '';
begin
  Result := False;
     
  Lock;
  try
    if CheckWebsiteAndFilePath(AWebsite, filepath) then
    begin
      Result := InternalOpen(filepath);
    end;
  finally
    Unlock;
  end;
end;

function TDBDataProcess.ConnectFile(const AFile: String): Boolean;
begin
  Result := False;
      
  Lock;
  try
    Result := InternalOpen(AFile);
  finally
    Unlock;
  end;
end;     

procedure TDBDataProcess.ConnectCacheDB(const AIndex: Integer);
begin
  if not FFilterAllSites then
  begin
    Exit;
  end;

  FConnectedWebsite := FWebsite;

  FCacheWebsite := TModuleContainer(FDBCache[AIndex].Data.Module).ID;
  if FCacheWebsite <> FConnectedWebsite then
  begin
    Connect(FCacheWebsite);
  end;
end;

procedure TDBDataProcess.DisconnectCacheDB;
begin 
  if not FFilterAllSites then
  begin
    Exit;
  end;

  if FCacheWebsite <> FConnectedWebsite then
  begin
    Connect(FConnectedWebsite);
  end;
end;

function TDBDataProcess.Open(const AWebsite: String): Boolean;
begin
  Lock;

  try
    Close;
    Result := False;

    if not Connect(AWebsite) then
    begin
      Exit;
    end;

    try
      if not TableExist(FTableName) then
      begin
        CreateTable;
      end;

      CheckAndRepairDatabase(AWebsite);

      OpenTable(FTableName, True);
      Result := FReadQuery.Active;
    except
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '.Open.Error!', E);
      end;
    end;
  finally
    Unlock;
  end;
end;

function TDBDataProcess.OpenTable(const ATableName: String;
  CheckRecordCount: Boolean): Boolean;
begin
  Result := FReadQuery.Active;

  if not FConn.Connected then
  begin
    Exit;  
  end;

  try
    if ATableName <> '' then
    begin
      FTableName := ATableName;
    end;

    if FTableName = '' then
    begin
      Exit;
    end;

    if TableExist(FTableName) then
    begin
      if FReadQuery.Active then
      begin
        FReadQuery.Close;
      end;

      if not FTrans.Active then
      begin
        FTrans.Active := True;
      end;

      if CheckRecordCount then
      begin
        GetRecordCount;
      end; 

      FReadQuery.SQL.Text := FSQLSelectOrderBy;
      FReadQuery.Open;

      SetDBCache;
    end;
  except
    on E: Exception do
      SendLogException(Self.ClassName + '[' + Website + '].OpenTable.Error!', E);
  end;

  Result := FReadQuery.Active;
end;

function TDBDataProcess.TableExist(const ATableName: String): Boolean;
var
  ts: TStringList;
  i: Integer;
begin
  Result := False;

  if not FConn.Connected then
  begin
    Exit;
  end;

  ts := TStringList.Create;
  try
    FConn.GetTableNames(ts);
    ts.Sorted := True;
    Result := ts.Find(ATableName, i);
  finally
    ts.Free;
  end;
end;

procedure TDBDataProcess.CheckFieldsExist;
var
  CurrentDBFields: TStringList;
  FieldOldName, FieldNewName: String;
  i: Integer;
begin
  if not FConn.Connected then
  begin
    Exit;
  end;

  CurrentDBFields := TStringList.Create;

  Lock;
  try
    try
      FConn.GetFieldNames(FTableName, CurrentDBFields);

      if CurrentDBFields.Count <> Length(DB_FIELDS) then
      begin
        CurrentDBFields.Sorted := True;
        ConvertNewTable(CurrentDBFields);
      end
      else
      begin
        for i := Low(DB_FIELDS) to High(DB_FIELDS) do
        begin
          FieldOldName := CurrentDBFields[i];
          FieldNewName := DB_FIELDS[i].Name;

          if not SameText(FieldOldName, FieldNewName) then
          begin
            RenameField(FieldOldName, FieldNewName);
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '[' + FTableName + '].CheckFieldsExist.Error!', E);
      end;
    end;
  finally
    CurrentDBFields.Free;
    Unlock;
  end;
end;
    
procedure TDBDataProcess.CloseTable;
begin
  if not FReadQuery.Active then
  begin
    Exit;
  end;

  FDBCache.Clear;
  FSearchedCacheIndices.Clear;
  FRecordCount := 0;
  FDBRecordCount := 0;
  RemoveFilter;
  FReadQuery.Close;
end;

procedure TDBDataProcess.SetDBCache(AClearCache: Boolean = True);
var
  Stmt: psqlite3_stmt;
  DBHandle: psqlite3;
  Item: TMangaInfoData;
  CacheItem: TMangaInfoCache;

  function ColToStr(ColIdx: Integer): String;
  var
    P: PAnsiChar;
  begin
    P := PAnsiChar(sqlite3_column_text(Stmt, ColIdx));

    if P = nil then
    begin
      Result := '';
    end
    else
    begin
      Result := String(P);
    end;
  end;

begin
  if (FDBCache = nil) or (FDBRecordCount = 0) then
  begin
    Exit;
  end;

  if AClearCache then
  begin
    FDBCache.Clear;
  end;

  FDBCache.Capacity := FDBCache.Count + FDBRecordCount;

  DBHandle := psqlite3(FConn.Handle);
  if sqlite3_prepare_v2(DBHandle, PChar(FReadQuery.SQL.Text), -1, @Stmt, nil) <> SQLITE_OK then
  begin
    Exit;
  end;

  try
    FRecNo := 0;
    while sqlite3_step(Stmt) = SQLITE_ROW do
    begin
      Item.Link       := ColToStr(DATA_PARAM_LINK);
      Item.Title      := ColToStr(DATA_PARAM_TITLE);
      Item.AltTitles  := ColToStr(DATA_PARAM_ALTTITLES);
      Item.Authors    := ColToStr(DATA_PARAM_AUTHORS);
      Item.Artists    := ColToStr(DATA_PARAM_ARTISTS);
      Item.Genres     := ColToStr(DATA_PARAM_GENRES);
      Item.Status     := ColToStr(DATA_PARAM_STATUS);
      Item.Summary    := ColToStr(DATA_PARAM_SUMMARY);
      Item.NumChapter := sqlite3_column_int(Stmt, DATA_PARAM_NUMCHAPTER);
      Item.JDN        := sqlite3_column_int(Stmt, DATA_PARAM_JDN);

      Item.Module := FModule;

      SetCacheTitleFormat(Item);

      CacheItem.SearchKey := NormalizeSearchText(Item.Title + ', ' + Item.AltTitles);
      CacheItem.Data := Item;
      FDBCache.Add(CacheItem);
      Inc(FRecNo);
    end;

    FRecordCount := FDBCache.Count;
  finally
    sqlite3_finalize(Stmt);
  end;
end; 

procedure TDBDataProcess.SetCacheTitleFormat(var AItem: TMangaInfoData);
begin
  AItem.TitleFormat := AItem.Title + ' (' + IntToStr(AItem.NumChapter) + ')';

  if FFilterAllSites then
  begin
    AItem.TitleFormat += ' [' + TModuleContainer(AItem.Module).Name + ']';
  end;
end;

procedure TDBDataProcess.SortCache;
begin
  if (FDBCache = nil) or (FDBCache.Count < 2) then
  begin
    Exit;
  end;

  FDBCache.Sort(specialize TComparer<TMangaInfoCache>.Construct(@NaturalCompareCacheTitles));
end;

function TDBDataProcess.FindCacheIndex(ALink: String): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FDBCache.Count - 1 do
  begin
    if FDBCache[i].Data.Link = ALink then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TDBDataProcess.GetCacheNode(ATree: TVirtualStringTree; ALink: String): PVirtualNode;
var
  Node: PVirtualNode;
  Index: Integer;
begin
  Result := nil;
  Index := FindCacheIndex(ALink);

  Node := ATree.GetFirst;
  while Node <> nil do
  begin
    if Node^.Index = Cardinal(Index) then
    begin
      Exit(Node);
    end;

    Node := ATree.GetNext(Node);
  end;
end;

procedure TDBDataProcess.AddCacheItem(const ALink, ATitle, AAltTitles, AAuthors,
  AArtists, AGenres, AStatus, ASummary: String; ANumChapter, AJDN: Integer);
var
  DataItem: TMangaInfoData;
  CacheItem: TMangaInfoCache;
begin
  DataItem.Link := ALink;
  DataItem.Title := ATitle;
  DataItem.AltTitles := AAltTitles;
  DataItem.Authors := AAuthors;
  DataItem.Artists := AArtists;
  DataItem.Genres := AGenres;
  DataItem.Status := AStatus;
  DataItem.Summary := ASummary;
  DataItem.NumChapter := ANumChapter;
  DataItem.JDN := AJDN;
  DataItem.Module := FModule;

  SetCacheTitleFormat(DataItem);

  CacheItem.SearchKey := NormalizeSearchText(DataItem.Title + ', ' + DataItem.AltTitles);
  CacheItem.Data := DataItem;

  FDBCache.Add(CacheItem);
  Inc(FRecordCount);
  SortCache;

  if FSearched then
  begin
    Search(FSearchedTitle);
  end;
end;

procedure TDBDataProcess.UpdateCacheItem(const ALink, ATitle, AAltTitles, AAuthors,
  AArtists, AGenres, AStatus, ASummary: String; ANumChapter, AJDN: Integer);
var
  DataItem: TMangaInfoData;
  CacheItem: TMangaInfoCache;
  CacheIndex: Integer;
begin
  CacheIndex := FindCacheIndex(ALink);

  if CacheIndex < 0 then
  begin
    Exit;
  end;

  DataItem.Link := ALink;
  DataItem.Title := ATitle;
  DataItem.AltTitles := AAltTitles;
  DataItem.Authors := AAuthors;
  DataItem.Artists := AArtists;
  DataItem.Genres := AGenres;
  DataItem.Status := AStatus;
  DataItem.Summary := ASummary;
  DataItem.NumChapter := ANumChapter;
  DataItem.JDN := AJDN;
  DataItem.Module := FDBCache[CacheIndex].Data.Module;

  SetCacheTitleFormat(DataItem);

  CacheItem.SearchKey := NormalizeSearchText(DataItem.Title + ', ' + DataItem.AltTitles);
  CacheItem.Data := DataItem;

  FDBCache[CacheIndex] := CacheItem;
  SortCache;

  if FSearched then
  begin
    Search(FSearchedTitle);
  end;
end;

procedure TDBDataProcess.DeleteCacheItem(const ALink: String);
var
  CacheIndex: Integer;
begin
  CacheIndex := FindCacheIndex(ALink);

  if CacheIndex < 0 then
  begin
    Exit;
  end;

  FDBCache.Delete(CacheIndex);
  Dec(FRecordCount);
  SortCache;

  if FSearched then
  begin
    Search(FSearchedTitle);
  end;
end;

procedure TDBDataProcess.Close;
begin
  FRecordCount := 0;
  FDBRecordCount := 0;

  if not FConn.Connected then
  begin
    Exit;
  end;

  try
    FReadQuery.Close;
    RemoveFilter;
    FConn.Close;
    FConn.DatabaseName := '';
    FSorted := False;
    FWebsite := '';
  except
    on E: Exception do
    begin
      SendLogException(Self.ClassName + '[' + Website + '].Close.Error!', E);
    end;
  end;
end;

procedure TDBDataProcess.Save;
begin
  Commit;
end;

procedure TDBDataProcess.Backup(const AWebsite: String);
begin
  if AWebsite = '' then
  begin
    Exit;
  end;

  if not FConn.Connected then
  begin
    Exit;
  end;

  with TSQLite3Backup.Create do
  begin
    try
      Backup(FConn, DATA_FOLDER + AWebsite + DBDATA_EXT);
    finally
      Free;
    end;
  end;
end;

procedure TDBDataProcess.Refresh(RecheckDataCount: Boolean);
begin 
  if not FConn.Connected then
  begin
    Exit;
  end;
     
  Lock;
  try
    if FReadQuery.Active then
    begin
      if RecheckDataCount then
      begin
        GetRecordCount;
      end;

      FReadQuery.Refresh;
    end
    else if Trim(FReadQuery.SQL.Text) <> '' then
    begin
      if RecheckDataCount then
      begin
        GetRecordCount;
      end;

      FReadQuery.Open;
    end;

    SetDBCache;
  finally
    Unlock;
  end;
end;

function TDBDataProcess.GetSQLFieldValue(const AIndex, AFieldIndex: Integer): String;
begin
  if AIndex < 0 then
  begin
    Exit;
  end;

  if AFieldIndex in [DATA_PARAM_NUMCHAPTER, DATA_PARAM_JDN] then
  begin
    Result := '0';
    Exit;
  end;

  if not FReadQuery.Active then
  begin
    Exit;
  end;

  if not GoToRecNo(AIndex) then
  begin
    Exit
  end;

  case AFieldIndex of
    DATA_PARAM_NUMCHAPTER, DATA_PARAM_JDN:
      Result := '0';
  else
    if (AFieldIndex >= Low(DB_FIELDS)) and (AFieldIndex <= High(DB_FIELDS)) then
    begin
      Result := FReadQuery.FieldByName(DB_FIELDS[AFieldIndex].Name).AsString;
    end;
  end;
end;   

function TDBDataProcess.GetSQLFieldValueInt(const AIndex, AFieldIndex: Integer): Integer;
begin    
  if AIndex < 0 then
  begin
    Exit;
  end;

  if not (AFieldIndex in [DATA_PARAM_NUMCHAPTER, DATA_PARAM_JDN]) then
  begin
    Result := 0;
    Exit;
  end;

  if not FReadQuery.Active then
  begin
    Exit;
  end;

  if not GoToRecNo(AIndex) then
  begin
    Exit
  end;

  case AFieldIndex of
    DATA_PARAM_NUMCHAPTER, DATA_PARAM_JDN:
      if (AFieldIndex >= Low(DB_FIELDS)) and (AFieldIndex <= High(DB_FIELDS)) then
      begin
        Result := FReadQuery.FieldByName(DB_FIELDS[AFieldIndex].Name).AsInteger;
      end;
  else
    Result := 0;
  end;
end;

function TDBDataProcess.GetFieldValue(const AIndex, AFieldIndex: Integer): String;
begin
  if AIndex < 0 then
  begin
    Result := '';
    Exit;
  end
  else if (FDBCache.Count = 0) or (AIndex > FDBCache.Count) then
  begin
    Result := GetSQLFieldValue(AIndex, AFieldIndex);
    Exit;
  end;

  case AFieldIndex of
    DATA_PARAM_LINK:
      Result := FDBCache[AIndex].Data.Link;
    DATA_PARAM_TITLE:      
      Result := FDBCache[AIndex].Data.Title;
    DATA_PARAM_ALTTITLES:   
      Result := FDBCache[AIndex].Data.AltTitles;
    DATA_PARAM_AUTHORS:    
      Result := FDBCache[AIndex].Data.Authors;
    DATA_PARAM_ARTISTS:   
      Result := FDBCache[AIndex].Data.Artists;
    DATA_PARAM_GENRES:    
      Result := FDBCache[AIndex].Data.Genres;
    DATA_PARAM_STATUS:
      Result := FDBCache[AIndex].Data.Status;
    DATA_PARAM_SUMMARY:  
      Result := FDBCache[AIndex].Data.Summary;
  else
    Result := '0';
  end;
end;

function TDBDataProcess.GetFieldValueInt(const AIndex, AFieldIndex: Integer): Integer;
begin
  if AIndex < 0 then
  begin
    Result := 0;
    Exit;
  end
  else if (FDBCache.Count = 0) or (AIndex > FDBCache.Count) then
  begin
    Result := GetSQLFieldValueInt(AIndex, AFieldIndex);
    Exit;
  end;

  case AFieldIndex of
    DATA_PARAM_NUMCHAPTER:
      Result := FDBCache[AIndex].Data.NumChapter;
    DATA_PARAM_JDN:
      Result := FDBCache[AIndex].Data.JDN;
  else
    Result := 0;
  end;
end;

function TDBDataProcess.CheckData(const ALink: String; const AFieldIndex: Integer): TField;
var
  sqlText, FieldKey: String;
  Field: TField;
begin
  Result := TField.Create(nil);

  if (ALink = '') or (not FConn.Connected) then
  begin
    Exit;
  end;
     
  Lock;
  try
    try
      if FReadQuery.Active then
      begin
        FReadQuery.Close;
      end;
                          
      ConnectCacheDB(FindCacheIndex(ALink));

      FieldKey := DB_FIELDS[DATA_PARAM_LINK].Name;
      sqlText := FReadQuery.SQL.Text;

      FReadQuery.SQL.Text := 'SELECT * FROM "' + FTableName + '" WHERE ("' + FieldKey + '"=:' + FieldKey + ');';
      FReadQuery.Params.ParamByName(FieldKey).AsString := ALink;

      FReadQuery.Open;
      Field := FReadQuery.FindField(DB_FIELDS[AFieldIndex].Name);
      if Assigned(Field) then
      begin
        Result := Field;
      end;

      DisconnectCacheDB;

      FReadQuery.SQL.Text := sqlText;
      FReadQuery.Open;
    except
      on E: Exception do
        SendLogException(ClassName + '[' + Website + '].CheckData.Error!', E);
    end;
  finally
    Unlock;
  end;
end;

function TDBDataProcess.ExistsData(const ALink: String): Boolean;
var       
  sqlText, FieldKey: String;
  Field: TField;
begin 
  Result := False;

  if (ALink = '') or (not FConn.Connected) then
  begin
    Exit;
  end;
     
  Lock;
  try
    try
      if FReadQuery.Active then
      begin
        FReadQuery.Close;
      end;

      ConnectCacheDB(FindCacheIndex(ALink));
                              
      FieldKey := DB_FIELDS[DATA_PARAM_LINK].Name;
      sqlText := FReadQuery.SQL.Text;

      FReadQuery.SQL.Text := 'SELECT * FROM "' + FTableName + '" WHERE ("' + FieldKey + '"=:' + FieldKey + ');';
      FReadQuery.Params.ParamByName(FieldKey).AsString := ALink;

      FReadQuery.Open;
      Field := FReadQuery.FindField(FieldKey);
      if Assigned(Field) then
      begin
        Result := Field.AsString <> '';
      end;

      DisconnectCacheDB;

      FReadQuery.SQL.Text := sqlText;
      FReadQuery.Open;
    except
      on E: Exception do
        SendLogException(ClassName + '[' + Website + '].ExistsData.Error!', E);
    end;
  finally
    Unlock;
  end;
end;

function TDBDataProcess.AddData(const Title, AltTitles, Link, Authors, Artists, Genres,
  Status, Summary: String; NumChapter, JDN: Integer; ExitExistsCheck: Boolean): Boolean;
var
  sql: String;
  i: Integer;
begin
  Result := False;

  if (Link = '') or (not FConn.Connected) then
  begin
    Exit;
  end;

  if ExistsData(Link) then
  begin
    if ExitExistsCheck then
    begin
      Exit;
    end;

    Result := UpdateData(Title, AltTitles, Link, Authors, Artists, Genres, Status, Summary, NumChapter);
    Exit;
  end;
     
  Lock;
  try
    try
      ConnectCacheDB(FindCacheIndex(Link));

      FWriteQuery.SQL.Text := 'INSERT OR IGNORE INTO "' + FTableName + '" (' + DBDataProcessParam + ') VALUES (' + DBDataProcessParamInsert + ');';

      // Set parameters - the parameter binding handles escaping
      FWriteQuery.Params.ParamByName('link').AsString := Link;
      FWriteQuery.Params.ParamByName('title').AsString := Title;
      FWriteQuery.Params.ParamByName('alttitles').AsString := AltTitles;
      FWriteQuery.Params.ParamByName('authors').AsString := Authors;
      FWriteQuery.Params.ParamByName('artists').AsString := Artists;
      FWriteQuery.Params.ParamByName('genres').AsString := Genres;
      FWriteQuery.Params.ParamByName('status').AsString := Status;
      FWriteQuery.Params.ParamByName('summary').AsString := Summary;
      FWriteQuery.Params.ParamByName('numchapter').AsInteger := NumChapter;
      FWriteQuery.Params.ParamByName('jdn').AsInteger := JDN;

      if FWriteQuery.Active then
      begin
        FWriteQuery.Close;
      end;

      FWriteQuery.ExecSQL;

      Result := FWriteQuery.RowsAffected > 0;
      FWriteQuery.Close;

      if Result then
      begin
        AddCacheItem(Link, Title, AltTitles, Authors, Artists, Genres,
          Status, Summary, NumChapter, JDN);
      end;

      DisconnectCacheDB;

      FReadQuery.Open;
    except
      on E: Exception do
      begin
        sql := FWriteQuery.SQL.Text;
        for i := 0 to FWriteQuery.Params.Count - 1 do
        begin
          sql := StringReplace(sql, ':' + FWriteQuery.Params[i].Name, QuotedStr(FWriteQuery.Params[i].AsString), [rfReplaceAll, rfIgnoreCase]);
        end;
        SendLogException(ClassName + '[' + Website + '].AddData.Error!' + LineEnding + sql, E);
      end;
    end;
  finally
    Unlock;
  end;
end;

function TDBDataProcess.AddData(const Title, AltTitles, Link, Authors, Artists, Genres,
  Status, Summary: String; NumChapter: Integer; JDN: TDateTime): Boolean;
begin
  Result := AddData(Title, AltTitles, Link, Authors, Artists, Genres, Status, Summary,
    NumChapter, DateToJDN(JDN), False);
end;

function TDBDataProcess.AddData(const Title, AltTitles, Link, Authors, Artists, Genres,
  Status, Summary: String; NumChapter: Integer; JDN: TDateTime; ExitExistsCheck: Boolean): Boolean;
begin
  Result := AddData(Title, AltTitles, Link, Authors, Artists, Genres, Status, Summary,
    NumChapter, DateToJDN(JDN), ExitExistsCheck);
end;

function TDBDataProcess.UpdateData(const Title, AltTitles, Link, Authors, Artists, Genres,
  Status, Summary: String; NumChapter: Integer): Boolean;
var
  lastUpdated: Integer;
  sql: String;
  i: Integer;
begin
  Result := False;

  if (Link = '') or
     (not FConn.Connected) then
  begin
    Exit;
  end;

  lastUpdated := CheckData(Link, DATA_PARAM_JDN).AsInteger;
  if (lastUpdated > (DateToJDN(Now - 1))) then
  begin
    Exit;
  end;
     
  Lock;
  try
    try
      ConnectCacheDB(FindCacheIndex(Link));

      FWriteQuery.SQL.Text := 'UPDATE "' + FTableName + '" SET ' +
        '"title" = :title, ' +
        '"alttitles" = :alttitles, ' +
        '"authors" = :authors, ' +
        '"artists" = :artists, ' +
        '"genres" = :genres, ' +
        '"status" = :status, ' +
        '"summary" = :summary, ' +
        '"numchapter" = :numchapter ' +
        'WHERE "link" = :link';

      // Set parameters                  
      FWriteQuery.Params.ParamByName('link').AsString := Link;
      FWriteQuery.Params.ParamByName('title').AsString := Title;
      FWriteQuery.Params.ParamByName('alttitles').AsString := AltTitles;
      FWriteQuery.Params.ParamByName('authors').AsString := Authors;
      FWriteQuery.Params.ParamByName('artists').AsString := Artists;
      FWriteQuery.Params.ParamByName('genres').AsString := Genres;
      FWriteQuery.Params.ParamByName('status').AsString := Status;
      FWriteQuery.Params.ParamByName('summary').AsString := Summary;
      FWriteQuery.Params.ParamByName('numchapter').AsInteger := NumChapter;
      
      if FWriteQuery.Active then
      begin
        FWriteQuery.Close;
      end;

      FWriteQuery.ExecSQL;
      Result := FWriteQuery.RowsAffected > 0;
      FWriteQuery.Close;

      if Result then
      begin
        UpdateCacheItem(Link, Title, AltTitles, Authors, Artists, Genres,
          Status, Summary, NumChapter, DateToJDN(Now));
      end;
                 
      DisconnectCacheDB;

      FReadQuery.Open;
    except
      on E: Exception do
      begin
        sql := FWriteQuery.SQL.Text;
        for i := 0 to FWriteQuery.Params.Count - 1 do
        begin
          sql := StringReplace(sql, ':' + FWriteQuery.Params[i].Name, QuotedStr(FWriteQuery.Params[i].AsString), [rfReplaceAll, rfIgnoreCase]);
        end;

        SendLogException(ClassName + '[' + Website + '].UpdateData.Error!' + LineEnding + sql, E);
      end;
    end;
  finally
    Unlock;
  end;
end;

function TDBDataProcess.DeleteData(const RecIndex: Integer): Boolean;
var
  Link: String;
  CacheIndex: Integer;
begin
  Result := False;

  try
    if FSearched then
    begin
      CacheIndex := FSearchedCacheIndices[RecIndex];
    end
    else
    begin
      CacheIndex := RecIndex;
    end;
    Link := FDBCache[CacheIndex].Data.Link;
    ConnectCacheDB(CacheIndex);

    FWriteQuery.SQL.Text := 'DELETE FROM "' + FTableName + '" WHERE "link" = :link';
    FWriteQuery.Params.ParamByName('link').AsString := Link;

    if FWriteQuery.Active then
    begin
      FWriteQuery.Close;
    end;

    FWriteQuery.ExecSQL;

    Result := FWriteQuery.RowsAffected > 0;

    if Result then
    begin
      DeleteCacheItem(Link);
    end;

    DisconnectCacheDB;

    FReadQuery.Open;
  except
    on E: Exception do
    begin
      SendLogException(ClassName + '[' + Website + '].DeleteData.Error!',E);
    end;
  end;
end;

procedure TDBDataProcess.Commit;
var
  queryactive: Boolean;
begin 
  if not FConn.Connected then
  begin
    Exit;
  end;
     
  Lock;
  try
    try
      queryactive := FWriteQuery.Active;
      if FWriteQuery.Active then
      begin
        FWriteQuery.Close;
      end;

      FTrans.CommitRetaining;
      if FWriteQuery.Active <> queryactive then
      begin
        FWriteQuery.Active := queryactive;
      end;
    except
      on E: Exception do
      begin
        SendLogException(Self.ClassName + '[' + Website + '].Commit.Error!',E);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TDBDataProcess.Rollback;
begin
  if not FConn.Connected then
  begin
    Exit;
  end;

  try
    FTrans.Rollback;
  except
    on E: Exception do
    begin
      SendLogException(Self.ClassName + '[' + Website + '].Rollback.Error!',E);
    end;
  end;
end;

function TDBDataProcess.RegexEscapeInput(const Input: String): String;
const
  RegexSpecialChars = ['.', '+', '*', '?', '^', '$', '(', ')', '[', ']', '{', '}', '|', '\'];
var
  i: Integer;
begin
  Result := '';

  for i := 1 to Length(Input) do
  begin
    if CharInSet(Input[i], RegexSpecialChars) then
    begin
      Result := Result + '\'; // Add escape character
    end;

    Result := Result + Input[i];
  end;
end;

function TDBDataProcess.RegexEscapeAltTitles(const ATitle: String): String;
const
  HeadRegex = '(?i)(^|,)[ \\t\\r\\n]*';
  TailRegex = '[ \\t\\r\\n]*(,|$)';
begin
  Result := HeadRegex + RegexEscapeInput(ATitle) + TailRegex;
end;

function TDBDataProcess.Search(ATitle: String): Boolean;
var
  CleanInput: String;
  Tokens: specialize TArray<String>;
  I, K: Integer;
  Item: TMangaInfoCache;
  TokenMatches: Boolean;
begin
  CleanInput := Trim(NormalizeSearchText(ATitle));
  FSearchedCacheIndices.Clear;

  if CleanInput = '' then
  begin
    FSearched := False; 
    FSearchedTitle := '';
    FRecordCount := FDBCache.Count;
    Exit(True);
  end;

  FSearchedTitle := CleanInput;
  Tokens := CleanInput.Split([' ', ','], TStringSplitOptions.ExcludeEmpty);

  if Tokens = nil then
  begin
    FSearched := False;
    FSearchedTitle := '';
    FRecordCount := FDBCache.Count;
    Exit(True);
  end;

  for I := 0 to FDBCache.Count - 1 do
  begin
    Item := FDBCache[I];
    TokenMatches := True;

    for K := 0 to High(Tokens) do
    begin
      if Pos(Tokens[K], Item.SearchKey) = 0 then
      begin
        TokenMatches := False;
        Break;
      end;
    end;

    if TokenMatches then
    begin
      FSearchedCacheIndices.Add(I);
    end;
  end;

  FSearched := True;
  FRecordCount := FSearchedCacheIndices.Count;
end;

function TDBDataProcess.CanFilter(const checkedGenres, uncheckedGenres: TStringList;
  const stTitle, stAuthors, stArtists, stStatus, stSummary: String;
  const minusDay: Integer; const haveAllChecked, searchNewManga: Boolean): Boolean;
begin
  Result := True;

  if not FReadQuery.Active then
  begin
    Exit(False);
  end;

  if ((stTitle = '') and
    (stAuthors = '') and
    (stArtists = '') and
    (stSummary = '') and
    (stStatus = '4') and
    (checkedGenres.Count = 0) and
    (uncheckedGenres.Count = 0)) and
    (not searchNewManga) and
    haveAllChecked then
  begin
    Result := False;
  end;
end;

function TDBDataProcess.BuildFTSMatchString(const checkedGenres,
  uncheckedGenres: TStringList; const stTitle, stAuthors, stArtists,
  stSummary: String; const haveAllChecked: Boolean): String;
var
  FTSClauses: TStringList;
  i: Integer;
  CheckedStr, UncheckedStr, OpStr: String;

  function Clean(const AText: String): String;
  begin
    Result := StringReplace(AText, '"', '""', [rfReplaceAll]);
  end;

begin
  Result := '';
  FTSClauses := TStringList.Create;

  try
    if Trim(stTitle) <> '' then
    begin
      FTSClauses.Add('(title : "' + Clean(stTitle) + '"* OR alttitles : "' + Clean(stTitle) + '"*)');
    end;

    if Trim(stAuthors) <> '' then
    begin
      FTSClauses.Add('authors : "' + Clean(stAuthors) + '"*');
    end;

    if Trim(stArtists) <> '' then
    begin
      FTSClauses.Add('artists : "' + Clean(stArtists) + '"*');
    end;

    if Trim(stSummary) <> '' then
    begin
      FTSClauses.Add('summary : "' + Clean(stSummary) + '"*');
    end;

    if checkedGenres.Count > 0 then
    begin
      if haveAllChecked then
      begin
        OpStr := ' AND ';
      end
      else
      begin
        OpStr := ' OR ';
      end;

      CheckedStr := '';
      for i := 0 to checkedGenres.Count - 1 do
      begin
        if CheckedStr <> '' then
        begin
          CheckedStr := CheckedStr + OpStr;
        end;

        CheckedStr := CheckedStr + '"' + Clean(checkedGenres[i]) + '"';
      end;

      FTSClauses.Add('genres : (' + CheckedStr + ')');
    end;

    if uncheckedGenres.Count > 0 then
    begin
      UncheckedStr := '';
      for i := 0 to uncheckedGenres.Count - 1 do
      begin
        if UncheckedStr <> '' then
        begin
          UncheckedStr := UncheckedStr + ' OR ';
        end;

        UncheckedStr := UncheckedStr + '"' + Clean(uncheckedGenres[i]) + '"';
      end;

      FTSClauses.Add('NOT genres : (' + UncheckedStr + ')');
    end;

    if FTSClauses.Count > 0 then
    begin
      Result := FTSClauses[0];

      for i := 1 to FTSClauses.Count - 1 do
      begin
        if SameText(Copy(FTSClauses[i], 1, 4), 'NOT ') then
        begin
          Result := Result + ' ' + FTSClauses[i];
        end
        else
        begin
          Result := Result + ' AND ' + FTSClauses[i];
        end;
      end;
    end
    else
    begin
      Result := '';
    end;
  finally
    FTSClauses.Free;
  end;
end; 

procedure TDBDataProcess.GenerateSQLFilterFTS(const checkedGenres,
  uncheckedGenres: TStringList; const stTitle, stAuthors, stArtists, stStatus,
  stSummary: String; const minusDay: Integer; const haveAllChecked,
  searchNewManga: Boolean);
var
  FTSMatchStr: String;
  WhereClauses: TStringList;
  i: Integer;
begin
  FTSMatchStr := BuildFTSMatchString(checkedGenres, uncheckedGenres, stTitle, stAuthors,
    stArtists, stSummary, haveAllChecked);

  FReadQuery.SQL.Add('SELECT m.* FROM "' + FTableName + '" m');

  if FTSMatchStr <> '' then
  begin
    FReadQuery.SQL.Add(' JOIN "' + FTableName + '_fts" fts ON m.rowid = fts.rowid');
  end;

  WhereClauses := TStringList.Create;
  try
    if FTSMatchStr <> '' then
    begin
      WhereClauses.Add('fts."' + FTableName + '_fts" MATCH ' + QuotedStr(FTSMatchStr));
    end;

    if searchNewManga then
    begin
      WhereClauses.Add('m.jdn > ' + IntToStr(DateToJDN(Now) - minusDay));
    end;

    if stStatus <> '4' then
    begin
      WhereClauses.Add('m.status = ' + QuotedStr(stStatus));
    end;

    if WhereClauses.Count > 0 then
    begin
      FReadQuery.SQL.Add(' WHERE ' + WhereClauses[0]);

      for i := 1 to WhereClauses.Count - 1 do
      begin
        FReadQuery.SQL.Add(' AND ' + WhereClauses[i]);
      end;
    end;
  finally
    WhereClauses.Free;
  end;
end;

procedure TDBDataProcess.GenerateSQLFilterREGEX(const checkedGenres,
  uncheckedGenres: TStringList; const stTitle, stAuthors, stArtists, stStatus,
  stSummary: String; const minusDay: Integer; const haveAllChecked,
  searchNewManga: Boolean);
var
  WhereClauses: TStringList;
  i: Integer;
  GenreExpr, GenreOp: String;
begin
  FReadQuery.SQL.Add('SELECT * FROM "' + FTableName + '"');

  WhereClauses := TStringList.Create;
  try
    if Trim(stTitle) <> '' then
    begin
      WhereClauses.Add('(title REGEXP ' + QuotedStr(stTitle) +
        ' OR alttitles REGEXP ' + QuotedStr(stTitle) + ')');
    end;

    if Trim(stAuthors) <> '' then
    begin
      WhereClauses.Add('authors REGEXP ' + QuotedStr(stAuthors));
    end;

    if Trim(stArtists) <> '' then
    begin
      WhereClauses.Add('artists REGEXP ' + QuotedStr(stArtists));
    end;

    if Trim(stSummary) <> '' then
    begin
      WhereClauses.Add('summary REGEXP ' + QuotedStr(stSummary));
    end;

    if stStatus <> '4' then
    begin
      WhereClauses.Add('status = ' + QuotedStr(stStatus));
    end;

    if searchNewManga then
    begin
      WhereClauses.Add('jdn > ' + IntToStr(DateToJDN(Now) - minusDay));
    end;

    if checkedGenres.Count > 0 then
    begin
      if haveAllChecked then
      begin
        GenreOp := ' AND ';
      end
      else
      begin
        GenreOp := ' OR ';
      end;

      GenreExpr := '';
      for i := 0 to checkedGenres.Count - 1 do
      begin
        if GenreExpr <> '' then
        begin
          GenreExpr := GenreExpr + GenreOp;
        end;

        GenreExpr := GenreExpr + 'genres REGEXP ' + QuotedStr(checkedGenres[i]);
      end;

      WhereClauses.Add('(' + GenreExpr + ')');
    end;

    if uncheckedGenres.Count > 0 then
    begin
      GenreExpr := '';
      for i := 0 to uncheckedGenres.Count - 1 do
      begin
        if GenreExpr <> '' then
        begin
          GenreExpr := GenreExpr + ' AND ';
        end;

        GenreExpr := GenreExpr + 'genres NOT REGEXP ' + QuotedStr(uncheckedGenres[i]);
      end;

      WhereClauses.Add('(' + GenreExpr + ')');
    end;

    if WhereClauses.Count > 0 then
    begin
      FReadQuery.SQL.Add(' WHERE ' + WhereClauses[0]);

      for i := 1 to WhereClauses.Count - 1 do
      begin
        FReadQuery.SQL.Add(' AND ' + WhereClauses[i]);
      end;
    end;
  finally
    WhereClauses.Free;
  end;
end;

procedure TDBDataProcess.GenerateSQLFilter(const checkedGenres,
  uncheckedGenres: TStringList; const stTitle, stAuthors, stArtists, stStatus,
  stSummary: String; const minusDay: Integer; const haveAllChecked,
  searchNewManga, useRegExpr: Boolean);
begin
  FReadQuery.SQL.Clear;

  if not useRegExpr then
  begin
    GenerateSQLFilterFTS(checkedGenres, uncheckedGenres, stTitle, stAuthors,
    stArtists, stStatus, stSummary, minusDay, haveAllChecked, searchNewManga);
  end
  else
  begin
    GenerateSQLFilterREGEX(checkedGenres, uncheckedGenres, stTitle, stAuthors,
    stArtists, stStatus, stSummary, minusDay, haveAllChecked, searchNewManga);
  end;
end;

function TDBDataProcess.Filter(const checkedGenres,
  uncheckedGenres: TStringList; const stTitle, stAuthors, stArtists, stStatus,
  stSummary: String; const minusDay: Integer; const haveAllChecked,
  searchNewManga: Boolean; const useRegExpr: Boolean): Boolean;
var        
  i: Integer;
  MainSite: String;
  SiteModule: TModuleContainer;
begin
  Result := False;
  if FReadQuery.Active = False then
  begin
    Exit;
  end;

  if not CanFilter(checkedGenres, uncheckedGenres, stTitle, stAuthors,
    stArtists, stStatus, stSummary, minusDay, haveAllChecked, searchNewManga) then
  begin
    Exit;
  end;
      
  Lock;
  try
    GenerateSQLFilter(checkedGenres, uncheckedGenres, stTitle, stAuthors, stArtists,
    stStatus, stSummary, minusDay, haveAllChecked, searchNewManga, useRegExpr);
                      
    FDBCache.clear;
    if FFilterAllSites then
    begin 
      MainSite := FWebsite;

      for i := 0 to FSitesList.Count - 1 do
      begin
        SiteModule := TModuleContainer(FSitesList.Objects[i]);

        if FileExists(DBDataFilePath(SiteModule.ID)) then
        begin
          Connect(SiteModule.ID);

          CheckAndRepairDatabase(SiteModule.ID);

          GetRecordCount;
          SetDBCache(False);
        end;
      end;

      Connect(MainSite);
      SortCache;
    end
    else
    begin
      SetDBCache(False);
    end;

    FRecordCount := FDBCache.Count;
    FFiltered := True;
    FReadQuery.SQL.Text := FSQLSelectOrderBy;
    FReadQuery.Open;
  finally
    Unlock;
  end;
end;

procedure TDBDataProcess.CreateDatabase(const AWebsite: String);
var
  filepath: String;
begin
  filepath := '';
  Close;

  if CheckWebsiteAndFilePath(AWebsite, filepath) then
  begin
    DeleteFile(filepath);
  end;

  if not ForceDirectories(DATA_FOLDER) then
  begin
    Exit;
  end;

  InternalOpen(filepath);
  ExecuteDirect('PRAGMA auto_vacuum = FULL;');
  CreateTable;
  CreateIndexes;
  CreateFTSTable;
end;

procedure TDBDataProcess.GetFieldNames(const List: TStringList);
begin
  if (List <> nil) and (FReadQuery.Active) then
  begin
    FReadQuery.GetFieldNames(List);
  end;
end;

procedure TDBDataProcess.RemoveFilter;
begin
  if not FFiltered then
  begin
    Exit;
  end;

  FFilterAllSites := False;
  FFiltered := False;
  FReadQuery.SQL.Text := FSQLSelectOrderBy;
  FRecordCount := 0;
end;

procedure TDBDataProcess.Sort;
begin
  if not FConn.Connected then
  begin
    Exit;
  end;

  FSorted := True;

  Lock;
  try
    with FConn do
    begin
      try
        ExecuteDirect('DROP TABLE IF EXISTS "' + FTableName + '_ordered"');
        ExecuteDirect('CREATE TABLE "' + FTableName + '_ordered" (' + DBDataProccesCreateParam + ')');
        ExecuteDirect('INSERT INTO "' + FTableName + '_ordered" (' + DBDataProcessParam + ') SELECT ' + DBDataProcessParam + ' FROM "' + FTableName + '" ORDER BY "title" COLLATE NATCMP');
        ExecuteDirect('DROP TABLE "' + FTableName + '"');
        ExecuteDirect('ALTER TABLE "' + FTableName + '_ordered" RENAME TO "' + FTableName + '"');
      except
        on E: Exception do
        begin
          SendLogException(Self.ClassName + '[' + Website + '].Sort.Error!', E);
        end;
      end;
    end;

    FTrans.Commit;
  finally
    Unlock;
  end;
end;

function TDBDataProcess.WebsiteLoaded(const AWebsite: String): Boolean;
var
  i: Integer;
begin 
  Result := False;

  if FWebsite = AWebsite then
  begin      
    Result := True;
    Exit;
  end;

  if not FFilterAllSites then
  begin
    Exit;
  end;

  for i := 0 to FSitesList.Count - 1 do
  begin
    if FSitesList[i] = AWebsite then
    begin
      Result := True;
      Break;
    end;
  end;
end;

initialization
  InitDBConstants;

end.
