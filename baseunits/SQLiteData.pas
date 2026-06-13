unit SQLiteData;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LazFileUtils, MultiLog, strutils, sqlite3conn, sqldb, DB, SQLite3Dyn;

type

  { TSQLite3ConnectionH }

  TSQLite3ConnectionH = class(TSQLite3Connection)
  protected
    procedure DoInternalDisconnect; override;
    procedure sqlite3_handlerror;
  public
    procedure ExecuteSQL(const asql: string); inline;
    function ExecuteQuery(const asql: string): string; inline;
    property Handle read GetHandle;
    property Statements;
  end;

  TExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  { TSQliteData }

  TSQliteData = class
  private
    FAutoVacuum: Boolean;
    FConn: TSQLite3ConnectionH;
    FFieldsParams: String;
    FOnError: TExceptionEvent;
    FTrans: TSQLTransaction;
    FQuery: TSQLQuery;
    FFileName: String;
    FTableName: String;
    FCreateParams: String;
    FSelectParams: String;
    FRecordCount: Integer;
    procedure DoOnError(E: Exception);
    function GetAutoApplyUpdates: Boolean;
    procedure SetAutoApplyUpdates(AValue: Boolean);
    procedure SetAutoVacuum(AValue: Boolean);
    procedure SetCreateParams(AValue: String);
    procedure SetFieldsParams(AValue: String);
    procedure SetOnError(AValue: TExceptionEvent);
    procedure SetSelectParams(AValue: String);
  protected
    function OpenDB: Boolean; virtual;
    function CreateDB: Boolean; virtual;
    function ConvertNewTableIF: Boolean; virtual;
    procedure DoConvertNewTable; virtual;
    procedure GetRecordCount; virtual;
    procedure SetRecordCount(const AValue: Integer); virtual;
    procedure IncRecordCount(const N: Integer = 1);
  public
    constructor Create;
    destructor Destroy; override;
    function Open(const AOpenTable: Boolean = True; const AGetRecordCount: Boolean = True): Boolean; virtual;
    function OpenTable(const AGetRecordCount: Boolean = True): Boolean; virtual;
    procedure Close; virtual;
    procedure CloseTable; virtual;
    procedure Refresh(RecheckDataCount: Boolean = False); virtual;
    procedure Commit; virtual;
    procedure CommitRetaining; virtual;
    procedure Vacuum; virtual;
    procedure Save; virtual;
    function Connected: Boolean; inline;
    property Connection: TSQLite3ConnectionH read FConn;
    property Transaction: TSQLTransaction read FTrans;
    property Table: TSQLQuery read FQuery;
    property Filename: String read FFileName write FFileName;
    property TableName: String read FTableName write FTableName;
    property CreateParams: String read FCreateParams write SetCreateParams;
    property SelectParams: String read FSelectParams write SetSelectParams;
    property FieldsParams: String read FFieldsParams write SetFieldsParams;
    property RecordCount: Integer read FRecordCount;
    property AutoApplyUpdates: Boolean read GetAutoApplyUpdates write SetAutoApplyUpdates;
    property AutoVacuum: Boolean read FAutoVacuum write SetAutoVacuum;
    property OnError: TExceptionEvent read FOnError write SetOnError;
  end;

var
  {$ifdef NO_COMMIT_QUEUE}
  MAX_COMMIT_QUEUE:Integer=0;
  MAX_SQL_FLUSH_QUEUE:Integer=0;
  {$else}
  MAX_COMMIT_QUEUE:Integer=1 shl 4;
  MAX_SQL_FLUSH_QUEUE:Integer=1 shl 8;
  {$endif}
  MAX_BIG_SQL_FLUSH_QUEUE:Integer=1 shl {$ifdef CPU64}14{$else}12{$endif}-1;

type
  { TSQLiteDataWA }

  TSQLiteDataWA = class(TSQliteData)
  private
    FUpdateCount: Integer;
  public
    tempSQL: String;
    tempSQLcount: Integer;
    commitCount: Integer;
    maxSQLqueue: Integer;
    maxCommitQueue: Integer;
    Guardian: TRTLCriticalSection;
  protected
    procedure InternalCommit; inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginUpdate; inline;
    procedure EndUpdate; inline;
    procedure AppendSQL(const SQL:string); inline;
    procedure AppendSQLSafe(const SQL:string); inline;
    procedure FlushSQL(const UseQueue:Boolean=True); inline;
    procedure FlushSQLSafe; inline;
    procedure Commit(const UseQueue:Boolean=True);
  end;

function QuotedStrD(const S: String): String; overload; inline;

function PrepSQLValue(const V: String): String; overload; inline;
function PrepSQLValue(const V: Integer): String; overload; inline;
function PrepSQLValue(const V: Boolean): String; overload; inline;
function PrepSQLValue(const V: TDateTime): String; overload; inline;

implementation

function ToStrZeroPad(const i, len: Word): String;
begin
  Result:=IntToStr(i);
  if Length(Result)<len then
    Result:=StringOfChar('0',len-Length(Result))+Result;
end;

function QuotedStrD(const S: String): String;
begin
  Result := AnsiQuotedStr(S, '"');
end;

function PrepSQLValue(const V: String): String;
begin
  Result:=AnsiQuotedStr(V,'''');
end;

function PrepSQLValue(const V: Integer): String;
begin
  Result:=IntToStr(V);
end;

function PrepSQLValue(const V: Boolean): String;
begin
  Result:=BoolToStr(V,'1','0');
end;

function PrepSQLValue(const V: TDateTime): String;
var
  Year,Month,Day,Hour,Minute,Second,MiliSecond:word;
begin
  DecodeDate(V,Year,Month,Day);
  DecodeTime(V,Hour,Minute,Second,MiliSecond);
  Result:=''''+ToStrZeroPad(Year,4)+'-'+ToStrZeroPad(Month,2)+'-'+ToStrZeroPad(Day,2)+' '+
          ToStrZeroPad(Hour,2)+':'+ToStrZeroPad(Minute,2)+':'+ToStrZeroPad(Second,2)+'.'+ToStrZeroPad(MiliSecond,3)+'''';
end;

{ TSQLiteDataWA }

procedure TSQLiteDataWA.InternalCommit;
begin
  if (commitCount>0) and (FUpdateCount=0) then
  try
    Transaction.CommitRetaining;
    commitCount:=0;
  except
    on E: Exception do
      Transaction.Rollback;
  end;
end;

constructor TSQLiteDataWA.Create;
begin
  inherited Create;
  InitCriticalSection(Guardian);
  maxSQLqueue:=MAX_SQL_FLUSH_QUEUE;
  maxCommitQueue:=MAX_COMMIT_QUEUE;
  Table.PacketRecords:=1;
  Table.UniDirectional:=True;
  tempSQL:='';
  tempSQLcount:=0;
  commitCount:=0;
  FUpdateCount:=0;
end;

destructor TSQLiteDataWA.Destroy;
begin
  FUpdateCount:=0;
  FlushSQL;
  InternalCommit;
  DoneCriticalSection(Guardian);
  inherited Destroy;
end;

procedure TSQLiteDataWA.BeginUpdate;
begin
  InterlockedIncrement(FUpdateCount);
end;

procedure TSQLiteDataWA.EndUpdate;
begin
  if FUpdateCount>0 then
    InterlockedDecrement(FUpdateCount);
  if FUpdateCount=0 then
    FlushSQL;
end;

procedure TSQLiteDataWA.AppendSQL(const SQL: string);
begin
  tempSQL+=SQL;
  Inc(tempSQLcount);
  if tempSQLcount>=maxSQLqueue then
    FlushSQL;
end;

procedure TSQLiteDataWA.AppendSQLSafe(const SQL: string);
begin
  if TryEnterCriticalSection(Guardian)<>0 then
    try
      AppendSQL(SQL);
    finally
      LeaveCriticalSection(Guardian);
    end
  else
    AppendSQL(SQL);
end;

procedure TSQLiteDataWA.FlushSQL(const UseQueue: Boolean);
begin
  if (tempSQLcount>0) and ((FUpdateCount=0) or (UseQueue=False)) then
  begin
    Connection.ExecuteSQL(tempsql);
    tempSQL:='';
    tempSQLcount:=0;
    Inc(commitCount);
  end;
  if commitCount>=maxCommitQueue then
    InternalCommit;
end;

procedure TSQLiteDataWA.FlushSQLSafe;
begin
  if TryEnterCriticalSection(Guardian)<>0 then
    try
      FlushSQL;
    finally
      LeaveCriticalSection(Guardian);
    end
  else
    FlushSQL;
end;

procedure TSQLiteDataWA.Commit(const UseQueue: Boolean);
begin
  if not Connection.Connected then Exit;
  EnterCriticalSection(Guardian);
  try
    FlushSQL(UseQueue);
    if not UseQueue then
      InterlockedExchange(FUpdateCount,0);
    InternalCommit;
  finally
    LeaveCriticalSection(Guardian);
  end;
end;

{ TSQLite3ConnectionH }

procedure TSQLite3ConnectionH.DoInternalDisconnect;
var
  L: TList;
  i: Integer;
  lhandle: psqlite3;
begin
  L := Statements.LockList;
  try
    for i:=0 to L.Count-1 do
      TCustomSQLStatement(L[i]).Unprepare;
    L.Clear;
  finally
    Statements.UnlockList;
  end;
  lhandle:=Handle;
  if lhandle <> nil then
  begin
    checkerror(sqlite3_close_v2(lhandle));
    ReleaseSQLite;
  end;
end;

procedure TSQLite3ConnectionH.sqlite3_handlerror;
var
  ErrMsg: string;
  ErrCode: integer;
begin
  ErrMsg:=strpas(sqlite3_errmsg(Handle));
  ErrCode:=sqlite3_extended_errcode(Handle);
  Logger.SendCallStack(Self.ClassName+' Error '+IntToStr(ErrCode)+': '+ErrMsg);
end;

procedure TSQLite3ConnectionH.ExecuteSQL(const asql: string);
var
  zSql: PAnsiChar;
  zSqlend: PAnsiChar;
  zLeftover: PAnsiChar;
  pStmt: psqlite3_stmt;
  rc: Integer;
begin
  zSql:=PAnsiChar(asql);
  zSqlend:=zSql+Length(asql);
  zLeftover:=nil;
  rc:=SQLITE_OK;

  while (rc=SQLITE_OK) and (zSql<zSqlEnd) do
  begin
    pStmt:=nil;
    rc:=sqlite3_prepare_v2(Handle,zSql,zSqlend-zSql,@pStmt,@zLeftover);
    if rc<>SQLITE_OK then
      sqlite3_handlerror
    else
    try
      rc:=sqlite3_step(pStmt);
      if (rc<>SQLITE_DONE) and (rc<>SQLITE_ROW) then
        sqlite3_handlerror
      else
      begin
        zSql:=zLeftover;
        rc:=SQLITE_OK;
      end;
    finally
      sqlite3_finalize(pStmt);
    end;
  end;
end;

function TSQLite3ConnectionH.ExecuteQuery(const asql: string): string;
var
  zSql: PAnsiChar;
  zSqlend: PAnsiChar;
  zLeftover: PAnsiChar;
  pStmt: psqlite3_stmt;
  rc: Integer;
  i: Integer;
begin
  Result:='';
  zSql:=PAnsiChar(asql);
  zSqlend:=zSql+Length(asql);
  zLeftover:=nil;
  rc:=SQLITE_OK;

  while (rc=SQLITE_OK) and (zSql<zSqlEnd) do
  begin
    pStmt:=nil;
    rc:=sqlite3_prepare_v2(Handle,zSql,zSqlend-zSql,@pStmt,@zLeftover);
    if (rc<>SQLITE_OK) then
      sqlite3_handlerror
    else
    try
      while True do
      begin
        rc:=sqlite3_step(pStmt);
        for i:=0 to sqlite3_column_count(pStmt)-1 do
          Result+=sqlite3_column_text(pStmt,i);
        if rc<>SQLITE_ROW then Break;
      end;
      if rc<>SQLITE_DONE then
        sqlite3_handlerror
      else
      begin
        zSql:=zLeftover;
        rc:=SQLITE_OK;
      end;
    finally
      sqlite3_finalize(pStmt);
    end;
  end;
end;

{ TSQliteData }

procedure TSQliteData.DoOnError(E: Exception);
begin
  if Assigned(OnError) then
    OnError(Self, E);
end;

function TSQliteData.GetAutoApplyUpdates: Boolean;
begin
  Result := sqoAutoApplyUpdates in FQuery.Options;
end;

procedure TSQliteData.SetAutoApplyUpdates(AValue: Boolean);
begin
  if AValue then
    FQuery.Options := FQuery.Options + [sqoAutoApplyUpdates]
  else
    FQuery.Options := FQuery.Options - [sqoAutoApplyUpdates];
end;

procedure TSQliteData.SetAutoVacuum(AValue: Boolean);
begin
  if FAutoVacuum = AValue then Exit;
  FAutoVacuum := AValue;
end;

procedure TSQliteData.SetCreateParams(AValue: String);
begin
  if FCreateParams = AValue then Exit;
  FCreateParams := TrimSet(Trim(AValue), ['(', ')', ';']);
end;

procedure TSQliteData.SetFieldsParams(AValue: String);
begin
  if FFieldsParams = AValue then Exit;
  FFieldsParams := AValue;
end;

procedure TSQliteData.SetOnError(AValue: TExceptionEvent);
begin
  if FOnError = AValue then Exit;
  FOnError := AValue;
end;

procedure TSQliteData.SetSelectParams(AValue: String);
begin
  if FSelectParams = AValue then Exit;
  FSelectParams := AValue;
end;

function TSQliteData.OpenDB: Boolean;
begin
  Result := False;
  if FFileName = '' then Exit;
  try
    FConn.DatabaseName := FFileName;
    FConn.Connected := True;
    FTrans.Active := True;
  except
  end;
  Result := FConn.Connected;
end;

function TSQliteData.CreateDB: Boolean;
begin
  Result := False;
  if (FTableName = '') or (FCreateParams = '') then Exit;
  if not FConn.Connected then
    if not OpenDB then Exit;
  try
    FConn.ExecuteDirect('DROP TABLE IF EXISTS ' + QuotedStrD(FTableName));
    FConn.ExecuteDirect('CREATE TABLE ' + QuotedStrD(FTableName) + ' (' + FCreateParams + ')');
    FTrans.Commit;
    Result := True;
  except
  end;
end;

function TSQliteData.ConvertNewTableIF: Boolean;
begin
  Result := False;
end;

procedure TSQliteData.DoConvertNewTable;
var
  qactive: Boolean;
begin
  if not FConn.Connected then Exit;
  try
    qactive := FQuery.Active;
    if FQuery.Active then FQuery.Close;
    with FConn do
    begin
      ExecuteDirect('DROP TABLE IF EXISTS ' + QuotedStrD('temp' + FTableName));
      ExecuteDirect('CREATE TABLE ' + QuotedStrD('temp' + FTableName) + ' (' + FCreateParams + ')');
      ExecuteDirect('INSERT INTO ' + QuotedStrD('temp' + FTableName) + ' (' + FFieldsParams + ') SELECT ' + FFieldsParams + ' FROM "' + FTableName + '"');
      ExecuteDirect('DROP TABLE ' + QuotedStrD(FTableName));
      ExecuteDirect('ALTER TABLE ' + QuotedStrD('temp' + FTableName) + ' RENAME TO ' + QuotedStrD(FTableName));
    end;
    FTrans.Commit;
    if qactive <> FQuery.Active then
      FQuery.Active := qactive;
  except
    FTrans.Rollback;
  end;
end;

procedure TSQliteData.GetRecordCount;
begin
  FRecordCount:=StrToIntDef(FConn.ExecuteQuery('SELECT COUNT(*) FROM'+QuotedStrD(FTableName)+';'),0);
end;

procedure TSQliteData.SetRecordCount(const AValue: Integer);
begin
  if FRecordCount = AValue then Exit;
  FRecordCount := AValue;
end;

procedure TSQliteData.IncRecordCount(const N: Integer);
begin
  Inc(FRecordCount, N);
end;

procedure TSQliteData.Vacuum;
var
  qactive: Boolean;
begin
  if not FConn.Connected then Exit;
  try
    qactive := FQuery.Active;
    if FQuery.Active then FQuery.Close;
    try
      FConn.ExecuteDirect('END TRANSACTION');
      FConn.ExecuteDirect('VACUUM');
    except
    end;
    FConn.ExecuteDirect('BEGIN TRANSACTION');
    if FQuery.Active <> qactive then
      FQuery.Active := qactive;
  except
    on E: Exception do
      DoOnError(E);
  end;
end;

constructor TSQliteData.Create;
begin
  FConn := TSQLite3ConnectionH.Create(nil);
  FTrans := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  FConn.CharSet := 'UTF8';
  FConn.Transaction := FTrans;
  FQuery.DataBase := FTrans.DataBase;
  FQuery.Transaction := FTrans;
  AutoApplyUpdates := True;
  FAutoVacuum := True;
  FRecordCount := 0;
  FFileName := '';
  FTableName := 'maintable';
  FCreateParams := '';
end;

destructor TSQliteData.Destroy;
begin
  Self.Close;
  FConn.Free;
  FQuery.Free;
  FTrans.Free;
  inherited Destroy;
end;

function TSQliteData.Open(const AOpenTable: Boolean;
  const AGetRecordCount: Boolean): Boolean;
begin
  Result := False;
  if (FFileName = '') or (FCreateParams = '') then Exit;
  if FileExists(FFileName) then
    Result := OpenDB
  else
    Result := CreateDB;
  if Result and AOpenTable then
    Result := OpenTable(AGetRecordCount);
end;

function TSQliteData.OpenTable(const AGetRecordCount: Boolean): Boolean;
begin
  Result := False;
  if not FConn.Connected then Exit;
  try
    if FQuery.Active then FQuery.Close;
    if FSelectParams <> '' then
      FQuery.SQL.Text := FSelectParams
    else
      FQuery.SQL.Text := 'SELECT * FROM ' + QuotedStrD(FTableName);
    FQuery.Open;
    if AGetRecordCount then
      GetRecordCount
    else
      FRecordCount := FQuery.RecordCount;
  except
    on E: Exception do
      DoOnError(E);
  end;
  Result := FQuery.Active;
  if Result and ConvertNewTableIF then DoConvertNewTable;
end;

procedure TSQliteData.Close;
begin
  if not FConn.Connected then Exit;
  try
    Save;
    if FAutoVacuum then
      Vacuum;
    CloseTable;
    FTrans.Active := False;
    FConn.Close;
  except
    on E: Exception do
      DoOnError(E);
  end;
end;

procedure TSQliteData.CloseTable;
begin
  if not FConn.Connected then Exit;
  if not FQuery.Active then Exit;
  try
    FQuery.Close;
    FRecordCount := 0;
  except
    on E: Exception do
      DoOnError(E);
  end;
end;

procedure TSQliteData.Refresh(RecheckDataCount: Boolean);
begin
  if not FConn.Connected then Exit;
  if FQuery.Active then
    FQuery.Refresh
  else
    FQuery.Open;
  if RecheckDataCount then
    GetRecordCount
  else
    FRecordCount := FQuery.RecordCount;
end;

procedure TSQliteData.Commit;
begin
  if not FConn.Connected then Exit;
  try
    if FQuery.Active then
      FQuery.ApplyUpdates;
    Transaction.Commit;
  except
    Transaction.Rollback;
  end;
end;

procedure TSQliteData.CommitRetaining;
begin
  if not FConn.Connected then Exit;
  try
    if FQuery.Active then
      FQuery.ApplyUpdates;
    Transaction.CommitRetaining;
  except
    Transaction.RollbackRetaining;
  end;
end;

procedure TSQliteData.Save;
var
  qactive: Boolean;
begin
  if not FConn.Connected then Exit;
  try
    qactive := FQuery.Active;
    if FQuery.Active then
    begin
      FQuery.ApplyUpdates;
      FQuery.Close;
    end;
    FTrans.Commit;
    if qactive <> qactive then
      FQuery.Active := FQuery.Active;
    if FQuery.Active then
      GetRecordCount;
  except
    on E: Exception do
      DoOnError(E);
  end;
end;

function TSQliteData.Connected: Boolean;
begin
  Result := FConn.Connected and FQuery.Active;
end;

end.
