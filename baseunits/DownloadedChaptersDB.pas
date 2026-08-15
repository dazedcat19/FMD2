unit DownloadedChaptersDB;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SQLiteData, SQLDB, LazFileUtils;

type

  { TDownloadedChaptersDB }

  TDownloadedChaptersDB = class(TSQliteData)
  private
    FGuardian: TRTLCriticalSection;
    FCommitCount: Integer;
    function GetChapters(const AModuleID, ALink: String): String;
    procedure SetChapters(const AModuleID, ALink: String; AValue: String);
  protected
    procedure Lock; inline;
    procedure UnLock; inline;
    procedure InternalCommit; inline;
    procedure CheckCommit; inline;
  public
    constructor Create;
    destructor Destroy; override;
    function Open: Boolean;
    procedure Commit; override;
    procedure Delete(const AModuleID, ALink: String);
    property Chapters[const AModuleID, ALink: String]: String read GetChapters write SetChapters;
  end;

implementation

uses
  uBaseUnit;

function CleanStr(const S: String): String;
begin
  Result := LowerCase(Trim(S));
  if Pos(' ', Result) > 0 then
  begin
    Result := StringReplace(Result, ' ', '', [rfReplaceAll]);
  end;

  while Pos(LineEnding + LineEnding, Result) > 0 do
  begin
    Result := StringReplace(Result, LineEnding + LineEnding, LineEnding, [rfReplaceAll]);
  end;
end;

{ TDownloadedChaptersDB }

function TDownloadedChaptersDB.GetChapters(const AModuleID, ALink: String
  ): String;
begin
  Result := '';
  if not Connected then
  begin
    Exit;
  end;

  Lock;
  try
    with Table do
    begin
      if Locate('id', LowerCase(AModuleID+ALink), []) then
      begin
        Result := Fields[1].AsString;
      end;
    end;
  finally
    UnLock;
  end;
end;

procedure TDownloadedChaptersDB.SetChapters(const AModuleID, ALink: String;
  AValue: String);
var
  posted: Boolean;
begin
  if AValue = '' then
  begin
    Exit;
  end;

  if not Connected then
  begin
    Exit;
  end;

  posted := False;
  Lock;
  try
    with Table do
    begin
      if Locate('id', LowerCase(AModuleID + ALink), []) then
      begin
        Edit;
        Fields[1].AsString := MergeCaseInsensitive([Fields[1].AsString, AValue]);
      end
      else
      begin
        Append;
        Fields[0].AsString := LowerCase(AModuleID + ALink);
        Fields[1].AsString := AValue;
      end;

      try
        Post;
        posted := True;
      except
        CancelUpdates;
      end;
    end;
  finally
    Unlock;
  end;

  if posted then
  begin
    CheckCommit;
  end;
end;

procedure TDownloadedChaptersDB.Lock;
begin
  EnterCriticalSection(FGuardian);
end;

procedure TDownloadedChaptersDB.UnLock;
begin
  LeaveCriticalSection(FGuardian);
end;

procedure TDownloadedChaptersDB.InternalCommit;
begin
  inherited CommitRetaining;

  FCommitCount := 0;
end;

procedure TDownloadedChaptersDB.CheckCommit;
begin
  Inc(FCommitCount);
  if FCommitCount >= MAX_COMMIT_QUEUE then
  begin
    InternalCommit;
  end;
end;

constructor TDownloadedChaptersDB.Create;
begin
  inherited Create;

  InitCriticalSection(FGuardian);

  FCommitCount := 0;
  AutoApplyUpdates := True;
  Table.Options := Table.Options - [sqoAutoCommit];
  Table.PacketRecords := 1;
  Table.UniDirectional := False;
  TableName := 'downloadedchapters';
  CreateParams := '"id" VARCHAR(3000) NOT NULL PRIMARY KEY,' + '"chapters" TEXT';
  FieldsParams := '"id","chapters"';
  SelectParams := 'SELECT ' + FieldsParams + ' FROM ' + QuotedStrD(TableName);
end;

destructor TDownloadedChaptersDB.Destroy;
begin           
  DoneCriticalSection(FGuardian);

  InternalCommit;

  inherited Destroy;
end;

function TDownloadedChaptersDB.Open: Boolean;
begin
  Result := inherited Open(True,False);
end;

procedure TDownloadedChaptersDB.Commit;
begin
  InternalCommit;
end;

procedure TDownloadedChaptersDB.Delete(const AModuleID, ALink: String);
begin
  if not Connected then
  begin
    Exit;
  end;

  Lock;
  try
    with Table do
    begin
      if Locate('id', LowerCase(AModuleID+ALink), []) then
      begin
        Delete;
      end;
    end;
  finally
    UnLock;
  end;

  CheckCommit;
end;

end.

