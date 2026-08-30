unit FileCache;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type

  TOnLoadFile = function(const AFileName: String): TObject;

  { TFileCache }

  TFileCache = class
  private
    FGuardian: TRTLCriticalSection;
    FCachedFiles: TStringList;

  public
    OnLoadFile: TOnLoadFile;

    constructor Create(const AOnLoadFile: TOnLoadFile = nil);
    destructor Destroy; override;

    procedure InitCachedList;
    procedure Add(const AName: String; var AObject: TObject);
    function Find(const AName: String): TObject;
    procedure Clear; inline;
    function Count: Integer; inline;
  end;

implementation


{ TFileCache }

procedure TFileCache.InitCachedList;
begin
  if FCachedFiles <> nil then
  begin
    Exit;
  end;

  FCachedFiles := TStringList.Create;
  FCachedFiles.OwnsObjects := True;
  FCachedFiles.Sorted := True;
  FCachedFiles.Duplicates := dupIgnore;
end;

constructor TFileCache.Create(const AOnLoadFile: TOnLoadFile);
begin
  InitCriticalSection(FGuardian);
  InitCachedList;

  if AOnLoadFile <> nil then
  begin
    OnLoadFile := AOnLoadFile;
  end;
end;

destructor TFileCache.Destroy;
begin
  FCachedFiles.Free;
  DoneCriticalSection(FGuardian);

  inherited Destroy;
end;

procedure TFileCache.Add(const AName: String; var AObject: TObject);
var
  i: Integer;
begin
  InitCachedList;

  i := FCachedFiles.Add(AName);

  if FCachedFiles.Objects[i] = nil then
  begin
    FCachedFiles.Objects[i] := AObject
  end
  else
  begin
    AObject.Free;
    AObject := FCachedFiles.Objects[i];
  end;
end;

function TFileCache.Find(const AName: String): TObject;
var
  i: Integer;
begin
  Result := nil;

  if FCachedFiles.Find(AName, i) then
  begin
    Result := FCachedFiles.Objects[i];
  end
  else if Assigned(OnLoadFile) then
  begin
    EnterCriticalSection(FGuardian);

    try
      Result := OnLoadFile(AName);
      if Result <> nil then
      begin
        Add(AName, Result);
      end;
    finally
      LeaveCriticalSection(FGuardian);
    end;
  end;
end;

procedure TFileCache.Clear;
begin
  FCachedFiles.Clear;
end;

function TFileCache.Count: Integer;
begin
  Result := FCachedFiles.Count;
end;

end.
