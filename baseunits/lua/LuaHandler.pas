unit LuaHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif},
  LuaBase, LuaClass;

type

  { TLuaHandler }

  TLuaHandler = class
  protected
    FHandle: Plua_State;
    FLoadedChunks: TStringList;
    FLoadedObjects: TStringList;
    FCallFunctionCount: Cardinal;
    procedure NewHandle;
    procedure FreeHandle;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Handle: Plua_State read FHandle;
  public
    function LoadChunk(const AName: String; const AChunk: TMemoryStream): Integer;
    function LoadChunkExecute(const AName: String; const AChunk: TMemoryStream): Integer;
    procedure LoadObject(const AName: String; const AObject: TObject; const AddMetaTable: TluaClassAddMetaTable = nil);
    procedure CallFunction(const AFunctionName: String); inline;
    procedure ClearStack; inline;
  end;

implementation

uses httpsendthread;

{ TLuaHandler }

procedure TLuaHandler.NewHandle;
begin
  FreeHandle;
  FHandle := LuaNewBaseState;
  FCallFunctionCount := 0;
end;

procedure TLuaHandler.FreeHandle;
begin
  if FHandle <> nil then
  begin
    lua_close(FHandle);
    FHandle := nil;
  end;
  FLoadedChunks.Clear;
  FLoadedObjects.Clear;
end;

constructor TLuaHandler.Create;
  function createlist: TStringList;
  begin
    Result := TStringList.Create;
    with Result do
    begin
      Sorted := True;
      OwnsObjects := False;
      CaseSensitive := True;
    end;
  end;
begin
  FLoadedChunks := createlist;
  FLoadedObjects := createlist;
  NewHandle;
end;

destructor TLuaHandler.Destroy;
begin
  FreeHandle;
  FLoadedChunks.Free;
  FLoadedObjects.Free;
  inherited Destroy;
end;

function TLuaHandler.LoadChunk(const AName: String;
  const AChunk: TMemoryStream): Integer;
begin
  if FLoadedChunks.IndexOf(AName) <> -1 then Exit(0);
  Result := LuaLoadFromStreamOrFile(FHandle, AChunk, AName);
  if Result = 0 then
    FLoadedChunks.AddObject(AName, AChunk);
end;

function TLuaHandler.LoadChunkExecute(const AName: String;
  const AChunk: TMemoryStream): Integer;
var
  l: Integer;
begin
  Result := 0;

  l := FLoadedChunks.IndexOf(AName);
  if (l <> -1) then
  begin
    if AlwaysLoadLuaFromFile then
    begin
      FLoadedChunks.Delete(l);
    end
    else
    begin
      Exit;
    end;
  end;

  Result := LuaLoadFromStreamOrFile(FHandle, AChunk, AName);
  if Result = 0 then
  begin
    FLoadedChunks.AddObject(AName, AChunk);
    Result := lua_pcall(FHandle, 0, LUA_MULTRET, 0);
  end;
end;

procedure TLuaHandler.LoadObject(const AName: String;
  const AObject: TObject; const AddMetaTable: TluaClassAddMetaTable);
var
  i: Integer = -1;
begin
  if FLoadedObjects.Find(AName, i) then
  begin
    if FLoadedObjects.Objects[i] = AObject then
      Exit
    else
      FLoadedObjects.Delete(i);
  end;
  luaClassPushObject(FHandle, AObject, AName, False, AddMetaTable);
  FLoadedObjects.AddObject(AName, AObject);
  if (AObject is THTTPSendThread) and (TLuaHandler(THTTPSendThread(AObject).LuaHandler) <> self) then
    THTTPSendThread(AObject).LuaHandler := Self;
end;

procedure TLuaHandler.CallFunction(const AFunctionName: String);
begin
  if FCallFunctionCount > 15 then
  begin
    lua_gc(FHandle, LUA_GCCOLLECT, 0);
    lua_gc(FHandle, LUA_GCCOLLECT, 0);
    FCallFunctionCount := 0;
  end;
  LuaCallFunction(FHandle, AFunctionName);
  Inc(FCallFunctionCount);
end;

procedure TLuaHandler.ClearStack;
begin
  lua_settop(FHandle, 0);
end;

end.
