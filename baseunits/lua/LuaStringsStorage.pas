unit LuaStringsStorage;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaClass,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

type

  { TStringsStorage }

  TStringsStorage = class
  private
    FStrings: TStringList;
    FCS: TRTLCriticalSection;
    function GetValues(const AName: String): String;
    procedure SetValues(const AName: String; AValue: String);
    function GetText: String;
    procedure SetText(AValue: String);
  public
    Tag: Integer;
    Enable: Boolean;
    Status: String;
    constructor Create;
    destructor Destroy; override;
    procedure Remove(const AName: String);
    property Values[const AName: String]: String read GetValues write SetValues; default;
    property Text: String read GetText write SetText;
  end;

procedure luaStringsStorageAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);

implementation

uses
  LuaUtils;

{ TStringsStorage }

function TStringsStorage.GetValues(const AName: String): String;
begin
  Result := FStrings.Values[AName];
end;

function TStringsStorage.GetText: String;
begin
  Result := FStrings.Text;
end;

procedure TStringsStorage.SetText(AValue: String);
begin
  EnterCriticalsection(FCS);
  try
    FStrings.Text := AValue;
  finally
    LeaveCriticalsection(FCS);
  end;
end;

procedure TStringsStorage.SetValues(const AName: String; AValue: String);
begin
  EnterCriticalsection(FCS);
  try
    FStrings.Values[AName] := AValue;
  finally
    LeaveCriticalsection(FCS);
  end;
end;

constructor TStringsStorage.Create;
begin
  FStrings := TStringList.Create;
  InitCriticalSection(FCS);
  Tag := 0;
  Enable := True;
  Status := 'my status of ' + Self.ClassName;
end;

destructor TStringsStorage.Destroy;
begin
  FStrings.Free;
  DoneCriticalsection(FCS);
  inherited Destroy;
end;

procedure TStringsStorage.Remove(const AName: String);
var
  i: Integer;
begin
  i := FStrings.IndexOfName(AName);
  if (i < 0) or (i >= FStrings.Count) then
    Exit;
  EnterCriticalsection(FCS);
  try
    FStrings.Delete(i);
  finally
    LeaveCriticalsection(FCS);
  end;
end;

type
  TUserData = TStringsStorage;

function strings_destroy(L: Plua_State): Integer; cdecl;
begin
  TUserData(luaClassGetObject(L)).Free;
  Result := 0;
end;

function strings_getvalue(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).GetValues(luaToString(L, 1)));
  Result := 1;
end;

function strings_setvalue(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).SetValues(luaToString(L, 1), luaToString(L, 2));
end;

function strings_remove(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Remove(luaToString(L, 1));
end;

function strings_gettext(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, TUserData(luaClassGetObject(L)).Text);
  Result := 1;
end;

function strings_settext(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
  TUserData(luaClassGetObject(L)).Text := luaToString(L, 1);
end;

const
  methods: packed array [0..3] of luaL_Reg = (
    (name: 'Remove'; func: @strings_remove),
    (name: 'Free'; func: @strings_destroy),
    (name: 'Destroy'; func: @strings_destroy),
    (name: nil; func: nil)
    );

procedure luaStringsStorageAddMetaTable(const L: Plua_State; const Obj: Pointer;
  const MetaTable, UserData: Integer);
begin
  luaClassAddFunction(L, MetaTable, UserData, methods);
  luaClassAddDefaultArrayProperty(L, MetaTable, UserData, @strings_getvalue, @strings_setvalue);
  luaClassAddProperty(L, MetaTable, UserData, 'Text', @strings_gettext, @strings_settext);
  luaClassAddIntegerProperty(L, MetaTable, 'Tag', @TUserData(Obj).Tag);
  luaClassAddBooleanProperty(L, MetaTable, 'Enable', @TUserData(Obj).Enable);
  luaClassAddStringProperty(L, MetaTable, 'Status', @TUserData(Obj).Status);
end;

initialization
  luaClassRegister(TUserData, @luaStringsStorageAddMetaTable);

end.
