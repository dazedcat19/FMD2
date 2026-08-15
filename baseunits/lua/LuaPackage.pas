unit LuaPackage;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, FileCache, LuaBase,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

procedure RegisterLoader(const L: Plua_State);
procedure AddLib(const AName: String; const ARegLib: lua_CFunction);

procedure ClearCache;

var
  LuaLibDir: String = 'lua' + DirectorySeparator;

implementation

uses
  FileUtil, LazFileUtils, MultiLog, LuaUtils;

const
  LIBPREFIX = 'fmd.';

type

  TLuaLib = class
    RegLib: lua_CFunction;
  end;

  { TCachedPackage }

  TCachedPackage = class
  public
    FileName: String;
    Stream: TMemoryStream;
    constructor Create(const AFileName: String; const AStream: TMemoryStream);
    destructor Destroy; override;
  end;

var
  HostPackage,
  Package: TFileCache;

{ TCacheItem }

constructor TCachedPackage.Create(const AFileName: String;
  const AStream: TMemoryStream);
begin
  FileName := AFileName;
  Stream := AStream;
end;

destructor TCachedPackage.Destroy;
begin
  Stream.Free;
  inherited Destroy;
end;

{ LuaPackage }

function _findpackage(L: Plua_State): Integer; cdecl;
var
  i: Integer;
  o: TLuaLib;
  c: TCachedPackage;
  p: String;
begin
  p := luaToString(L, 1);

  if p.StartsWith(LIBPREFIX) then
  begin
    o := TLuaLib(HostPackage.Find(p));
    if o <> nil then
    begin
      lua_pushcfunction(L, o.RegLib);
      Exit(1);
    end
  end;

  c := TCachedPackage(Package.Find(p));
  if c <> nil then
  begin
    i := LuaLoadFromStreamOrFile(L, c.Stream, c.FileName);

    if i = 0 then
    begin
      Exit(1);
    end
    else
    begin
      Logger.SendError('require '+QuotedStr(p)+' '+LuaGetReturnString(i)+': '+lua_tostring(L,-1));
    end;
  end;

  Result := 0;
end;

procedure RegisterLoader(const L: Plua_State);
var
  top, loaders, i: Integer;
begin
  top:=lua_gettop(L);
  lua_getglobal(L, 'package');

  if LUA_VERSION_NUM = 501 then
  begin
    lua_getfield(L, -1, 'loaders');
  end
  else
  begin
    lua_getfield(L, -1, 'searchers');
  end;

  loaders := lua_gettop(L);
  i := 0;
  repeat
    Inc(i);
    lua_rawgeti(L, loaders, i);

  until lua_type(L, -1) <= LUA_TNIL;

  lua_pop(L, 1);
  for i := i downto 2 do // shift items down to make a room
  begin
    lua_rawseti(L, loaders, i);
  end;

  lua_pushcfunction(L, @_findpackage);
  lua_rawseti(L, loaders, 1);
  lua_settop(L, top);
end;

function LoadLuaFile(const AFileName: String): TObject;
var
  f: String;
  m: TMemoryStream;
begin
  Result := nil;
  f := LuaLibDir + AFileName.Replace('.', DirectorySeparator) + '.lua';

  if FileExists(f) then
  begin
    m := LuaDumpFileToStream(f);

    if m <> nil then
    begin
      Result := TCachedPackage.Create(f, m);
    end;
  end;
end;

procedure InitPackages;
begin
  if HostPackage = nil then
  begin
    HostPackage := TFileCache.Create;
  end;

  if Package = nil then
  begin
    Package := TFileCache.Create(@LoadLuaFile);
  end;
end;

procedure AddLib(const AName: String; const ARegLib: lua_CFunction);
var
  lib: TLuaLib;
begin
  lib := TLuaLib.Create;
  lib.RegLib := ARegLib;

  InitPackages;
  HostPackage.Add(LIBPREFIX + AName, TObject(lib));
end;

procedure ClearCache;
begin
  Package.Clear;
end;

initialization
  InitPackages;

finalization
  Package.Free;
  HostPackage.Free;

end.
