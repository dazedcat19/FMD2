unit LuaPCRE2;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LuaPackage,
  {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses
  pcre2, pcre2lib, MultiLog, LuaUtils;

{/* directly pass the pointer by lua stack to pcre2 lib to save memory /*}

type
  { TLuaPCRE2Engine }

  TLuaPCRE2Engine = class
  public
    re: ppcre2_code_8;
    Expression,
    InputString: PAnsiChar;
    InputLength,
    ExpressionLength,
    StartOffset,
    ErrorOffset: Cardinal;
    ErrorCode: Integer;
    MatchData: ppcre2_match_data_8;
    MatchCount: Integer;
    OVector: PPCRE2_SIZE;
    destructor Destroy; override;
    function Exec: Boolean;
    function ExecNext: Boolean;
  end;

{ TLuaPCRE2Engine }

destructor TLuaPCRE2Engine.Destroy;
begin
  if MatchData<>nil then
    pcre2_match_data_free_8(MatchData);
  if re<>nil then
    pcre2_code_free_8(re);
  inherited Destroy;
end;

function TLuaPCRE2Engine.Exec: Boolean;
begin
  if MatchData<>nil then
    pcre2_match_data_free_8(MatchData);
  MatchData := pcre2_match_data_create_from_pattern_8(re, nil);
  MatchCount := pcre2_match_8(re, InputString, InputLength, StartOffset, 0, MatchData, nil);
  OVector := pcre2_get_ovector_pointer_8(MatchData);
  Result := MatchCount >= 0;
end;

function TLuaPCRE2Engine.ExecNext: Boolean;
begin
  Result := Exec;
  if Result then
    StartOffset := ovector.GetStart(0)+ovector.GetLength(0)-1
  else
    StartOffset := InputLength;
end;

function CreateLuaPCRE2Engine(L: Plua_State): TLuaPCRE2Engine;
begin
  Result := TLuaPCRE2Engine.Create;
  Result.InputString := lua_tolstring(L, 1, @Result.InputLength);
  Result.Expression := lua_tolstring(L, 2, @Result.ExpressionLength);
  Result.StartOffset := 0;
  if lua_gettop(L) >= 3 then
  begin
    if lua_isnumber(L, 3) then
      Result.StartOffset := lua_tointeger(L, 3) - 1  // lua index starts at 1 while pcre2 starts at 0
    else
    if (lua_gettop(L) > 3) and lua_isnumber(L, 4) then  // for gsub with 4 arguments
      Result.StartOffset := lua_tointeger(L, 4) - 1;
  end;

  Result.re := pcre2_compile_8(Result.Expression, Result.ExpressionLength, PCRE2_UTF, @Result.ErrorCode, @Result.ErrorOffset, nil);
  if Result.re = nil then
  begin
    Logger.Send(pcre2.GetErrorMessage(Result.ErrorCode, Result.ErrorOffset));
    Result.Free;
    Result := nil;
  end;
end;

function re_exec(L: Plua_State): Integer; cdecl;
var
  RE: TLuaPCRE2Engine;
  r: Boolean;
begin
  r := False;
  RE := CreateLuaPCRE2Engine(L);
  if RE <> nil then
  begin
    r := RE.Exec;
    RE.Free;
  end;
  lua_pushboolean(L, r);
  Result := 1;
end;

{ mimicing lua string.find return the position start and end instead of length }

function re_find(L: Plua_State): Integer; cdecl;
var
  RE: TLuaPCRE2Engine;
  i: Cardinal;
begin
  Result := 0;
  RE := CreateLuaPCRE2Engine(L);
  if RE <> nil then
  begin
    if RE.Exec then
    begin
       i := RE.OVector.GetStart(0);
       lua_pushinteger(L, i);
       lua_pushinteger(L, i-1 + RE.Ovector.GetLength(0));
       Result := 2;
    end;
    RE.Free;
  end;
end;

function internalmatch(L: Plua_State; RE: TLuaPCRE2Engine): Integer;
var
  i: Integer;
begin
  Result := 0;
  if RE.ExecNext then
  begin
    if RE.MatchCount = 1 then
    begin
      lua_pushlstring(L, RE.InputString + RE.OVector.GetStart(0) - 1, RE.OVector.GetLength(0));
      Result := 1;
    end
    else
      for i := 1 to RE.MatchCount - 1 do
      begin
        lua_pushlstring(L, RE.InputString + RE.OVector.GetStart(i) - 1, RE.OVector.GetLength(i));
        Inc(Result);
      end;
  end;
end;

function re_match(L: Plua_State): Integer; cdecl;
var
  RE: TLuaPCRE2Engine;
begin
  RE := CreateLuaPCRE2Engine(L);
  if RE <> nil then
  begin
    Result := internalmatch(L, RE);
    RE.Free;
  end
  else
    Result := 0;
end;

function internalgmatch(L: Plua_State): Integer; cdecl;
var
  RE: TLuaPCRE2Engine;
begin
  RE := TLuaPCRE2Engine(lua_topointer(L, 1));
  Result := internalmatch(L, RE);
  if Result = 0 then RE.Free;
end;

function internalgmatchnil(L: Plua_State): Integer; cdecl;
begin
  Result := 0;
end;

function re_gmatch(L: Plua_State): Integer; cdecl;
var
  RE: TLuaPCRE2Engine;
begin
  RE := CreateLuaPCRE2Engine(L);
  if RE <> nil then
  begin
    lua_pushcfunction(L, @internalgmatch);
    lua_pushlightuserdata(L, RE);
    Result := 2;
  end
  else
  begin
    lua_pushcfunction(L, @internalgmatchnil);
    Result := 1;
  end;
end;

const
  PCRE2_GSUB_OPTIONS = PCRE2_SUBSTITUTE_OVERFLOW_LENGTH + PCRE2_SUBSTITUTE_GLOBAL;

function re_gsub(L: Plua_State): Integer; cdecl;
var
  RE: TLuaPCRE2Engine;
  result_string: String;
  result_length: Cardinal;
  replacement: PAnsiChar;
  replacement_length: Cardinal;

  procedure doSubstitute;
  begin
    RE.ErrorCode := pcre2_substitute_8(RE.re, RE.InputString, RE.InputLength,
      RE.StartOffset, PCRE2_GSUB_OPTIONS, nil, nil, replacement, replacement_length,
      PAnsiChar(result_string), @result_length);
    if RE.ErrorCode = PCRE2_ERROR_NOMEMORY then
    begin
      SetLength(result_string, result_length);
      doSubstitute;
    end
    else
      SetLength(result_string, result_length);
  end;

begin
  RE := CreateLuaPCRE2Engine(L);
  result_string := lua_tolstring(L, 1, @result_length);
  replacement := lua_tolstring(L, 3, @replacement_length);
  if RE <> nil then
  begin
    result_length := MAX_PATH;
    SetLength(result_string, result_length);
    doSubstitute;
    RE.Free;
  end;
  lua_pushlstring(L, PAnsiChar(result_string), result_length);
  Result := 1;
end;

const
  methods: packed array [0..5] of luaL_Reg = (
    (name: 'exec'; func: @re_exec),
    (name: 'find'; func: @re_find),
    (name: 'match'; func: @re_match),
    (name: 'gmatch'; func: @re_gmatch),
    (name: 'gsub'; func: @re_gsub),
    (name: nil; func: nil)
    );

function luaopen_pcre2(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, methods);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('pcre2', @luaopen_pcre2);

end.
