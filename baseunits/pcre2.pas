{ pcre2

  Copyright (C) 2020 riderkick

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}


unit pcre2;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, pcre2lib;

type
  TPCRE2SubstringOffset = packed record
    Start,
    Len: Cardinal;
  end;

  PPCRE2SubstringOffsets = ^TPCRE2SubstringOffsets;
  TPCRE2SubstringOffsets = array of TPCRE2SubstringOffset;
  TPCRE2GroupSubstringOffsets = array of TPCRE2SubstringOffsets;

  { TPCRE2SubsstringOffsetHelper }

  TPCRE2SubsstringOffsetHelper = type helper for TPCRE2SubstringOffset
  public
    function GetString(const s: String): String;
    function IsNil: Boolean;
  end;

  { TPCRE2OVectorHelper }

  TPCRE2OVectorHelper = type helper for PPCRE2_SIZE
  public
    function GetStart(const idx: Integer): Cardinal;
    function GetLength(const idx: Integer): Cardinal;
    function GetSubstringOffset(const idx: Integer): TPCRE2SubstringOffset;
  end;
  //
  TPCRE2SubStrings = array of TPCRE2SubstringOffset;

function Exec(const InputString, Expression: String; const StartOffset: Integer = 1): Boolean;
function Find(const InputString, Expression: String; const StartOffset: Integer = 1): TPCRE2SubstringOffset;
function Match(const InputString, Expression: String; const StartOffset: Integer = 1): TPCRE2SubstringOffsets;
function GMatch(const InputString, Expression: String; const StartOffset: Integer = 1): TPCRE2GroupSubstringOffsets;
function Substitute(const InputString, Expression, Replacement: String; ReplaceAll: Boolean = False): String;
function GSub(const InputString, Expression, Replacement: String): String; inline;
function Version: String;
function GetErrorMessage(const error_code: Integer; const error_offset: Cardinal = 0): String;

implementation

function GetErrorMessage(const error_code: Integer; const error_offset: Cardinal): String;
begin
  setlength(Result, MAX_PATH);
  setlength(Result, pcre2_get_error_message_8(error_code, PAnsiChar(Result), MAX_PATH));
  Result := 'PCRE2 error ' + IntToStr(error_code) + ': ' + Result;
  if error_offset <> 0 then
    Result := Result + ' at ' + IntToStr(error_offset) + ' position';
end;

function Exec(const InputString, Expression: String; const StartOffset: Integer): Boolean;
var
  re: ppcre2_code_8;
  match_data: ppcre2_match_data_8;
  offset_count, error_offset: Integer;
begin
  Result := False;
  re := pcre2_compile_8(PAnsiChar(Expression), Length(Expression), PCRE2_UTF, @offset_count, @error_offset, nil);
  if re <> nil then
  begin
    match_data := pcre2_match_data_create_from_pattern_8(re, nil);
    offset_count := pcre2_match_8(re, PAnsiChar(InputString), Length(InputString), StartOffset, 0, match_data, nil);
    pcre2_match_data_free_8(match_data);
    pcre2_code_free_8(re);
    if offset_count >= 0 then
      Result := True;
  end
  else
    raise Exception.Create(GetErrorMessage(offset_count, error_offset));
end;

function Find(const InputString, Expression: String;
  const StartOffset: Integer): TPCRE2SubstringOffset;
var
  re: ppcre2_code_8;
  match_data: ppcre2_match_data_8;
  offset_count, error_offset: Integer;
  ovector: PPCRE2_SIZE;
begin
  Result.Start := 0;
  Result.Len := 0;
  re := pcre2_compile_8(PAnsiChar(Expression), Length(Expression), PCRE2_UTF,
    @offset_count, @error_offset, nil);
  if re <> nil then
  begin
    match_data := pcre2_match_data_create_from_pattern_8(re, nil);
    offset_count := pcre2_match_8(re, PAnsiChar(InputString), Length(InputString),
      StartOffset - 1, 0, match_data, nil);
    pcre2_match_data_free_8(match_data);
    pcre2_code_free_8(re);
    if offset_count >= 0 then
    begin
      ovector := pcre2_get_ovector_pointer_8(match_data);
      Result := ovector.GetSubstringOffset(0);
    end
    else
      raise Exception.Create(GetErrorMessage(offset_count));
  end
  else
    raise Exception.Create(GetErrorMessage(offset_count, error_offset));
end;

function Match(const InputString, Expression: String; const StartOffset: Integer): TPCRE2SubstringOffsets;
var
  re: ppcre2_code_8;
  match_data: ppcre2_match_data_8;
  offset_count, error_offset, i: Integer;
  ovector: PPCRE2_SIZE;
begin
  SetLength(Result, 0);
  re := pcre2_compile_8(PAnsiChar(Expression), Length(Expression), PCRE2_UTF,
    @offset_count, @error_offset, nil);
  if re <> nil then
  begin
    match_data := pcre2_match_data_create_from_pattern_8(re, nil);
    offset_count := pcre2_match_8(re, PAnsiChar(InputString), Length(InputString),
      StartOffset - 1, 0, match_data, nil);
    pcre2_match_data_free_8(match_data);
    pcre2_code_free_8(re);
    if offset_count >= 0 then
    begin
      ovector := pcre2_get_ovector_pointer_8(match_data);
      if offset_count = 0 then
      begin
        SetLength(Result, 1);
        Result[0] := ovector.GetSubstringOffset(0);
      end
      else
      begin
        SetLength(Result, offset_count - 1);
        for i := 1 to offset_count - 1 do
          Result[i - 1] := ovector.GetSubstringOffset(i);
      end;
    end
    else
      raise Exception.Create(GetErrorMessage(offset_count));
  end
  else
    raise Exception.Create(GetErrorMessage(offset_count, error_offset));
end;

function GMatch(const InputString, Expression: String; const StartOffset: Integer): TPCRE2GroupSubstringOffsets;
var
  re: ppcre2_code_8;
  match_data: ppcre2_match_data_8;
  offset_count, error_offset, i, AStartOffset, InputLength: Integer;
  ovector: PPCRE2_SIZE;
  AMatch: TPCRE2SubstringOffset;
  AResult: PPCRE2SubstringOffsets;
begin
  SetLength(Result, 0);
  re := pcre2_compile_8(PAnsiChar(Expression), Length(Expression), PCRE2_UTF,
    @offset_count, @error_offset, nil);
  if re <> nil then
  begin
    InputLength := Length(InputString);
    AMatch.Start := StartOffset-1;
    AMatch.Len := 0;
    while True do
    begin
      AStartOffset := AMatch.Start + AMatch.Len;
      match_data := pcre2_match_data_create_from_pattern_8(re, nil);
      offset_count := pcre2_match_8(re, PAnsiChar(InputString), InputLength,
        AStartOffset, 0, match_data, nil);
      pcre2_match_data_free_8(match_data);
      pcre2_code_free_8(re);
      if offset_count >= 0 then
      begin
        ovector := pcre2_get_ovector_pointer_8(match_data);
        AMatch := ovector.GetSubstringOffset(0);
        SetLength(Result, Length(Result) + 1);
        AResult := @Result[High(Result)];
        if offset_count = 0 then
        begin
          SetLength(AResult^, 1);
          AResult^[0] := ovector.GetSubstringOffset(0);
        end
        else
        begin
          SetLength(AResult^, offset_count - 1);
          for i := 1 to offset_count - 1 do
            AResult^[i - 1] := ovector.GetSubstringOffset(i);
        end;
      end
      else
        raise Exception.Create(GetErrorMessage(offset_count));
    end;
  end
  else
    raise Exception.Create(GetErrorMessage(offset_count, error_offset));
end;

function Substitute(const InputString, Expression, Replacement: String;
  ReplaceAll: Boolean = False): String;
var
  re: ppcre2_code_8;
  options: PCRE2_OPTIONS = PCRE2_SUBSTITUTE_OVERFLOW_LENGTH;
  error_code, error_offset, result_len: Integer;

  procedure dosubstitute;
  begin
    error_code := pcre2_substitute_8(re, PAnsiChar(InputString), Length(InputString),
      0, options, nil, nil, PAnsiChar(Replacement), length(Replacement),
      PAnsiChar(Result), @result_len);
    if error_code = PCRE2_ERROR_NOMEMORY then
    begin
      setlength(Result, result_len);
      dosubstitute;
    end
    else
      setlength(Result, result_len);
  end;

begin
  Result := InputString;
  re := pcre2_compile_8(PAnsiChar(Expression), Length(Expression), PCRE2_UTF,
    @error_code, @error_offset, nil);
  if re <> nil then
  begin
    if ReplaceAll then
      options := options + PCRE2_SUBSTITUTE_GLOBAL;
    setlength(Result, MAX_PATH);
    result_len := MAX_PATH;
    dosubstitute;
    pcre2_code_free_8(re);
    if error_code < 0 then
      raise Exception.Create(GetErrorMessage(error_code));
  end
  else
    raise Exception.Create(GetErrorMessage(error_code, error_offset));
end;

function GSub(const InputString, Expression, Replacement: String): String;
begin
  Result := Substitute(InputString, Expression, Replacement, True);
end;

function Version: String;
begin
  Result := '';
  setlength(Result, MAX_PATH);
  setlength(Result, pcre2_config_8(PCRE2_CONFIG_VERSION, PAnsiChar(Result)) - 1);
end;

{ TPCRE2SubsstringOffsetHelper }

function TPCRE2SubsstringOffsetHelper.GetString(const s: String): String;
begin
  Result := Copy(s, Self.Start, self.Len);
end;

function TPCRE2SubsstringOffsetHelper.IsNil: Boolean;
begin
  Result := (Self.Start = 0) and (Self.Len = 0);
end;

{ TPCRE2OVectorHelper }

function TPCRE2OVectorHelper.GetStart(const idx: Integer): Cardinal;
begin
  Result := (Self[idx * 2]) + 1;
end;

function TPCRE2OVectorHelper.GetLength(const idx: Integer): Cardinal;
begin
  Result := (Self[idx * 2 + 1]) - (Self[idx * 2]);
end;

function TPCRE2OVectorHelper.GetSubstringOffset(const idx: Integer): TPCRE2SubstringOffset;
begin
  Result.Start := self.GetStart(idx);
  Result.Len := self.GetLength(idx);
end;

end.
