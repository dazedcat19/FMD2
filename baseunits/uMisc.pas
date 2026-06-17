unit uMisc;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  ShellApi, Windows,
  {$else}
  UTF8Process,
  {$endif}
  Classes, SysUtils, strutils;

type
  TArrayOfString = array of String;

//String utils
procedure VolumeChapterPadZero(var S: String; VolLength, ChapLength: Integer);
function padZeros(const S: String; VolLength, ChapLength: Integer): String; inline;
function BrackText(const S: String): String; overload; inline;
function BrackText(const S: Integer): String; overload; inline;
function BrackSquareText(const S: String): String; overload; inline;
function BrackSquareText(const S: Integer): String; overload; inline;
function BrackTextQuoted(const S: String): String; overload; inline;
function BrackTextQuoted(const S: Integer): String; overload; inline;
function StringToASCII(S: String): String;
function StringToHex(S: String): String;

//Searching
function FindStrLinear(aList: TStrings; aValue: String): Boolean;
function FindStrLinearPos(aList: TStrings; aValue: String): Integer;

//formatting
function FormatByteSize(const bytes: Longint; persecond: Boolean = False): String;

//run external process
function RunExternalProcessAsAdmin(Exe, Params: String; ShowWind: Boolean = True;
  isPersistent: Boolean = True): Boolean;
function RunExternalProcess(Exe: String; Params: array of String; ShowWind: Boolean = True;
  isPersistent: Boolean = True): Boolean; overload;
function RunExternalProcess(Exe, Params: String; ShowWind: Boolean = True;
  isPersistent: Boolean = True): Boolean; overload;
function RunExternalProcess(CommandLine: String; ShowWind: Boolean = True;
  isPersistent: Boolean = True): Boolean; overload;

//stringutils
procedure ParseCommandLine(const cmd: String; var Output: TStrings;
  AStripQuotes: Boolean = False);
function ParsedCommandLine(const cmd: String): TArrayOfString;
function StringsToArray(const S: TStrings): TArrayOfString;
function StringsToCommandLine(const S: TStrings): String; overload;
function StringsToCommandLine(const S: array of String): String; overload;
procedure DeleteArrayOfString(var TheStrings: TArrayOfString; Index: Integer);

const
  RANDOM_SLEEP = 3000;

implementation

{ uMisc }

function BrackText(const S: String): String;
begin
  Result := '(' + S + ')';
end;

function BrackText(const S: Integer): String;
begin
  Result := BrackText(IntToStr(S));
end;

function BrackSquareText(const S: String): String;
begin
  Result := '[' + S + ']';
end;

function BrackSquareText(const S: Integer): String;
begin
  Result := BrackText(IntToStr(S));
end;

function BrackTextQuoted(const S: String): String;
begin
  Result := BrackText(QuotedStr(S));
end;

function BrackTextQuoted(const S: Integer): String;
begin
  Result := BrackText(QuotedStr(IntToStr(S)));
end;

function StringToASCII(S: String): String;
var
  i: Integer;
begin
  Result := '#0';
  if Length(S) > 0 then
  begin
    Result := '';
    for i := 1 to Length(S) do
      Result := Result + '#' + IntToStr(Ord(S[i])) + ';';
  end;
end;

function StringToHex(S: String): String;
var
  i: Integer;
begin
  Result := '#0';
  if Length(S) > 0 then
  begin
    Result := '';
    for i := 1 to Length(S) do
      Result := Result + '#' + IntToHex(Ord(S[i]), 2) + ';';
  end;
end;

procedure VolumeChapterPadZero(var S: String; VolLength, ChapLength: Integer);

  procedure searchChap(var i, cstart, clength: Integer; var t: String);
  var
    j: Integer;
  begin
    for j := i to Length(t) do
    begin
      if (cstart = -1) and (t[j] in ['0'..'9']) then
        cstart := j
      else if (cstart > -1) and ((not (t[j] in ['0'..'9'])) or (j = Length(t))) then
      begin
        if j = Length(t) then
          clength := j - cstart + 2
        else
          clength := j - cstart;
        Break;
      end;
    end;
  end;

var
  i, cstart, vstart, clength, vlength, vp: Integer;
  t, c, v: String;
  vol: Boolean = False;
  cha: Boolean = False;
begin
  cstart := 0;
  vstart := 0;
  clength := 0;
  vlength := 0;
  vp := 0;
  c := '';
  v := '';

  t := S;
  i := 1;

  // Volume number
  if VolLength > 0 then
  begin
    vp := Pos('VOL', upcase(t));
    if (vp > 0) then
    begin
      if (vp > 2) and (not (t[vp - 1] in [',', '.', '-', '_', ' '])) then
        vol := False
      else if Pos('VOLUME NOT AVAILABLE', upcase(t)) > 0 then
        vol := False
      else
        vol := True;
    end;

    vstart := -1;
    vlength := 1;
    if vol then
    begin
      for i := Pos('VOL', upcase(t)) to Length(t) do
      begin
        if (vstart = -1) and (t[i] in ['0'..'9']) then
          vstart := i
        else if (vstart > -1) and ((not (t[i] in ['0'..'9'])) or (i = Length(t))) then
        begin
          if i = Length(t) then
            vlength := i - vstart + 1
          else
            vlength := i - vstart;
          Break;
        end;
      end;
      if vstart = -1 then
        vol := False;
    end;
    if vol then
    begin
      v := Copy(t, vstart, vlength);
      while Length(v) < VolLength do
      begin
        v := '0' + v;
      end;
    end;
  end;

  cstart := -1;
  clength := 1;
  // Chapter number
  if ChapLength > 0 then
  begin
    if i = Length(t) then
      i := 1;
    searchChap(i, cstart, clength, t);
    if (cstart = -1) and (not (i = 1)) then  //if not found at first try
    begin
      i := 1;
      searchChap(i, cstart, clength, t);
    end
    else
    if (cstart = -1) then
      cha := False
    else
      cha := True;

    if cha then
    begin
      c := Copy(t, cstart, clength);
      while Length(c) < ChapLength do
      begin
        c := '0' + c;
      end;
    end;
  end;

  if (cha) and (vol) and (vstart < cstart) then
  begin
    Delete(t, cstart, clength);
    Insert(c, t, cstart);
    Delete(t, vstart, vlength);
    Insert(v, t, vstart);
  end
  else
  if (cha) and (vol) and (vstart > cstart) then
  begin
    Delete(t, vstart, vlength);
    Insert(v, t, vstart);
    Delete(t, cstart, clength);
    Insert(c, t, cstart);
  end
  else
  if (vol) then
  begin
    Delete(t, vstart, vlength);
    Insert(v, t, vstart);
  end
  else
  if (cha) then
  begin
    Delete(t, cstart, clength);
    Insert(c, t, cstart);
  end;
  S := t;
end;

function padZeros(const S: String; VolLength, ChapLength: Integer): String;
begin
  Result := S;
  VolumeChapterPadZero(Result, VolLength, ChapLength);
end;

function FindStrLinearPos(aList: TStrings; aValue: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to aList.Count - 1 do
  begin
    if CompareText(aList.Strings[i], aValue) = 0 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function FindStrLinear(aList: TStrings; aValue: String): Boolean;
begin
  if FindStrLinearPos(aList, aValue) >= 0 then
    Result := True
  else
    Result := False;
end;

function FormatByteSize(const bytes: Longint; persecond: Boolean = False): String;
const
  B = 1;
  KB = 1024 * B;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if bytes < KB then
  begin
    Result := FormatFloat('#.##', bytes);

    if persecond then
    begin
      Result += ' B ps';
    end
    else
    begin
      Result += ' bytes';
    end;
  end
  else if bytes >= KB then
  begin
    Result := FormatFloat('#.## KB', bytes / KB);
  end
  else if bytes >= MB then
  begin
    Result := FormatFloat('#.## MB', bytes / MB);
  end
  else if bytes >= GB then
  begin
    Result := FormatFloat('#.## GB', bytes / GB);
  end;
end;


function RunExternalProcessAsAdmin(Exe, Params: String; ShowWind: Boolean;
  isPersistent: Boolean): Boolean;
var
 {$IFDEF WINDOWS}
  SEInfo: TSHELLEXECUTEINFOW;
 {$ELSE}
  Process: TProcessUTF8;
  pr: TStringList;
 {$ENDIF}
begin
  {$IFDEF WINDOWS}
  Initialize(SEInfo);
  FillChar(SEInfo, SizeOf(SEInfo), 0);
  SEInfo.cbSize := SizeOf(SEInfo);
  with SEInfo do begin
    wnd := 0;
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    if isPersistent then
      fMask := fMask or SEE_MASK_NOCLOSEPROCESS;
    lpVerb := 'runas';
    lpFile := PWideChar(UTF8Decode(Exe));
    lpParameters := PWideChar(UTF8Decode(Params));
    if ShowWind then
      nShow := SW_SHOWNORMAL
    else
      nShow := SW_HIDE;
  end;
  Result := ShellExecuteExW(@SEInfo);
  if isPersistent then
    WaitForSingleObject(SEInfo.hProcess, INFINITE);
  {$ELSE}
  Process := TProcessUTF8.Create(nil);
  try
    Process.Executable := Exe;
    pr := TStringList.Create;
    try
      ParseCommandLine(Params, TStrings(pr), True);
      Process.Parameters.Assign(pr);
    finally
      pr.Free;
    end;
    Process.Execute;
  finally
    Process.Free;
  end;
  {$ENDIF}
end;

{$ifdef windows}
function WinRunProcessA(Exe, Params: String; ShowWind: Boolean; isPersistent: Boolean): Boolean;
var
  SEInfo: TSHELLEXECUTEINFOA;
begin
  Initialize(SEInfo);
  FillChar(SEInfo, SizeOf(SEInfo), 0);
  SEInfo.cbSize := SizeOf(TSHELLEXECUTEINFOA);
  with SEInfo do begin
    wnd := 0;
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    if isPersistent then
      fMask := fMask or SEE_MASK_NOCLOSEPROCESS;
    lpFile := PChar(Utf8ToAnsi(Exe));
    lpParameters := PChar(Utf8ToAnsi(Params));
    if ShowWind then
      nShow := SW_SHOWNORMAL
    else
      nShow := SW_HIDE;
  end;
  Result := ShellExecuteExA(@SEInfo);
  if isPersistent then
    WaitForSingleObject(SEInfo.hProcess, INFINITE);
end;

function WinRunProcessW(Exe, Params: String; ShowWind: Boolean; isPersistent: Boolean): Boolean;
var
  SEInfo: TSHELLEXECUTEINFOW;
begin
  Initialize(SEInfo);
  FillChar(SEInfo, SizeOf(SEInfo), 0);
  SEInfo.cbSize := SizeOf(TSHELLEXECUTEINFOW);
  with SEInfo do begin
    wnd := 0;
    fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
    if isPersistent then
      fMask := fMask or SEE_MASK_NOCLOSEPROCESS;
    lpFile := PWideChar(UTF8Decode(Exe));
    lpParameters := PWideChar(UTF8Decode(Params));
    if ShowWind then
      nShow := SW_SHOWNORMAL
    else
      nShow := SW_HIDE;
  end;
  Result := ShellApi.ShellExecuteExW(@SEInfo);
  if isPersistent then
    WaitForSingleObject(SEInfo.hProcess, INFINITE);
end;

{$endif}

function RunExternalProcess(Exe: String; Params: array of String;
  ShowWind: Boolean; isPersistent: Boolean): Boolean;
{$ifndef windows}
var
  Process: TProcessUTF8;
  I: Integer;
{$endif}
begin
  if Trim(Exe) = '' then Exit(False);
  Result := True;
  {$ifdef windows}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := WinRunProcessW(Exe, StringsToCommandLine(Params), ShowWind, isPersistent)
  else
    Result := WinRunProcessA(Exe, StringsToCommandLine(Params), ShowWind, isPersistent);
  {$else}
  Process := TProcessUTF8.Create(nil);
  try
    Process.InheritHandles := isPersistent;
    Process.Executable := Exe;
    Process.Parameters.AddStrings(Params);
    if isPersistent then
      Process.Options := Process.Options + [poWaitOnExit]
    else
      Process.Options := [];
    if ShowWind then
      Process.ShowWindow := swoShow
    else
      Process.ShowWindow := swoHIDE;
    // Copy default environment variables including DISPLAY variable for GUI application to work
    for I := 0 to GetEnvironmentVariableCount - 1 do
      Process.Environment.Add(GetEnvironmentString(I));
    Process.Execute;
  except
    on E: Exception do
    begin
      WriteLog_E('RunExternalProcess.Error '#13#10 +
        'Executable: ' + Exe + #13#10 +
        'Parameters: ' + StringsToCommandLine(Params) + #13#10 +
        'Message   : ' + E.Message + #13#10 +
        GetStackTraceInfo);
    end;
  end;
  Process.Free;
 {$endif}
end;

function RunExternalProcess(Exe, Params: String; ShowWind: Boolean;
  isPersistent: Boolean): Boolean;
begin
  if Trim(Exe) = '' then Exit(False);
  {$ifdef windows}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := WinRunProcessW(Exe, Params, ShowWind, isPersistent)
  else
    Result := WinRunProcessA(Exe, Params, ShowWind, isPersistent);
  {$else}
  Result := RunExternalProcess(Exe, ParsedCommandLine(Params), ShowWind, isPersistent);
  {$endif}
end;

function RunExternalProcess(CommandLine: String; ShowWind: Boolean;
  isPersistent: Boolean): Boolean;
var
  s: String;
  sa: TArrayOfString;
begin
  if Trim(CommandLine) = '' then Exit(False);
  try
    sa := ParsedCommandLine(CommandLine);
    s := sa[Low(sa)];
    DeleteArrayOfString(sa, Low(sa));
    {$ifdef windows}
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      Result := WinRunProcessW(s, StringsToCommandLine(sa), ShowWind, isPersistent)
    else
      Result := WinRunProcessA(s, StringsToCommandLine(sa), ShowWind, isPersistent);
    {$else}
    Result := RunExternalProcess(s, sa, ShowWind, isPersistent);
    {$endif}
  finally
    SetLength(sa, 0);
  end;
end;

procedure ParseCommandLine(const cmd: String; var Output: TStrings;
  AStripQuotes: Boolean = False);
var
  s, cl: String;
  cq: Integer;
  acl, lq: Boolean;

  procedure Addcl;
  begin
    if cl <> '' then
    begin
      if AStripQuotes and (Length(cl) > 1) then
      begin
        if cl[1] = '"' then
          Delete(cl, 1, 1);
        if cl[Length(cl)] = '"' then
          Delete(cl, Length(cl), 1);
      end;
      Output.Add(cl);
      cl := '';
      acl := False;
    end;
  end;

begin
  if not Assigned(Output) then Exit;
  Output.Clear;
  Output.BeginUpdate;
  try
    s := cmd;
    cl := '';
    cq := 0;
    lq := False;
    while s <> '' do
    begin
      acl := True;
      if s[1] = '"' then
      begin
        Inc(cq);
        lq := True;
      end
      else
      begin
        if s[1] = ' ' then
        begin
          if cq > 0 then
          begin
            if (not odd(cq)) and lq then
            begin
              cq := 0;
              Addcl;
            end;
          end
          else
            Addcl;
        end;
        lq := False;
      end;
      if acl then
        cl := cl + s[1];
      Delete(s, 1, 1);
    end;
    Addcl;
  finally
    Output.EndUpdate;
  end;
end;

function ParsedCommandLine(const cmd: String): TArrayOfString;
var
  ts: TStrings;
begin
  if cmd = '' then Exit;
  ts := TStringList.Create;
  try
    ParseCommandLine(cmd, ts, True);
    Result := StringsToArray(ts);
  finally
    ts.Free;
  end;
end;

function StringsToArray(const S: TStrings): TArrayOfString;
var
  i: Integer;
begin
  SetLength(Result, 0);
  if not Assigned(S) then Exit;
  if S.Count = 0 then Exit;
  SetLength(Result, S.Count);
  for i := 0 to S.Count - 1 do
    Result[i] := S[i];
end;

function StringsToCommandLine(const S: TStrings): String;
var
  i: Integer;
begin
  Result := '';
  if S.Count > 0 then
  begin
    for i := 0 to S.Count - 1 do
    begin
      if Pos(' ', S[i]) <> 0 then
        Result := Result + '"' + S[i] + '" '
      else
        Result := Result + S[i] + ' ';
    end;
    Result := Trim(Result);
  end;
end;

function StringsToCommandLine(const S: array of String): String;
var
  i: Integer;
begin
  Result := '';
  if Length(S) > 0 then
  begin
    for i := Low(S) to High(S) do
    begin
      if (Pos(' ', S[i]) <> 0) and
        ((LeftStr(S[i], 1) <> '"') and (RightStr(S[i], 1) <> '"')) then
        Result := Result + '"' + S[i] + '" '
      else
        Result := Result + S[i] + ' ';
    end;
    Result := Trim(Result);
  end;
end;

procedure DeleteArrayOfString(var TheStrings: TArrayOfString; Index: Integer);
var
  i: Integer;
begin
  if (Index < Low(TheStrings)) and (Index > High(TheStrings)) then Exit;
  if Length(TheStrings) > 0 then
  begin
    if Index < High(TheStrings) then
    begin
      for i := Index to High(TheStrings) - 1 do
        TheStrings[i] := TheStrings[i + 1];
    end;
    SetLength(TheStrings, Length(TheStrings) - 1);
  end;
end;

end.
