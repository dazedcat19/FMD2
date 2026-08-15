{
Copyright (C) 2015 Antônio Galvão/Rik van Kekem

This is the file COPYING.modifiedLGPL. All files contain headers showing the
appropriate license. See there if this modification can be applied.

These files are distributed under the Library GNU General Public License
(see the file COPYING.LGPL) with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but
you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.


If you didn't receive a copy of the file COPYING.LGPL, contact:
      Free Software Foundation, Inc.,
      675 Mass Ave
      Cambridge, MA  02139
      USA
}

unit NaturalSortUnit;
{NaturalSort sorts string lists in a natural order.}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF LINUX}
  , UnixType
  {$ENDIF}
  ;

type
  TSortType = (stNatural, stFloatThousand);

procedure NaturalSort(aList: TStrings; SortType: TSortType);
function UTF8FloatThousandCompareList(aList: TStringList;  Index1, Index2: Integer): Integer;
function UTF8NaturalCompareList(aList: TStringList;  Index1, Index2: Integer): Integer;
function UTF8FloatThousandCompareText(const S1, S2: string): Integer;
function UTF8LogicalCompareText(const S1, S2: string): Integer;
function UTF8NaturalCompareText(const S1, S2: string): Integer;
{$IFNDEF WINDOWS}
function strcoll(s1, s2: pchar):integer; cdecl; external 'libc';
function wcscoll(s1, s2: pwchar_t): integer; cdecl; external 'libc' Name 'wcscoll';
{$ELSE}
function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll';

const
  //LINGUISTIC_IGNORECASE  = $00000010;
  NORM_LINGUISTIC_CASING = $08000000;
{$ENDIF}

var
  L: TStringList;
{$IFDEF WINDOWS}
  AtLeastXP :boolean = False;
{$ENDIF}

implementation

{$DEFINE NATURAL_SORT}
{
Natural Sort:
01
001
0001
Logical sort:
0001
001
01
}

{$IFNDEF WINDOWS}
function IsASCII(const s: string): boolean; inline;
var
  i: Integer;
begin
  for i:=1 to length(s) do if ord(s[i])>127 then exit(false);
  Result:=true;
end;
{$ENDIF}

function UTF8NaturalCompareBase(const Str1, Str2: string; Human: boolean; SortType :TSortType; const ADecSeparator, AThousandSeparator: Char): Integer;
{
 UTF8NaturalCompareBase compares UTF-8 strings in a collated order and
 so numbers are sorted too. If Human is set, it sorts like this:

 01
 001
 0001

 and

 0
 00
 000
 000_A
 000_B

 in a human, intuitive order.
 }
{
const
  NORM_LINGUISTIC_CASING = $08000000;
}
var
  Num1, Num2: double;
  pStr1, pStr2: PChar;
  Len1, Len2: integer;
  TextLen1, TextLen2: integer;
  TextStr1: string = '';
  TextStr2: string = '';
  i: integer;
  j: integer;
  {$IFNDEF WINDOWS}
  ConvertedString1: UCS4string = nil;
  ConvertedString2: UCS4string = nil;
  {$ENDIF}

  function IsNumber(ch: char): boolean;
  begin
    Result := ch in ['0'..'9'];
    //Result := ((ch >= '0') and (ch <= '9'));
  end;

  function GetFloatThousand(var pch: PChar; var Len: integer): double;

    function IsThousand(pchr: PChar): boolean;
    begin
      Result := False;
      if IsNumber((pchr + 1)^) and IsNumber((pchr + 2)^) and IsNumber((pchr + 3)^)
      and (IsNumber((pchr - 1)^)) then
        Result := True;
    end;

  var
    FoundDecSeparator: boolean;
    Count: integer = 0;
  begin
    FoundDecSeparator := False;
    Result := 0;
    while (pch^ <> #0) and ( IsNumber(pch^) or
    ((not FoundDecSeparator) and((pch^= ADecSeparator)
      or ( (pch^ = AThousandSeparator) and IsThousand(pch) )
     )))
    do
    begin
      if (pch^ = ADecSeparator)
      then
      begin
        FoundDecSeparator := True;
        Count := 0;
      end
      else
      begin
        if FoundDecSeparator then
        begin
          Inc(Count);
          Result := Result + (Ord(pch^) - Ord('0')) * Power(10, - Count);
        end
        else
          if isnumber(pch^) then
            Result := Result * 10 + Ord(pch^) - Ord('0');
      end;
      Inc(Len);
      Inc(pch);
    end;
  end;

  function GetInteger(var pch: PChar; var Len: integer): double;
  begin
    Result := 0;
    while (pch^ <> #0) and IsNumber(pch^) do
    begin
      Result := Result * 10 + Ord(pch^) - Ord('0');
      Inc(Len);
      Inc(pch);
    end;

  end;

  procedure GetChars;
  begin
    TextLen1 := 0;
    while not ((pStr1 + TextLen1)^ in ['0'..'9']) and ((pStr1 + TextLen1)^ <> #0) do
      Inc(TextLen1);
    SetLength(TextStr1, TextLen1);
    i := 1;
    j := 0;
    while i <= TextLen1 do
    begin
      TextStr1[i] := (pStr1 + j)^;
      Inc(i);
      Inc(j);
    end;

    TextLen2 := 0;
    while not ((pStr2 + TextLen2)^ in ['0'..'9']) and ((pStr2 + TextLen2)^ <> #0) do
      Inc(TextLen2);
    SetLength(TextStr2, TextLen2);
    i := 1;
    j := 0;
    while i <= TextLen2 do
    begin
      TextStr2[i] := (pStr2 + j)^;
      Inc(i);
      Inc(j);
    end;
  end;

begin
  if (Str1 <> '') and (Str2 <> '') then
  begin
    pStr1 := PChar(Str1);
    pStr2 := PChar(Str2);
    Result := 0;
    while not ((pStr1^ = #0) or (pStr2^ = #0)) do
    begin
      TextLen1 := 1;
      TextLen2 := 1;
      Len1 := 0;
      Len2 := 0;
      while (pStr1^ = ' ') do
      begin
        Inc(pStr1);
        Inc(Len1);
      end;
      while (pStr2^ = ' ') do
      begin
        Inc(pStr2);
        Inc(Len2);
      end;
      if IsNumber(pStr1^) and IsNumber(pStr2^) then
      begin
        if SortType = stNatural then
        begin
          Num1 := GetInteger(pStr1, Len1);
          Num2 := GetInteger(pStr2, Len2);
        end
        else
        begin
          Num1 := GetFloatThousand(pStr1, Len1);
          Num2 := GetFloatThousand(pStr2, Len2);
        end;
        if Num1 < Num2 then
          Result := -1
        else if Num1 > Num2 then
          Result := 1
        else
        begin
          Result := Sign(Len1 - Len2);
          if not Human then
            Result := -Result;
        end;
        Dec(pStr1);
        Dec(pStr2);
      end
      else
      begin
        GetChars;
        if TextStr1 <> TextStr2 then
        begin
          {$IFDEF WINDOWS}
          Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_LINGUISTIC_CASING,
            pWideChar(UTF8Decode(TextStr1)), Length(TextStr1),
            pWideChar(UTF8Decode(TextStr2)), Length(TextStr2)) - 2;
          {$ELSE}
          if IsAscii(TextStr1) and IsAscii(TextStr2) then
            Result := strcoll(pchar(textstr1), pchar(textstr2))
          else
          begin
            ConvertedString1 := UnicodeStringToUCS4String(UTF8Decode(TextStr1));
            ConvertedString2 := UnicodeStringToUCS4String(UTF8Decode(TextStr2));
            Result := wcscoll(pWchar_t(ConvertedString1), pWChar_t(ConvertedString2));
          end;
          {$ENDIF}
        end
        else
          Result := 0;
      end;
      if Result <> 0 then
        Break;
      Inc(pStr1, TextLen1);
      Inc(pStr2, TextLen2);
    end;
  end;
  Num1 := Length(Str1);
  Num2 := Length(Str2);
  if (Result = 0) and (Num1 <> Num2) then
  begin
    if Num1 < Num2 then
      Result := -1
    else
      Result := 1;
  end;
end;

function UTF8LogicalCompareText(const S1, S2: string): Integer;
begin
  {$IFDEF WINDOWS}
    if AtLeastXP then
      Result := StrCmpLogicalW(PWideChar(UTF8Decode(S1)), PWideChar(UTF8Decode(S2)))
    else
      Result := UTF8NaturalCompareBase(S1, S2, False, stNatural, DefaultFormatSettings.DecimalSeparator, DefaultFormatSettings.ThousandSeparator);
  {$ELSE}
    Result := UTF8NaturalCompareBase(S1, S2, False, stNatural, DefaultFormatSettings.DecimalSeparator, DefaultFormatSettings.ThousandSeparator);
  {$ENDIF}
end;

function UTF8NaturalCompareText(const S1, S2: string): Integer;
begin
  Result := UTF8NaturalCompareBase(S1, S2, True, stNatural,
                                   DefaultFormatSettings.DecimalSeparator,
                                   DefaultFormatSettings.ThousandSeparator);
end;

function UTF8FloatThousandCompareText(const S1, S2: string): Integer;
begin
  Result := UTF8NaturalCompareBase(S1, S2, True, stFloatThousand,
                                   DefaultFormatSettings.DecimalSeparator,
                                   DefaultFormatSettings.ThousandSeparator);
end;

function UTF8FloatThousandCompareList(aList: TStringList;  Index1, Index2: Integer): Integer;
begin
  Result := UTF8FloatThousandCompareText(aList[Index1], aList[Index2]);
end;

function UTF8NaturalCompareList(aList: TStringList;  Index1, Index2: Integer): Integer;
{
    StrCmpLogicalW sorts zeroes like this:
    000
    000_A
    000_B
    00
    0

    and

    0001
    001
    01

    Our function sorts like this:

    01
    001
    0001

    and

    0
    00
    000
    000_A
    000_B

    which, in our opinion, is better than the Windows one.
    You can do your choice.

    }
begin
  {$IFDEF NATURAL_SORT}
    {Uncomment NATURAL_SORT define just at the beggining of implementation section
    to use the human, intuitive sort way. Otherwise use Windows API function.}
    Result := UTF8NaturalCompareText(aList[Index1], aList[Index2]);
  {$ELSE}
    Result := UTF8LogicalCompareText(aList[Index1], aList[Index2]);
  {$ENDIF}
end;

procedure NaturalSort(aList: TStrings; SortType: TSortType);
begin
  L.Assign(aList);
  if SortType = stNatural then
    L.CustomSort(@UTF8NaturalCompareList)
  else if SortType = stFloatThousand then
    L.CustomSort(@UTF8FloatThousandCompareList);
  aList.Assign(L);
end;

{$IFDEF WINDOWS}
function IsAtLeastXP: boolean;
begin
  case SysUtils.Win32MajorVersion of
  0..4: Result := False;
  5: if (SysUtils.Win32MinorVersion >= 1) then Result := True
     else Result := False;
  6..20: Result := True;
  end;
end;
{$ENDIF}

initialization
  L := TStringList.Create;
  {$IFDEF WINDOWS}
  AtLeastXP := IsAtLeastXP;
  {$ENDIF}
finalization
  L.Free;

end.

