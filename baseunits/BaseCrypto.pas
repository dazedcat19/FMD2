unit BaseCrypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64, DCPrijndael, DCPsha256, DCPmd5, Math;

function HexToStr(const h: String): String;
procedure HexToBytes(const h: String; var o :TBytes);
function BytesToHex(const b: TBytes): String;
function BytesToString(const b: TBytes): String;
function JSHexToStr(const h: String): String;
function StrToHexStr(const s: String): String;

function Pkcs7AddPad(const s: String): String;
function Pkcs7RemovePad(const s: String): String;
function AESEncrpytCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
function AESDecryptCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
function AESDecryptCBCMD5Base64ZerosPadding(const s, key, iv: String): String;
function MD5Hex(const s: String): String;
function AESDecryptCBC(const s, key, iv: String): String;

implementation

function HexToStr(const h: String): String;
var
  i: Integer;
begin
  SetLength(Result,Length(h) div 2);
  for i:=1 to Length(Result) do
    Result[i]:=Char(StrToInt('$'+Copy(h,(i*2)-1,2)));
end;

procedure HexToBytes(const h: String; var o :TBytes);
var
  i, l: Integer;
begin
  l:=Length(h) div 2;
  SetLength(o,l);
  for i:=Low(o) to High(o) do
    o[i]:=Byte(StrToInt('$'+Copy(h,(i*2)+1,2)));
end;

function BytesToHex(const b: TBytes): String;
var
  i: Integer;
begin
  Result:='';
  for i:=Low(b) to High(b) do
    Result+=IntToHex(b[i],2);
end;

function BytesToString(const b: TBytes): String;
var
  i: Integer;
begin
  Result:='';
  for i:=Low(b) to High(b) do
    Result+=Char(b[i]);
end;

function JSHexToStr(const h: String): String;
begin
  Result := HexToStr(StringReplace(h, '\x', '', [rfIgnoreCase, rfReplaceAll]));
end;

function StrToHexStr(const s: String): String;
begin
  SetLength(Result, Length(s) * 2);
  BinToHex(@s[1],@Result[1],Length(s));
end;

// Pkcs7 padding described in RFC 5652 https://tools.ietf.org/html/rfc5652#section-6.3
function Pkcs7AddPad(const s: String): String;
var
  l: Integer;
begin
  Result:=s;
  l:=16-(Length(s) and 15);
  if l>0 then
    result += StringOfChar(Char(l),l);
end;

function Pkcs7RemovePad(const s: String): String;
begin
  Result:=s;
  SetLength(Result,Length(Result)-Ord(Result[Length(Result)]));
end;

function AESEncrpytCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
var
  i: String;
  ivb: TBytes;
begin
  Result:='';
  with TDCP_rijndael.Create(nil) do
  begin
    try
      InitStr(key,TDCP_sha256);
      HexToBytes(iv,ivb);
      SetIV(ivb[0]);
      i:=Pkcs7AddPad(s);
      SetLength(Result,Length(i));
      EncryptCBC(i[1],Result[1],Length(i));
      Burn;
      Result:=EncodeStringBase64(Result);
    except
    end;
    Free;
  end;
end;

function AESDecryptCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
var
  data: String;
  ivb: TBytes;
begin
  Result:='';
  with TDCP_rijndael.Create(nil) do
  begin
    try
      InitStr(key,TDCP_sha256);
      HexToBytes(iv,ivb);
      SetIV(ivb[0]);
      data:=DecodeStringBase64(s);
      SetLength(Result,Length(data));
      DecryptCBC(data[1],Result[1],Length(data));
      Burn;
      Result:=Pkcs7RemovePad(Result);
    except
    end;
    Free;
  end;
end;

function AESDecryptCBC(const s, key, iv: String): String;
var
  ivBytes: array[0 .. 15] of Byte;
  keyBytes: TBytes;
  i: Integer;
begin
  Result := '';
  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key)-1 do
    keyBytes[i] := Byte(key[i + 1]);

  FillChar(ivBytes, 16, 0);
  for i := 0 to Min(16, Length(iv)) - 1 do
    ivBytes[i] := Byte(iv[i + 1]);

  with TDCP_rijndael.Create(nil) do
    try
      Init(keyBytes, Length(key) * 8, @ivBytes[0]);
      SetLength(Result,Length(s));
      DecryptCBC(s[1],Result[1],Length(s));
      Burn;
    finally
      Free;
    end;
end;

function AESDecryptCBCMD5Base64ZerosPadding(const s, key, iv: String): String;
begin
  Result := AESDecryptCBC(DecodeStringBase64(s), MD5Hex(key), iv);
end;

function MD5Hex(const s: String): String;
var
  h: TBytes;
begin
  with TDCP_md5.Create(nil) do
    try
      Init;
      UpdateStr(s);
      SetLength(h, 16);
      Final(h);
    finally
      Free;
    end;
  Result := LowerCase(StrToHexStr(BytesToString(h)));
end;

end.

