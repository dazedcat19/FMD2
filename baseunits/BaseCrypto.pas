unit BaseCrypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64, DCPrijndael, DCPrc4, DCPsha256, DCPsha512, DCPmd5, Math;

function HexToStr(const h: String): String;
procedure HexToBytes(const h: String; var o :TBytes);
function BytesToHex(const b: TBytes): String;
function BytesToString(const b: TBytes): String;
function JSHexToStr(const h: String): String;
function StrToHexStr(const s: String): String;

function Pkcs7AddPad(const s: String): String;
function Pkcs7RemovePad(const s: String): String;
function AESEncryptCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
function AESDecryptCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
function AESDecryptCBCMD5Base64ZerosPadding(const s, key, iv: String): String;
function AESDecryptCBCHexBase64ZerosPadding(const s, key, iv: String): String;
function MD5Hex(const s: String): String;
function AESEncryptCBC(const s, key, iv: String): String;
function AESDecryptCBC(const s, key, iv: String): String;
function RC4(const s, key: String): String;

function SHA256(const s: String): String;
function SHA256Hex(const s: String): String;
function HMAC_SHA256(const s, key: String): String;
function HMAC_SHA256Hex(const s, key: String): String;
function SHA512(const s: String): String;
function SHA512Hex(const s: String): String;
function HMAC_SHA512(const s, key: String): String;
function HMAC_SHA512Hex(const s, key: String): String;

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

function AESEncryptCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
var
  i: String;
  ivb: TBytes;
begin
  Result := '';
  if (s = '') or (key = '') or (iv = '') then Exit;

  with TDCP_rijndael.Create(nil) do
    try
      try
        InitStr(key, TDCP_sha256);
        HexToBytes(iv, ivb);
        SetIV(ivb[0]);
        i := Pkcs7AddPad(s);
        SetLength(Result, Length(i));
        EncryptCBC(i[1], Result[1], Length(i));
        Burn;
        Result := EncodeStringBase64(Result);
      except
        Result := '';
      end;
    finally
      Free;
    end;
end;

function AESDecryptCBCSHA256Base64Pkcs7(const s, key, iv: String): string;
var
  data: String;
  ivb: TBytes;
begin
  Result := '';
  if (s = '') or (key = '') or (iv = '') then Exit;

  with TDCP_rijndael.Create(nil) do
    try
      try
        InitStr(key, TDCP_sha256);
        HexToBytes(iv, ivb);
        SetIV(ivb[0]);
        data := DecodeStringBase64(s);
        SetLength(Result, Length(data));
        DecryptCBC(data[1], Result[1], Length(data));
        Burn;
        Result := Pkcs7RemovePad(Result);
      except
        Result := '';
      end;
    finally
      Free;
    end;
end;

function AESEncryptCBC(const s, key, iv: String): String;
var
  ivBytes: array[0..15] of Byte;
  keyBytes: TBytes;
  i: Integer;
begin
  Result := '';
  if (s = '') or (key = '') or (iv = '') then Exit;

  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  FillChar(ivBytes, SizeOf(ivBytes), 0);
  for i := 0 to Min(16, Length(iv)) - 1 do
    ivBytes[i] := Byte(iv[i + 1]);

  with TDCP_rijndael.Create(nil) do
    try
      try
        Init(keyBytes[0], Length(key) * 8, @ivBytes[0]);
        SetLength(Result, Length(s));
        EncryptCBC(s[1], Result[1], Length(s));
        Burn;
      except
        Result := '';
      end;
    finally
      Free;
    end;
end;

function AESDecryptCBC(const s, key, iv: String): String;
var
  ivBytes: array[0..15] of Byte;
  keyBytes: TBytes;
  i: Integer;
begin
  Result := '';
  if (s = '') or (key = '') or (iv = '') then Exit;
  
  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  FillChar(ivBytes, SizeOf(ivBytes), 0);
  for i := 0 to Min(16, Length(iv)) - 1 do
    ivBytes[i] := Byte(iv[i + 1]);

  with TDCP_rijndael.Create(nil) do
    try
      try
        Init(keyBytes[0], Length(key) * 8, @ivBytes[0]);
        SetLength(Result, Length(s));
        DecryptCBC(s[1], Result[1], Length(s));
        Burn;
      except
        Result := '';
      end;
    finally
      Free;
    end;
end;

function AESDecryptCBCMD5Base64ZerosPadding(const s, key, iv: String): String;
begin
  if (s = '') or (key = '') or (iv = '') then Exit;

  Result := AESDecryptCBC(DecodeStringBase64(s), MD5Hex(key), iv);
end;

function AESDecryptCBCHexBase64ZerosPadding(const s, key, iv: String): String;
var
  keyBytes, ivBytes: TBytes;
  data: String;
begin
  Result := '';
  if (s = '') or (key = '') or (iv = '') then Exit;

  HexToBytes(key, keyBytes);
  HexToBytes(iv, ivBytes);
  
  with TDCP_rijndael.Create(nil) do
    try
      try
        Init(keyBytes[0], Length(keyBytes) * 8, @ivBytes[0]);
        data := DecodeStringBase64(s);
        SetLength(Result, Length(data));
        DecryptCBC(data[1], Result[1], Length(data));
        Burn;
        while (Length(Result) > 0) and (Result[Length(Result)] = #0) do
          SetLength(Result, Length(Result) - 1);
      except
        Result := '';
      end;
    finally
      Free;
    end;
end;

function MD5Hex(const s: String): String;
var
  h: array[0..15] of Byte;
begin
  with TDCP_md5.Create(nil) do
    try
      Init;
      Update(s[1], Length(s));
      Final(h);
    finally
      Free;
    end;
  Result := LowerCase(StrToHexStr(BytesToString(h)));
end;

function RC4(const s, key: String): String;
begin
  Result := '';
  if (s = '') or (key = '') then Exit;

  SetLength(Result, Length(s));
  with TDCP_rc4.Create(nil) do
  try
    Init(key[1], Length(key) * 8, nil);
    Encrypt(s[1], Result[1], Length(s));
    Burn;
  finally
    Free;
  end;
end;

function SHA256(const s: String): String;
var
  digest: array[0..31] of Byte;
begin
  with TDCP_sha256.Create(nil) do
    try
      Init;
      Update(s[1], Length(s));
      Final(digest);
    finally
      Free;
    end;
  SetString(Result, PChar(@digest[0]), 32);
end;

function SHA256Hex(const s: String): String;
begin
  Result := LowerCase(StrToHexStr(SHA256(s)));
end;

function HMAC_SHA256(const s, key: String): String;
const
  BLOCK_SIZE = 64;
var
  k: String;
  ipad, opad: array[0..BLOCK_SIZE - 1] of Byte;
  inner: array[0..31] of Byte;
  outer: array[0..31] of Byte;
  i: Integer;
begin
  if (s = '') or (key = '') then Exit;

  if Length(key) > BLOCK_SIZE then
    k := SHA256(key)
  else
    k := key;

  for i := 0 to BLOCK_SIZE - 1 do
  begin
    if i < Length(k) then
    begin
      ipad[i] := Byte(k[i + 1]) xor $36;
      opad[i] := Byte(k[i + 1]) xor $5C;
    end
    else
    begin
      ipad[i] := $36;
      opad[i] := $5C;
    end;
  end;

  with TDCP_sha256.Create(nil) do
    try
      Init;
      Update(ipad[0], BLOCK_SIZE);
      Update(s[1], Length(s));
      Final(inner);
    finally
      Free;
    end;

  with TDCP_sha256.Create(nil) do
    try
      Init;
      Update(opad[0], BLOCK_SIZE);
      Update(inner[0], 32);
      Final(outer);
    finally
      Free;
    end;

  SetString(Result, PChar(@outer[0]), 32);
end;

function HMAC_SHA256Hex(const s, key: String): String;
begin
  if (s = '') or (key = '') then Exit;

  Result := LowerCase(StrToHexStr(HMAC_SHA256(s, key)));
end;

function SHA512(const s: String): String;
var
  digest: array[0..63] of Byte;
begin
  with TDCP_sha512.Create(nil) do
    try
      Init;
      Update(s[1], Length(s));
      Final(digest);
    finally
      Free;
    end;
  SetString(Result, PChar(@digest[0]), 64);
end;

function SHA512Hex(const s: String): String;
begin
  Result := LowerCase(StrToHexStr(SHA512(s)));
end;

function HMAC_SHA512(const s, key: String): String;
const
  BLOCK_SIZE = 128;
var
  k: String;
  ipad, opad: array[0..BLOCK_SIZE - 1] of Byte;
  inner: array[0..63] of Byte;
  outer: array[0..63] of Byte;
  i: Integer;
begin
  if (s = '') or (key = '') then Exit;

  if Length(key) > BLOCK_SIZE then
    k := SHA512(key)
  else
    k := key;

  for i := 0 to BLOCK_SIZE - 1 do
  begin
    if i < Length(k) then
    begin
      ipad[i] := Byte(k[i + 1]) xor $36;
      opad[i] := Byte(k[i + 1]) xor $5C;
    end
    else
    begin
      ipad[i] := $36;
      opad[i] := $5C;
    end;
  end;

  with TDCP_sha512.Create(nil) do
    try
      Init;
      Update(ipad[0], BLOCK_SIZE);
      Update(s[1], Length(s));
      Final(inner);
    finally
      Free;
    end;

  with TDCP_sha512.Create(nil) do
    try
      Init;
      Update(opad[0], BLOCK_SIZE);
      Update(inner[0], 64);
      Final(outer);
    finally
      Free;
    end;

  SetString(Result, PChar(@outer[0]), 64);
end;

function HMAC_SHA512Hex(const s, key: String): String;
begin
  if (s = '') or (key = '') then Exit;

  Result := LowerCase(StrToHexStr(HMAC_SHA512(s, key)));
end;

end.