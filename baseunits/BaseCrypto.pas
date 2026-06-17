unit BaseCrypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64, DCPrijndael, DCPrc4, DCPsha256, DCPsha512, DCPmd5, DCPsha1, Math;

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
function AESEncryptCBC(const s, key, iv: String): String;
function AESDecryptCBC(const s, key, iv: String): String;
function AESEncryptECBPkcs7(const s, key: String): String;
function AESDecryptECBPkcs7(const s, key: String): String;
function AESCTR(const s, key, iv: String): String;
function AESCFB(const s, key, iv: String): String;
function AESOFB(const s, key, iv: String): String;
function MD5Hex(const s: String): String;
function RC4(const s, key: String): String;
function PBKDF2SHA256(const password, salt: String; iterations, dkLen: Integer): String;
function Base64URLEncode(const s: String): String;
function Base64URLDecode(const s: String): String;
function AESEncryptGCM(const s, key, iv, aad: String): String;
function AESDecryptGCM(const s, key, iv, aad: String): String;

function SHA1Hex(const s: String): String;
function HMAC_SHA1Hex(const s, key: String): String;
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

function SHA1Hex(const s: String): String;
var
  digest: array[0..19] of Byte;
begin
  Result := '';
  if s = '' then Exit;
  with TDCP_sha1.Create(nil) do
    try
      Init;
      Update(s[1], Length(s));
      Final(digest);
    finally
      Free;
    end;
  Result := LowerCase(StrToHexStr(BytesToString(digest)));
end;

function HMAC_SHA1Hex(const s, key: String): String;
const
  BLOCK_SIZE = 64;
var
  k: String;
  ipad, opad: array[0..BLOCK_SIZE - 1] of Byte;
  inner, outer: array[0..19] of Byte;
  i: Integer;
  innerStr: String;
begin
  Result := '';
  if (s = '') or (key = '') then Exit;

  if Length(key) > BLOCK_SIZE then
    k := SHA1Hex(key)
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

  with TDCP_sha1.Create(nil) do
    try
      Init;
      Update(ipad[0], BLOCK_SIZE);
      Update(s[1], Length(s));
      Final(inner);
    finally
      Free;
    end;

  with TDCP_sha1.Create(nil) do
    try
      Init;
      Update(opad[0], BLOCK_SIZE);
      Update(inner[0], 20);
      Final(outer);
    finally
      Free;
    end;

  SetString(innerStr, PChar(@outer[0]), 20);
  Result := LowerCase(StrToHexStr(innerStr));
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

function AESEncryptECBPkcs7(const s, key: String): String;
var
  keyBytes: TBytes;
  i: Integer;
  padded: String;
begin
  Result := '';
  if (s = '') or (key = '') then Exit;

  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  padded := Pkcs7AddPad(s);
  with TDCP_rijndael.Create(nil) do
    try
      try
        Init(keyBytes[0], Length(keyBytes) * 8, nil);
        SetLength(Result, Length(padded));
        EncryptECB(padded[1], Result[1]);
        Burn;
      except
        Result := '';
      end;
    finally
      Free;
    end;
end;

function AESDecryptECBPkcs7(const s, key: String): String;
var
  keyBytes: TBytes;
  i: Integer;
begin
  Result := '';
  if (s = '') or (key = '') then Exit;

  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  with TDCP_rijndael.Create(nil) do
    try
      try
        Init(keyBytes[0], Length(keyBytes) * 8, nil);
        SetLength(Result, Length(s));
        DecryptECB(s[1], Result[1]);
        Burn;
        Result := Pkcs7RemovePad(Result);
      except
        Result := '';
      end;
    finally
      Free;
    end;
end;

function AESCTR(const s, key, iv: String): String;
var
  keyBytes: TBytes;
  Counter: array[0..15] of Byte;
  Keystream: array[0..15] of Byte;
  i, j, n: Integer;
  ptrS, ptrR: PChar;
begin
  Result := '';
  if (s = '') or (key = '') or (iv = '') then Exit;

  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  FillChar(Counter, SizeOf(Counter), 0);
  for i := 0 to Min(15, Length(iv) - 1) do
    Counter[i] := Byte(iv[i + 1]);

  SetLength(Result, Length(s));
  ptrS := @s[1];
  ptrR := @Result[1];

  with TDCP_rijndael.Create(nil) do
    try
      try
        Init(keyBytes[0], Length(keyBytes) * 8, nil);
        i := 0;
        while i < Length(s) do
        begin
          EncryptECB(Counter, Keystream);
          n := Min(16, Length(s) - i);

          for j := 0 to n - 1 do
            ptrR[i + j] := Char(Byte(ptrS[i + j]) xor Keystream[j]);

          for j := 15 downto 0 do
          begin
            if Counter[j] = 255 then
              Counter[j] := 0
            else
            begin
              Inc(Counter[j]);
              Break;
            end;
          end;

          Inc(i, 16);
        end;
        Burn;
      except
        Result := '';
      end;
    finally
      Free;
    end;
end;

function AESCFB(const s, key, iv: String): String;
var
  keyBytes: TBytes;
  ivBytes: array[0..15] of Byte;
  Feedback: array[0..15] of Byte;
  Keystream: array[0..15] of Byte;
  i, j, n: Integer;
begin
  Result := '';
  if (s = '') or (key = '') or (iv = '') then Exit;

  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  FillChar(ivBytes, SizeOf(ivBytes), 0);
  for i := 0 to Min(15, Length(iv) - 1) do
    ivBytes[i] := Byte(iv[i + 1]);

  Move(ivBytes[0], Feedback[0], 16);
  SetLength(Result, Length(s));

  with TDCP_rijndael.Create(nil) do
    try
      try
        Init(keyBytes[0], Length(keyBytes) * 8, nil);
        i := 0;
        while i < Length(s) do
        begin
          EncryptECB(Feedback, Keystream);
          n := Min(16, Length(s) - i);
          for j := 0 to n - 1 do
          begin
            Result[i + j + 1] := Char(Byte(s[i + j + 1]) xor Keystream[j]);
            Feedback[j] := Byte(Result[i + j + 1]);
          end;
          Inc(i, n);
        end;
        Burn;
      except
        Result := '';
      end;
    finally
      Free;
    end;
end;

function AESOFB(const s, key, iv: String): String;
var
  keyBytes: TBytes;
  ivBytes: array[0..15] of Byte;
  Output: array[0..15] of Byte;
  i, j, n: Integer;
begin
  Result := '';
  if (s = '') or (key = '') or (iv = '') then Exit;

  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  FillChar(ivBytes, SizeOf(ivBytes), 0);
  for i := 0 to Min(15, Length(iv) - 1) do
    ivBytes[i] := Byte(iv[i + 1]);

  Move(ivBytes[0], Output[0], 16);
  SetLength(Result, Length(s));

  with TDCP_rijndael.Create(nil) do
    try
      try
        Init(keyBytes[0], Length(keyBytes) * 8, nil);
        i := 0;
        while i < Length(s) do
        begin
          EncryptECB(Output, Output);
          n := Min(16, Length(s) - i);
          for j := 0 to n - 1 do
            Result[i + j + 1] := Char(Byte(s[i + j + 1]) xor Output[j]);
          Inc(i, n);
        end;
        Burn;
      except
        Result := '';
      end;
    finally
      Free;
    end;
end;

function PBKDF2SHA256(const password, salt: String; iterations, dkLen: Integer): String;
var
  blockCount, i, j, k: Integer;
  T, U, PRK: String;
  hmacBuf: array[0..31] of Byte;
  saltBlock: String;
  blockNum: array[0..3] of Byte;
begin
  Result := '';
  if (password = '') or (salt = '') or (iterations < 1) or (dkLen < 1) then Exit;

  blockCount := (dkLen + 31) div 32;

  for i := 1 to blockCount do
  begin
    blockNum[0] := (i shr 24) and $FF;
    blockNum[1] := (i shr 16) and $FF;
    blockNum[2] := (i shr 8) and $FF;
    blockNum[3] := i and $FF;
    SetString(saltBlock, PChar(@blockNum[0]), 4);
    U := HMAC_SHA256(salt + saltBlock, password);
    T := U;

    for j := 2 to iterations do
    begin
      U := HMAC_SHA256(U, password);
      for k := 1 to 32 do
        T[k] := Char(Byte(T[k]) xor Byte(U[k]));
    end;

    Result := Result + T;
  end;

  SetLength(Result, dkLen);
end;

function Base64URLEncode(const s: String): String;
begin
  Result := EncodeStringBase64(s);
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

function Base64URLDecode(const s: String): String;
var
  padded: String;
  rem: Integer;
begin
  padded := StringReplace(s, '-', '+', [rfReplaceAll]);
  padded := StringReplace(padded, '_', '/', [rfReplaceAll]);
  rem := Length(padded) mod 4;
  if rem = 2 then padded := padded + '=='
  else if rem = 3 then padded := padded + '=';
  Result := DecodeStringBase64(padded);
end;

type
  TBlock128 = array[0..15] of Byte;

procedure XorBlock128(var Dest: TBlock128; const Src: TBlock128);
var
  i: Integer;
begin
  for i := 0 to 15 do
    Dest[i] := Dest[i] xor Src[i];
end;

procedure Inc32Block(var Block: TBlock128);
var
  i: Integer;
begin
  for i := 15 downto 12 do
  begin
    if Block[i] = 255 then
      Block[i] := 0
    else begin
      Inc(Block[i]);
      Break;
    end;
  end;
end;

procedure GFMult128(var X: TBlock128; const Y: TBlock128);
var
  Z, V: TBlock128;
  i, j, k: Integer;
  carry, nextCarry: Boolean;
begin
  FillChar(Z, SizeOf(Z), 0);
  V := X;
  for i := 0 to 15 do
  begin
    for j := 7 downto 0 do
    begin
      if (Y[i] and (1 shl j)) <> 0 then
        XorBlock128(Z, V);

      carry := False;
      for k := 0 to 15 do
      begin
        nextCarry := (V[k] and 1) <> 0;
        V[k] := V[k] shr 1;
        if carry then
          V[k] := V[k] or $80;
        carry := nextCarry;
      end;
      if carry then
        V[0] := V[0] xor $E1;
    end;
  end;
  X := Z;
end;

procedure ComputeGHASH(const H: TBlock128; const AAD, C: String; var OutTag: TBlock128);
var
  X, tmp: TBlock128;
  i, blocks, rem: Integer;
  lenA, lenC: Int64;
begin
  FillChar(X, SizeOf(X), 0);

  // Process AAD
  blocks := Length(AAD) div 16;
  for i := 0 to blocks - 1 do
  begin
    Move(AAD[i * 16 + 1], tmp, 16);
    XorBlock128(X, tmp);
    GFMult128(X, H);
  end;
  rem := Length(AAD) mod 16;
  if rem > 0 then
  begin
    FillChar(tmp, SizeOf(tmp), 0);
    Move(AAD[blocks * 16 + 1], tmp, rem);
    XorBlock128(X, tmp);
    GFMult128(X, H);
  end;

  // Process Ciphertext
  blocks := Length(C) div 16;
  for i := 0 to blocks - 1 do
  begin
    Move(C[i * 16 + 1], tmp, 16);
    XorBlock128(X, tmp);
    GFMult128(X, H);
  end;
  rem := Length(C) mod 16;
  if rem > 0 then
  begin
    FillChar(tmp, SizeOf(tmp), 0);
    Move(C[blocks * 16 + 1], tmp, rem);
    XorBlock128(X, tmp);
    GFMult128(X, H);
  end;

  // Length block (64-bit Big Endian)
  FillChar(tmp, SizeOf(tmp), 0);
  lenA := Length(AAD) * 8;
  lenC := Length(C) * 8;
  
  tmp[0] := (lenA shr 56) and $FF;
  tmp[1] := (lenA shr 48) and $FF;
  tmp[2] := (lenA shr 40) and $FF;
  tmp[3] := (lenA shr 32) and $FF;
  tmp[4] := (lenA shr 24) and $FF;
  tmp[5] := (lenA shr 16) and $FF;
  tmp[6] := (lenA shr 8) and $FF;
  tmp[7] := lenA and $FF;
  
  tmp[8] := (lenC shr 56) and $FF;
  tmp[9] := (lenC shr 48) and $FF;
  tmp[10] := (lenC shr 40) and $FF;
  tmp[11] := (lenC shr 32) and $FF;
  tmp[12] := (lenC shr 24) and $FF;
  tmp[13] := (lenC shr 16) and $FF;
  tmp[14] := (lenC shr 8) and $FF;
  tmp[15] := lenC and $FF;

  XorBlock128(X, tmp);
  GFMult128(X, H);

  OutTag := X;
end;

function AESEncryptGCM(const s, key, iv, aad: String): String;
var
  keyBytes: TBytes;
  Cipher: TDCP_rijndael;
  H, J0, CB, TagBlock, E_J0, tmp: TBlock128;
  i, j, blocks, rem: Integer;
  ZeroBlock: TBlock128;
  CText: String;
begin
  Result := '';
  if (s = '') or (key = '') or (iv = '') then Exit;

  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  FillChar(ZeroBlock, SizeOf(ZeroBlock), 0);

  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(keyBytes[0], Length(keyBytes) * 8, nil);
    Cipher.EncryptECB(ZeroBlock, H);

    if Length(iv) = 12 then
    begin
      FillChar(J0, SizeOf(J0), 0);
      Move(iv[1], J0[0], 12);
      J0[15] := 1;
    end
    else
      ComputeGHASH(H, '', iv, J0);

    CB := J0;
    Inc32Block(CB);

    SetLength(CText, Length(s));
    blocks := Length(s) div 16;
    for i := 0 to blocks - 1 do
    begin
      Cipher.EncryptECB(CB, tmp);
      for j := 0 to 15 do
        CText[i * 16 + j + 1] := Char(Byte(s[i * 16 + j + 1]) xor tmp[j]);
      Inc32Block(CB);
    end;
    
    rem := Length(s) mod 16;
    if rem > 0 then
    begin
      Cipher.EncryptECB(CB, tmp);
      for j := 0 to rem - 1 do
        CText[blocks * 16 + j + 1] := Char(Byte(s[blocks * 16 + j + 1]) xor tmp[j]);
    end;

    ComputeGHASH(H, aad, CText, TagBlock);
    Cipher.EncryptECB(J0, E_J0);
    XorBlock128(TagBlock, E_J0);

    Result := CText;
    SetLength(Result, Length(CText) + 16);
    Move(TagBlock[0], Result[Length(CText) + 1], 16);
  finally
    Cipher.Free;
  end;
end;

function AESDecryptGCM(const s, key, iv, aad: String): String;
var
  keyBytes: TBytes;
  Cipher: TDCP_rijndael;
  H, J0, CB, TagBlock, E_J0, tmp, ExpectedTag: TBlock128;
  i, j, blocks, rem: Integer;
  ZeroBlock: TBlock128;
  CText, PText: String;
  diff: Byte;
begin
  Result := '';
  if (Length(s) < 16) or (key = '') or (iv = '') then Exit;

  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  // Extract Ciphertext and 16-byte Authentication Tag
  CText := Copy(s, 1, Length(s) - 16);
  Move(s[Length(s) - 15], ExpectedTag[0], 16);

  FillChar(ZeroBlock, SizeOf(ZeroBlock), 0);

  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(keyBytes[0], Length(keyBytes) * 8, nil);
    Cipher.EncryptECB(ZeroBlock, H);

    if Length(iv) = 12 then
    begin
      FillChar(J0, SizeOf(J0), 0);
      Move(iv[1], J0[0], 12);
      J0[15] := 1;
    end
    else
      ComputeGHASH(H, '', iv, J0);

    // Authenticate Tag first
    ComputeGHASH(H, aad, CText, TagBlock);
    Cipher.EncryptECB(J0, E_J0);
    XorBlock128(TagBlock, E_J0);

    // Constant-time tag validation to prevent timing attacks
    diff := 0;
    for i := 0 to 15 do
      diff := diff or (TagBlock[i] xor ExpectedTag[i]);
    if diff <> 0 then Exit; // Auth failed, return empty string

    // Decrypt
    CB := J0;
    Inc32Block(CB);

    SetLength(PText, Length(CText));
    blocks := Length(CText) div 16;
    for i := 0 to blocks - 1 do
    begin
      Cipher.EncryptECB(CB, tmp);
      for j := 0 to 15 do
        PText[i * 16 + j + 1] := Char(Byte(CText[i * 16 + j + 1]) xor tmp[j]);
      Inc32Block(CB);
    end;
    
    rem := Length(CText) mod 16;
    if rem > 0 then
    begin
      Cipher.EncryptECB(CB, tmp);
      for j := 0 to rem - 1 do
        PText[blocks * 16 + j + 1] := Char(Byte(CText[blocks * 16 + j + 1]) xor tmp[j]);
    end;

    Result := PText;
  finally
    Cipher.Free;
  end;
end;

end.