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
function EncodeBase64URL(const s: String): String;
function DecodeBase64URL(const s: String): String;
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

function X25519_PublicKey(const privKey: String): String;
function X25519_SharedSecret(const privKey, pubKey: String): String;
function SecretStream_InitPull(const header, key: String; var state: String): Boolean;
function SecretStream_Pull(var state: String; const chunk: String; var msg: String; var tag: Byte): Boolean;

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

function EncodeBase64URL(const s: String): String;
begin
  Result := EncodeStringBase64(s);
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
end;

function DecodeBase64URL(const s: String): String;
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

procedure GHASH(const H: TBlock128; const AAD, C: String; var OutTag: TBlock128);
var
  X, tmp: TBlock128;
  i, blocks, rem: Integer;
  lenA, lenC: Int64;
begin
  FillChar(X, SizeOf(X), 0);

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
      GHASH(H, '', iv, J0);

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

    GHASH(H, aad, CText, TagBlock);
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
      GHASH(H, '', iv, J0);

    GHASH(H, aad, CText, TagBlock);
    Cipher.EncryptECB(J0, E_J0);
    XorBlock128(TagBlock, E_J0);

    diff := 0;
    for i := 0 to 15 do
      diff := diff or (TagBlock[i] xor ExpectedTag[i]);
    if diff <> 0 then Exit;

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

const
  EVP_PKEY_X25519 = 1034;

  {$IFDEF MSWINDOWS}
    {$IFDEF WIN64}
    LIBCRYPTO_DLL = 'libcrypto-3-x64.dll';
    {$ELSE}
    LIBCRYPTO_DLL = 'libcrypto-3.dll';
    {$ENDIF}
  {$ELSE}
    LIBCRYPTO_DLL = 'libcrypto.so.3';
  {$ENDIF}

function EVP_PKEY_new_raw_private_key(pkey_type: Integer; e: Pointer;
  key: PByte; keylen: SizeUInt): Pointer; cdecl; external LIBCRYPTO_DLL;
function EVP_PKEY_new_raw_public_key(pkey_type: Integer; e: Pointer;
  key: PByte; keylen: SizeUInt): Pointer; cdecl; external LIBCRYPTO_DLL;
function EVP_PKEY_get_raw_public_key(pkey: Pointer; pub: PByte;
  var len: SizeUInt): Integer; cdecl; external LIBCRYPTO_DLL;
procedure EVP_PKEY_free(pkey: Pointer); cdecl; external LIBCRYPTO_DLL;
function EVP_PKEY_CTX_new(pkey: Pointer; e: Pointer): Pointer; cdecl; external LIBCRYPTO_DLL;
procedure EVP_PKEY_CTX_free(ctx: Pointer); cdecl; external LIBCRYPTO_DLL;
function EVP_PKEY_derive_init(ctx: Pointer): Integer; cdecl; external LIBCRYPTO_DLL;
function EVP_PKEY_derive_set_peer(ctx: Pointer; peer: Pointer): Integer; cdecl; external LIBCRYPTO_DLL;
function EVP_PKEY_derive(ctx: Pointer; secret: PByte;
  var secretlen: SizeUInt): Integer; cdecl; external LIBCRYPTO_DLL;

function LoadLE32(const b: array of Byte; ofs: Integer): Cardinal;
begin
  Result := b[ofs] or (b[ofs + 1] shl 8) or (b[ofs + 2] shl 16) or
    (Cardinal(b[ofs + 3]) shl 24);
end;

function LoadLE32P(p: PByte): Cardinal; inline;
begin
  Result := p[0] or (p[1] shl 8) or (p[2] shl 16) or (Cardinal(p[3]) shl 24);
end;

procedure StoreLE32(var b: array of Byte; ofs: Integer; v: Cardinal);
begin
  b[ofs] := v and $FF;
  b[ofs + 1] := (v shr 8) and $FF;
  b[ofs + 2] := (v shr 16) and $FF;
  b[ofs + 3] := (v shr 24) and $FF;
end;

function Rol32(v: Cardinal; n: Integer): Cardinal; inline;
begin
  Result := (v shl n) or (v shr (32 - n));
end;

procedure ChaChaQR(var a, b, c, d: Cardinal); inline;
begin
  a := a + b; d := d xor a; d := Rol32(d, 16);
  c := c + d; b := b xor c; b := Rol32(b, 12);
  a := a + b; d := d xor a; d := Rol32(d, 8);
  c := c + d; b := b xor c; b := Rol32(b, 7);
end;

type
  TBytes16 = array[0..15] of Byte;
  TBytes12 = array[0..11] of Byte;
  TBytes32 = array[0..31] of Byte;
  TBytes64 = array[0..63] of Byte;

  TPoly1305State = record
    r: array[0..4] of Cardinal;
    h: array[0..4] of Cardinal;
    pad: array[0..3] of Cardinal;
    buf: array[0..15] of Byte;
    leftover: Integer;
  end;

  TSecretStreamState = record
    k: TBytes32;
    nonce: TBytes12;
    pad: array[0..19] of Byte;
  end;

procedure HChaCha20(var o: TBytes32; const k: TBytes32; const n: TBytes16);
var
  st, x: array[0..15] of Cardinal;
  i: Integer;
begin
  st[0] := $61707865; st[1] := $3320646E;
  st[2] := $79622D32; st[3] := $6B206574;
  for i := 0 to 7 do
    st[4 + i] := LoadLE32(k, i * 4);
  for i := 0 to 3 do
    st[12 + i] := LoadLE32(n, i * 4);
  Move(st[0], x[0], SizeOf(st));
  for i := 1 to 10 do
  begin
    ChaChaQR(x[0], x[4], x[8], x[12]);
    ChaChaQR(x[1], x[5], x[9], x[13]);
    ChaChaQR(x[2], x[6], x[10], x[14]);
    ChaChaQR(x[3], x[7], x[11], x[15]);
    ChaChaQR(x[0], x[5], x[10], x[15]);
    ChaChaQR(x[1], x[6], x[11], x[12]);
    ChaChaQR(x[2], x[7], x[8], x[13]);
    ChaChaQR(x[3], x[4], x[9], x[14]);
  end;
  for i := 0 to 3 do
  begin
    StoreLE32(o, i * 4, x[i]);
    StoreLE32(o, 16 + i * 4, x[12 + i]);
  end;
end;

procedure ChaCha20Block(var o: TBytes64; const k: TBytes32; counter: Cardinal;
  const n: TBytes12);
var
  st, x: array[0..15] of Cardinal;
  i: Integer;
begin
  st[0] := $61707865; st[1] := $3320646E;
  st[2] := $79622D32; st[3] := $6B206574;
  for i := 0 to 7 do
    st[4 + i] := LoadLE32(k, i * 4);
  st[12] := counter;
  st[13] := LoadLE32(n, 0);
  st[14] := LoadLE32(n, 4);
  st[15] := LoadLE32(n, 8);
  Move(st[0], x[0], SizeOf(st));
  for i := 1 to 10 do
  begin
    ChaChaQR(x[0], x[4], x[8], x[12]);
    ChaChaQR(x[1], x[5], x[9], x[13]);
    ChaChaQR(x[2], x[6], x[10], x[14]);
    ChaChaQR(x[3], x[7], x[11], x[15]);
    ChaChaQR(x[0], x[5], x[10], x[15]);
    ChaChaQR(x[1], x[6], x[11], x[12]);
    ChaChaQR(x[2], x[7], x[8], x[13]);
    ChaChaQR(x[3], x[4], x[9], x[14]);
  end;
  for i := 0 to 15 do
    StoreLE32(o, i * 4, x[i] + st[i]);
end;

procedure ChaCha20Xor(dst, src: PByte; len: PtrUInt; const k: TBytes32;
  const n: TBytes12; ic: Cardinal);
var
  blk: TBytes64;
  n64, i: PtrUInt;
begin
  while len > 0 do
  begin
    ChaCha20Block(blk, k, ic, n);
    if len >= 64 then
      n64 := 64
    else
      n64 := len;
    for i := 0 to n64 - 1 do
      dst[i] := src[i] xor blk[i];
    Inc(dst, n64);
    Inc(src, n64);
    Dec(len, n64);
    Inc(ic);
  end;
end;

procedure Poly1305Init(var ps: TPoly1305State; key: PByte);
var
  t0, t1, t2, t3: Cardinal;
begin
  t0 := LoadLE32P(key);
  t1 := LoadLE32P(key + 4);
  t2 := LoadLE32P(key + 8);
  t3 := LoadLE32P(key + 12);
  ps.r[0] := t0 and $03FFFFFF;
  ps.r[1] := ((t0 shr 26) or (t1 shl 6)) and $03FFFF03;
  ps.r[2] := ((t1 shr 20) or (t2 shl 12)) and $03FFC0FF;
  ps.r[3] := ((t2 shr 14) or (t3 shl 18)) and $03F03FFF;
  ps.r[4] := (t3 shr 8) and $000FFFFF;
  FillChar(ps.h[0], SizeOf(ps.h), 0);
  ps.pad[0] := LoadLE32P(key + 16);
  ps.pad[1] := LoadLE32P(key + 20);
  ps.pad[2] := LoadLE32P(key + 24);
  ps.pad[3] := LoadLE32P(key + 28);
  ps.leftover := 0;
end;

procedure Poly1305Blocks(var ps: TPoly1305State; m: PByte; bytes: PtrUInt;
  hibit: Cardinal);
var
  h0, h1, h2, h3, h4: Cardinal;
  r0, r1, r2, r3, r4, s1, s2, s3, s4: Cardinal;
  d0, d1, d2, d3, d4, c: QWord;
  t: Cardinal;
begin
  h0 := ps.h[0]; h1 := ps.h[1]; h2 := ps.h[2]; h3 := ps.h[3]; h4 := ps.h[4];
  r0 := ps.r[0]; r1 := ps.r[1]; r2 := ps.r[2]; r3 := ps.r[3]; r4 := ps.r[4];
  s1 := r1 * 5; s2 := r2 * 5; s3 := r3 * 5; s4 := r4 * 5;

  while bytes >= 16 do
  begin
    t := LoadLE32P(m);      h0 := h0 + (t and $03FFFFFF);
    t := LoadLE32P(m + 3);  h1 := h1 + ((t shr 2) and $03FFFFFF);
    t := LoadLE32P(m + 6);  h2 := h2 + ((t shr 4) and $03FFFFFF);
    t := LoadLE32P(m + 9);  h3 := h3 + ((t shr 6) and $03FFFFFF);
    t := LoadLE32P(m + 12); h4 := h4 + ((t shr 8) or hibit);

    d0 := QWord(h0) * r0 + QWord(h1) * s4 + QWord(h2) * s3 + QWord(h3) * s2 + QWord(h4) * s1;
    d1 := QWord(h0) * r1 + QWord(h1) * r0 + QWord(h2) * s4 + QWord(h3) * s3 + QWord(h4) * s2;
    d2 := QWord(h0) * r2 + QWord(h1) * r1 + QWord(h2) * r0 + QWord(h3) * s4 + QWord(h4) * s3;
    d3 := QWord(h0) * r3 + QWord(h1) * r2 + QWord(h2) * r1 + QWord(h3) * r0 + QWord(h4) * s4;
    d4 := QWord(h0) * r4 + QWord(h1) * r3 + QWord(h2) * r2 + QWord(h3) * r1 + QWord(h4) * r0;

    c := d0 shr 26; h0 := Cardinal(d0 and $03FFFFFF);
    d1 := d1 + c; c := d1 shr 26; h1 := Cardinal(d1 and $03FFFFFF);
    d2 := d2 + c; c := d2 shr 26; h2 := Cardinal(d2 and $03FFFFFF);
    d3 := d3 + c; c := d3 shr 26; h3 := Cardinal(d3 and $03FFFFFF);
    d4 := d4 + c; c := d4 shr 26; h4 := Cardinal(d4 and $03FFFFFF);
    h0 := h0 + Cardinal(c) * Cardinal(5); c := h0 shr 26; h0 := h0 and $03FFFFFF;
    h1 := h1 + Cardinal(c);

    Inc(m, 16);
    Dec(bytes, 16);
  end;

  ps.h[0] := h0; ps.h[1] := h1; ps.h[2] := h2; ps.h[3] := h3; ps.h[4] := h4;
end;

procedure Poly1305Update(var ps: TPoly1305State; m: PByte; bytes: PtrUInt);
var
  want: PtrUInt;
  i: Integer;
begin
  if ps.leftover > 0 then
  begin
    want := 16 - PtrUInt(ps.leftover);
    if want > bytes then
      want := bytes;
    Move(m^, ps.buf[ps.leftover], want);
    Inc(ps.leftover, Integer(want));
    Inc(m, want);
    Dec(bytes, want);
    if ps.leftover < 16 then
      Exit;
    Poly1305Blocks(ps, @ps.buf[0], 16, 1 shl 24);
    ps.leftover := 0;
  end;

  if bytes >= 16 then
  begin
    want := (bytes div 16) * 16;
    Poly1305Blocks(ps, m, want, 1 shl 24);
    Inc(m, want);
    Dec(bytes, want);
  end;

  if bytes > 0 then
  begin
    for i := 0 to Integer(bytes) - 1 do
      ps.buf[i] := m[i];
    ps.leftover := Integer(bytes);
  end;
end;

procedure Poly1305Finish(var ps: TPoly1305State; var mac: TBytes16);
var
  h0, h1, h2, h3, h4: Cardinal;
  g0, g1, g2, g3, g4, mask: Cardinal;
  c, f: QWord;
  i: Integer;
begin
  if ps.leftover > 0 then
  begin
    ps.buf[ps.leftover] := 1;
    for i := ps.leftover + 1 to 15 do
      ps.buf[i] := 0;
    Poly1305Blocks(ps, @ps.buf[0], 16, 0);
    ps.leftover := 0;
  end;

  h0 := ps.h[0]; h1 := ps.h[1]; h2 := ps.h[2]; h3 := ps.h[3]; h4 := ps.h[4];

  c := h1 shr 26; h1 := h1 and $03FFFFFF;
  h2 := h2 + Cardinal(c); c := h2 shr 26; h2 := h2 and $03FFFFFF;
  h3 := h3 + Cardinal(c); c := h3 shr 26; h3 := h3 and $03FFFFFF;
  h4 := h4 + Cardinal(c); c := h4 shr 26; h4 := h4 and $03FFFFFF;
  h0 := h0 + Cardinal(c) * Cardinal(5); c := h0 shr 26; h0 := h0 and $03FFFFFF;
  h1 := h1 + Cardinal(c);

  g0 := h0 + Cardinal(5); c := g0 shr 26; g0 := g0 and $03FFFFFF;
  g1 := h1 + Cardinal(c); c := g1 shr 26; g1 := g1 and $03FFFFFF;
  g2 := h2 + Cardinal(c); c := g2 shr 26; g2 := g2 and $03FFFFFF;
  g3 := h3 + Cardinal(c); c := g3 shr 26; g3 := g3 and $03FFFFFF;
  g4 := h4 + Cardinal(c) - Cardinal(1 shl 26);

  mask := (g4 shr 31) - Cardinal(1);
  g0 := g0 and mask; g1 := g1 and mask; g2 := g2 and mask;
  g3 := g3 and mask; g4 := g4 and mask;
  mask := not mask;
  h0 := (h0 and mask) or g0;
  h1 := (h1 and mask) or g1;
  h2 := (h2 and mask) or g2;
  h3 := (h3 and mask) or g3;
  h4 := (h4 and mask) or g4;

  h0 := h0 or (h1 shl 26);
  h1 := (h1 shr 6) or (h2 shl 20);
  h2 := (h2 shr 12) or (h3 shl 14);
  h3 := (h3 shr 18) or (h4 shl 8);

  f := QWord(h0) + ps.pad[0];
  StoreLE32(mac, 0, Cardinal(f));
  f := QWord(h1) + ps.pad[1] + (f shr 32);
  StoreLE32(mac, 4, Cardinal(f));
  f := QWord(h2) + ps.pad[2] + (f shr 32);
  StoreLE32(mac, 8, Cardinal(f));
  f := QWord(h3) + ps.pad[3] + (f shr 32);
  StoreLE32(mac, 12, Cardinal(f));
end;

function X25519_PublicKey(const privKey: String): String;
var
  pkey: Pointer;
  len: SizeUInt;
  q: array[0..31] of Byte;
begin
  Result := '';
  if Length(privKey) = 32 then
  begin
    pkey := EVP_PKEY_new_raw_private_key(EVP_PKEY_X25519, nil, PByte(@privKey[1]), 32);
    if pkey <> nil then
    begin
      len := 32;
      if (EVP_PKEY_get_raw_public_key(pkey, @q[0], len) = 1) and (len = 32) then
        SetString(Result, PChar(@q[0]), 32);
      EVP_PKEY_free(pkey);
    end;
  end;
end;

function X25519_SharedSecret(const privKey, pubKey: String): String;
var
  priv, peer, ctx: Pointer;
  len: SizeUInt;
  q: array[0..31] of Byte;
begin
  Result := '';
  if (Length(privKey) = 32) and (Length(pubKey) = 32) then
  begin
    priv := EVP_PKEY_new_raw_private_key(EVP_PKEY_X25519, nil, PByte(@privKey[1]), 32);
    peer := EVP_PKEY_new_raw_public_key(EVP_PKEY_X25519, nil, PByte(@pubKey[1]), 32);
    ctx := nil;
    try
      if (priv <> nil) and (peer <> nil) then
        ctx := EVP_PKEY_CTX_new(priv, nil);
      if ctx <> nil then
      begin
        if (EVP_PKEY_derive_init(ctx) = 1) and
           (EVP_PKEY_derive_set_peer(ctx, peer) = 1) then
        begin
          len := 32;
          if EVP_PKEY_derive(ctx, @q[0], len) = 1 then
            SetString(Result, PChar(@q[0]), 32);
        end;
      end;
    finally
      if ctx <> nil then
        EVP_PKEY_CTX_free(ctx);
      if priv <> nil then
        EVP_PKEY_free(priv);
      if peer <> nil then
        EVP_PKEY_free(peer);
    end;
  end;
end;

procedure SecretStreamCounterReset(var st: TSecretStreamState);
begin
  FillChar(st.nonce[0], 4, 0);
  st.nonce[0] := 1;
end;

procedure SecretStreamRekey(var st: TSecretStreamState);
var
  buf: array[0..39] of Byte;
begin
  Move(st.k[0], buf[0], 32);
  Move(st.nonce[4], buf[32], 8);
  ChaCha20Xor(@buf[0], @buf[0], 40, st.k, st.nonce, 0);
  Move(buf[0], st.k[0], 32);
  Move(buf[32], st.nonce[4], 8);
  SecretStreamCounterReset(st);
end;

function SecretStream_InitPull(const header, key: String; var state: String): Boolean;
var
  st: TSecretStreamState;
  k: TBytes32;
  in16: TBytes16;
begin
  Result := False;
  if (Length(header) = 24) and (Length(key) = 32) then
  begin
    Move(key[1], k[0], 32);
    Move(header[1], in16[0], 16);
    HChaCha20(st.k, k, in16);
    SecretStreamCounterReset(st);
    Move(header[17], st.nonce[4], 8);
    FillChar(st.pad[0], SizeOf(st.pad), 0);
    SetLength(state, SizeOf(st));
    Move(st, state[1], SizeOf(st));
    Result := True;
  end;
end;

function SecretStream_Pull(var state: String; const chunk: String;
  var msg: String; var tag: Byte): Boolean;
var
  st: TSecretStreamState;
  mlen, padn, i: Integer;
  ks1, block64: TBytes64;
  poly: TPoly1305State;
  mac, stored: TBytes16;
  lenfield: array[0..7] of Byte;
  zeros: array[0..15] of Byte;
  diff, c: Cardinal;
begin
  Result := False;
  if (Length(state) <> SizeOf(st)) or (Length(chunk) < 17) then
    Exit;
  Move(state[1], st, SizeOf(st));
  mlen := Length(chunk) - 17;
  FillChar(zeros[0], SizeOf(zeros), 0);

  ChaCha20Block(block64, st.k, 0, st.nonce);
  Poly1305Init(poly, @block64[0]);

  ChaCha20Block(ks1, st.k, 1, st.nonce);

  block64[0] := Byte(chunk[1]);
  Move(ks1[1], block64[1], 63);
  Poly1305Update(poly, @block64[0], 64);
  if mlen > 0 then
    Poly1305Update(poly, @chunk[2], PtrUInt(mlen));
  padn := mlen and 15;
  if padn > 0 then
    Poly1305Update(poly, @zeros[0], PtrUInt(padn));
  Poly1305Update(poly, @zeros[0], 8);
  FillChar(lenfield[0], 8, 0);
  StoreLE32(lenfield, 0, Cardinal(64 + mlen));
  Poly1305Update(poly, @lenfield[0], 8);
  Poly1305Finish(poly, mac);

  Move(chunk[Length(chunk) - 15], stored[0], 16);
  diff := 0;
  for i := 0 to 15 do
    diff := diff or (mac[i] xor stored[i]);
  if diff <> 0 then
    Exit;

  tag := Byte(chunk[1]) xor ks1[0];
  SetLength(msg, mlen);
  if mlen > 0 then
    ChaCha20Xor(@msg[1], @chunk[2], PtrUInt(mlen), st.k, st.nonce, 2);

  for i := 0 to 7 do
    st.nonce[4 + i] := st.nonce[4 + i] xor mac[i];
  c := LoadLE32(st.nonce, 0) + 1;
  StoreLE32(st.nonce, 0, c);
  if ((tag and $02) <> 0) or (c = 0) then
    SecretStreamRekey(st);

  Move(st, state[1], SizeOf(st));
  Result := True;
end;

end.