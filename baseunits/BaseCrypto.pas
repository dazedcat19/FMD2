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
function AESGCMEncrypt(const plaintext, key, iv: String; tagLen: Integer = 16): String;
function AESGCMDecrypt(const ciphertext, key, iv: String; tagLen: Integer = 16): String;

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

// -------------------------------------------------------------------------
// AES-GCM (NIST SP 800-38D)
//
// Internals:
//   GHASH   — GF(2^128) multiply with the GCM reduction polynomial
//   GCM_CTR — AES-CTR with big-endian 32-bit counter in the low 4 bytes
//
// Public API:
//   AESGCMEncrypt(plaintext, key, iv [, tagLen]) -> ciphertext || tag
//   AESGCMDecrypt(ciphertext||tag, key, iv [, tagLen]) -> plaintext
//                 returns '' on auth failure
//
// tagLen defaults to 16 (full 128-bit tag). Values 12..16 are typical.
// The IV (nonce) is almost always 12 bytes in practice; other lengths are
// supported via the GHASH-based J0 derivation path.
// -------------------------------------------------------------------------

type
  TGCMBlock = array[0..15] of Byte;

// GF(2^128) multiply: result = X * Y mod (x^128 + x^7 + x^2 + x + 1)
procedure GHASHMul(const X, Y: TGCMBlock; out R: TGCMBlock);
var
  V: TGCMBlock;
  Z: TGCMBlock;
  i, j, k: Integer;
  bit: Byte;
  lsb: Byte;
begin
  FillChar(Z, SizeOf(Z), 0);
  Move(Y, V, SizeOf(V));

  for i := 0 to 15 do
  begin
    for j := 7 downto 0 do
    begin
      bit := (X[i] shr j) and 1;
      if bit = 1 then
        for k := 0 to 15 do
          Z[k] := Z[k] xor V[k];

      // save lsb of V before shifting
      lsb := V[15] and 1;
      // V = V >> 1 (big-endian bitstream)
      for k := 15 downto 1 do
        V[k] := (V[k] shr 1) or ((V[k - 1] and 1) shl 7);
      V[0] := V[0] shr 1;

      if lsb = 1 then
        V[0] := V[0] xor $E1;
    end;
  end;

  Move(Z, R, SizeOf(R));
end;

// GHASH(H, A, C): authenticate additional data A and ciphertext C under key H
// All lengths are in bytes; both A and C may be empty.
procedure GHASH(const H: TGCMBlock; const A, C: String; out Tag: TGCMBlock);
var
  Y, tmp: TGCMBlock;
  i, j, n: Integer;
  lenA, lenC: QWord;
  lenBuf: array[0..15] of Byte;
begin
  FillChar(Y, SizeOf(Y), 0);

  // Process A (additional authenticated data) in 16-byte blocks
  i := 1;
  while i <= Length(A) do
  begin
    FillChar(tmp, SizeOf(tmp), 0);
    n := Min(16, Length(A) - i + 1);
    Move(A[i], tmp[0], n);
    for j := 0 to 15 do tmp[j] := tmp[j] xor Y[j];
    GHASHMul(tmp, H, Y);
    Inc(i, 16);
  end;

  // Process C (ciphertext) in 16-byte blocks
  i := 1;
  while i <= Length(C) do
  begin
    FillChar(tmp, SizeOf(tmp), 0);
    n := Min(16, Length(C) - i + 1);
    Move(C[i], tmp[0], n);
    for j := 0 to 15 do tmp[j] := tmp[j] xor Y[j];
    GHASHMul(tmp, H, Y);
    Inc(i, 16);
  end;

  // Final block: len(A) || len(C) in bits, big-endian 64-bit each
  lenA := QWord(Length(A)) * 8;
  lenC := QWord(Length(C)) * 8;
  lenBuf[0]  := (lenA shr 56) and $FF;
  lenBuf[1]  := (lenA shr 48) and $FF;
  lenBuf[2]  := (lenA shr 40) and $FF;
  lenBuf[3]  := (lenA shr 32) and $FF;
  lenBuf[4]  := (lenA shr 24) and $FF;
  lenBuf[5]  := (lenA shr 16) and $FF;
  lenBuf[6]  := (lenA shr  8) and $FF;
  lenBuf[7]  :=  lenA         and $FF;
  lenBuf[8]  := (lenC shr 56) and $FF;
  lenBuf[9]  := (lenC shr 48) and $FF;
  lenBuf[10] := (lenC shr 40) and $FF;
  lenBuf[11] := (lenC shr 32) and $FF;
  lenBuf[12] := (lenC shr 24) and $FF;
  lenBuf[13] := (lenC shr 16) and $FF;
  lenBuf[14] := (lenC shr  8) and $FF;
  lenBuf[15] :=  lenC         and $FF;

  for j := 0 to 15 do lenBuf[j] := lenBuf[j] xor Y[j];
  GHASHMul(lenBuf, H, Tag);
end;

// Increment the 32-bit big-endian counter in the low 4 bytes of a GCM block
procedure GCMIncCounter(var ctr: TGCMBlock);
var
  i: Integer;
begin
  for i := 15 downto 12 do
  begin
    Inc(ctr[i]);
    if ctr[i] <> 0 then Break;
  end;
end;

// CTR-mode encrypt/decrypt using a pre-initialised rijndael cipher object.
// Counter starts at ctr0 and is incremented per 16-byte block.
procedure GCMCTRCrypt(cipher: TDCP_rijndael; const ctr0: TGCMBlock; const src: String; out dst: String);
var
  ctr: TGCMBlock;
  ks: TGCMBlock;
  i, j, n: Integer;
begin
  Move(ctr0, ctr, SizeOf(ctr));
  SetLength(dst, Length(src));
  i := 0;
  while i < Length(src) do
  begin
    cipher.EncryptECB(ctr, ks);
    n := Min(16, Length(src) - i);
    for j := 0 to n - 1 do
      dst[i + j + 1] := Char(Byte(src[i + j + 1]) xor ks[j]);
    GCMIncCounter(ctr);
    Inc(i, 16);
  end;
end;

// Derive J0 (the initial counter block) from the IV.
// For a 12-byte IV: J0 = IV || 0x00000001 (big-endian).
// For other lengths: J0 = GHASH(H, '', IV) per NIST SP 800-38D §7.1.
procedure GCMDeriveJ0(const H: TGCMBlock; const iv: String; out J0: TGCMBlock);
begin
  if Length(iv) = 12 then
  begin
    FillChar(J0, SizeOf(J0), 0);
    Move(iv[1], J0[0], 12);
    J0[15] := 1;
  end
  else
    GHASH(H, '', iv, J0);
end;

function AESGCMEncrypt(const plaintext, key, iv: String; tagLen: Integer): String;
var
  keyBytes: TBytes;
  i: Integer;
  H: TGCMBlock;
  J0: TGCMBlock;
  ctr1: TGCMBlock;
  zeroBlock: TGCMBlock;
  ciphertext: String;
  ghashTag, eJ0: TGCMBlock;
  tag: String;
  cipher: TDCP_rijndael;
begin
  Result := '';
  if (key = '') or (iv = '') then Exit;
  if tagLen < 1 then tagLen := 16;
  if tagLen > 16 then tagLen := 16;

  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  cipher := TDCP_rijndael.Create(nil);
  try
    try
      cipher.Init(keyBytes[0], Length(keyBytes) * 8, nil);

      // H = E(K, 0^128)
      FillChar(zeroBlock, SizeOf(zeroBlock), 0);
      cipher.EncryptECB(zeroBlock, H);

      // Derive J0
      GCMDeriveJ0(H, iv, J0);

      // CTR starts at J0+1
      Move(J0, ctr1, SizeOf(J0));
      GCMIncCounter(ctr1);

      // Encrypt plaintext
      if plaintext <> '' then
        GCMCTRCrypt(cipher, ctr1, plaintext, ciphertext)
      else
        ciphertext := '';

      // Tag = GHASH(H, '', ciphertext) xor E(K, J0)
      GHASH(H, '', ciphertext, ghashTag);
      cipher.EncryptECB(J0, eJ0);
      SetLength(tag, tagLen);
      for i := 0 to tagLen - 1 do
        tag[i + 1] := Char(ghashTag[i] xor eJ0[i]);

      cipher.Burn;
      Result := ciphertext + tag;
    except
      Result := '';
    end;
  finally
    cipher.Free;
  end;
end;

function AESGCMDecrypt(const ciphertext, key, iv: String; tagLen: Integer): String;
var
  keyBytes: TBytes;
  i: Integer;
  H: TGCMBlock;
  J0: TGCMBlock;
  ctr1: TGCMBlock;
  zeroBlock: TGCMBlock;
  ct, storedTag: String;
  ghashTag, eJ0: TGCMBlock;
  computedTag: String;
  ok: Boolean;
  cipher: TDCP_rijndael;
begin
  Result := '';
  if (key = '') or (iv = '') then Exit;
  if tagLen < 1 then tagLen := 16;
  if tagLen > 16 then tagLen := 16;
  if Length(ciphertext) < tagLen then Exit;

  ct := Copy(ciphertext, 1, Length(ciphertext) - tagLen);
  storedTag := Copy(ciphertext, Length(ciphertext) - tagLen + 1, tagLen);

  SetLength(keyBytes, Length(key));
  for i := 0 to Length(key) - 1 do
    keyBytes[i] := Byte(key[i + 1]);

  cipher := TDCP_rijndael.Create(nil);
  try
    try
      cipher.Init(keyBytes[0], Length(keyBytes) * 8, nil);

      FillChar(zeroBlock, SizeOf(zeroBlock), 0);
      cipher.EncryptECB(zeroBlock, H);

      GCMDeriveJ0(H, iv, J0);

      // Verify tag before decrypting
      GHASH(H, '', ct, ghashTag);
      cipher.EncryptECB(J0, eJ0);
      SetLength(computedTag, tagLen);
      for i := 0 to tagLen - 1 do
        computedTag[i + 1] := Char(ghashTag[i] xor eJ0[i]);

      // Constant-time comparison
      ok := True;
      for i := 1 to tagLen do
        if Byte(computedTag[i]) xor Byte(storedTag[i]) <> 0 then
          ok := False;

      if not ok then
      begin
        cipher.Burn;
        Exit;
      end;

      // Decrypt
      Move(J0, ctr1, SizeOf(J0));
      GCMIncCounter(ctr1);
      if ct <> '' then
        GCMCTRCrypt(cipher, ctr1, ct, Result)
      else
        Result := '';

      cipher.Burn;
    except
      Result := '';
    end;
  finally
    cipher.Free;
  end;
end;

end.