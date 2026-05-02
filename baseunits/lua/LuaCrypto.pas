unit LuaCrypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses
  LuaUtils, LuaPackage, BaseCrypto, synacode, uBaseUnit, htmlelements;

function GetLuaString(L: Plua_State; idx: Integer): String;
var
  len: size_t;
  p: PChar;
begin
  p := luaL_checklstring(L, idx, @len);
  SetString(Result, p, len);
end;

procedure PushLuaString(L: Plua_State; const s: String);
begin
  if s <> '' then
    lua_pushlstring(L, PChar(s), Length(s))
  else
    lua_pushliteral(L, '');
end;

function crypto_hextostr(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, HexToStr(GetLuaString(L, 1)));
  Result := 1;
end;

function crypto_strtohexstr(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, StrToHexStr(GetLuaString(L, 1)));
  Result := 1;
end;

function crypto_md5hex(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MD5Hex(GetLuaString(L, 1)));
  Result := 1;
end;

function crypto_sha256hex(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SHA256Hex(GetLuaString(L, 1)));
  Result := 1;
end;

function crypto_sha512hex(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SHA512Hex(GetLuaString(L, 1)));
  Result := 1;
end;

function crypto_hmac_sha256hex(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, HMAC_SHA256Hex(GetLuaString(L, 1), GetLuaString(L, 2)));
  Result := 1;
end;

function crypto_hmac_sha512hex(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, HMAC_SHA512Hex(GetLuaString(L, 1), GetLuaString(L, 2)));
  Result := 1;
end;

function crypto_aesencryptcbc(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, AESEncryptCBC(GetLuaString(L, 1), GetLuaString(L, 2), GetLuaString(L, 3)));
  Result := 1;
end;

function crypto_aesdecryptcbc(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, AESDecryptCBC(GetLuaString(L, 1), GetLuaString(L, 2), GetLuaString(L, 3)));
  Result := 1;
end;

function crypto_AESEncryptCBCSHA256Base64Pkcs7(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, AESEncryptCBCSHA256Base64Pkcs7(GetLuaString(L, 1), GetLuaString(L, 2), GetLuaString(L, 3)));
  Result := 1;
end;

function crypto_AESDecryptCBCSHA256Base64Pkcs7(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, AESDecryptCBCSHA256Base64Pkcs7(GetLuaString(L, 1), GetLuaString(L, 2), GetLuaString(L, 3)));
  Result := 1;
end;

function crypto_AESDecryptCBCMD5Base64ZerosPadding(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, AESDecryptCBCMD5Base64ZerosPadding(GetLuaString(L, 1), GetLuaString(L, 2), GetLuaString(L, 3)));
  Result := 1;
end;

function crypto_AESDecryptCBCHexBase64ZerosPadding(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, AESDecryptCBCHexBase64ZerosPadding(GetLuaString(L, 1), GetLuaString(L, 2), GetLuaString(L, 3)));
  Result := 1;
end;

function lua_encryptstring(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, EncryptString(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_decryptstring(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, DecryptString(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_htmldecode(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, HTMLDecode(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_htmlencode(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, EscapeHTML(GetLuaString(L, 1)));
  Result := 1;
end;

// -- synacode

function lua_decodeurl(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, DecodeURL(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_encodeurl(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeURL(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_decodeuu(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, DecodeUU(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_encodeuu(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeUU(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_encodeurlelement(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeURLElement(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_decodebase64(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, DecodeBase64(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_encodebase64(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeBase64(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_crc16(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, Crc16(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_crc32(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, Crc32(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_md4(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, MD4(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_md5(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, MD5(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_hmac_md5(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, HMAC_MD5(GetLuaString(L, 1), GetLuaString(L, 2)));
  Result := 1;
end;

function lua_md5longhash(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, MD5LongHash(GetLuaString(L, 1), lua_tointeger(L, 2)));
  Result := 1;
end;

function lua_sha1(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, SHA1(GetLuaString(L, 1)));
  Result := 1;
end;

function lua_hmac_sha1(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, HMAC_SHA1(GetLuaString(L, 1), GetLuaString(L, 2)));
  Result := 1;
end;

function lua_sha1longhash(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, SHA1LongHash(GetLuaString(L, 1), lua_tointeger(L, 2)));
  Result := 1;
end;

function crypto_sha256(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, SHA256(GetLuaString(L, 1)));
  Result := 1;
end;

function crypto_hmac_sha256(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, HMAC_SHA256(GetLuaString(L, 1), GetLuaString(L, 2)));
  Result := 1;
end;

function crypto_sha512(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, SHA512(GetLuaString(L, 1)));
  Result := 1;
end;

function crypto_hmac_sha512(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, HMAC_SHA512(GetLuaString(L, 1), GetLuaString(L, 2)));
  Result := 1;
end;

function crypto_rc4(L: Plua_State): Integer; cdecl;
begin
  PushLuaString(L, RC4(GetLuaString(L, 1), GetLuaString(L, 2)));
  Result := 1;
end;

const
  cryptomethods: packed array [0..22] of luaL_Reg = (
    (name: 'EncryptString'; func: @lua_encryptstring),
    (name: 'DecryptString'; func: @lua_decryptstring),
    (name: 'HTMLDecode'; func: @lua_htmldecode),
    (name: 'HTMLEncode'; func: @lua_htmlencode),
    (name: 'HexToStr'; func: @crypto_hextostr),
    (name: 'StrToHexStr'; func: @crypto_strtohexstr),
    (name: 'MD5Hex'; func: @crypto_md5hex),
    (name: 'SHA256'; func: @crypto_sha256),
    (name: 'SHA512'; func: @crypto_sha512),
    (name: 'SHA256Hex'; func: @crypto_sha256hex),
    (name: 'SHA512Hex'; func: @crypto_sha512hex),
    (name: 'HMAC_SHA256'; func: @crypto_hmac_sha256),
    (name: 'HMAC_SHA512'; func: @crypto_hmac_sha512),
    (name: 'HMAC_SHA256Hex'; func: @crypto_hmac_sha256hex),
    (name: 'HMAC_SHA512Hex'; func: @crypto_hmac_sha512hex),
	(name: 'AESEncryptCBC'; func: @crypto_aesencryptcbc),
    (name: 'AESDecryptCBC'; func: @crypto_aesdecryptcbc),
    (name: 'AESEncryptCBCSHA256Base64Pkcs7'; func: @crypto_AESEncryptCBCSHA256Base64Pkcs7),
    (name: 'AESDecryptCBCSHA256Base64Pkcs7'; func: @crypto_AESDecryptCBCSHA256Base64Pkcs7),
    (name: 'AESDecryptCBCMD5Base64ZerosPadding'; func: @crypto_AESDecryptCBCMD5Base64ZerosPadding),
    (name: 'AESDecryptCBCHexBase64ZerosPadding'; func: @crypto_AESDecryptCBCHexBase64ZerosPadding),
    (name: 'RC4'; func: @crypto_rc4),
    (name: nil; func: nil)
    );

  synacodemethods: packed array [0..16] of luaL_Reg = (
    (name: 'DecodeURL'; func: @lua_decodeurl),
    (name: 'EncodeURL'; func: @lua_encodeurl),
    (name: 'DecodeUU'; func: @lua_decodeuu),
    (name: 'EncodeUU'; func: @lua_encodeuu),
    (name: 'EncodeURLElement'; func: @lua_encodeurlelement),
    (name: 'DecodeBase64'; func: @lua_decodebase64),
    (name: 'EncodeBase64'; func: @lua_encodebase64),
    (name: 'CRC16'; func: @lua_crc16),
    (name: 'CRC32'; func: @lua_crc32),
    (name: 'MD4'; func: @lua_md4),
    (name: 'MD5'; func: @lua_md5),
    (name: 'HMAC_MD5'; func: @lua_hmac_md5),
    (name: 'MD5LongHash'; func: @lua_md5longhash),
    (name: 'SHA1'; func: @lua_sha1),
    (name: 'HMAC_SHA1'; func: @lua_hmac_sha1),
    (name: 'SHA1LongHash'; func: @lua_sha1longhash),
    (name: nil; func: nil)
    );

function luaopen_crypto(L: Plua_State): Integer; cdecl;
begin
  luaNewLibTable(L, [cryptomethods, synacodemethods]);
  Result := 1;
end;

initialization
  LuaPackage.AddLib('crypto', @luaopen_crypto);

end.