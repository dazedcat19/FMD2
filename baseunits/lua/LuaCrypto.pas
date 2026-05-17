unit LuaCrypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {$ifdef luajit}lua{$else}{$ifdef lua54}lua54{$else}lua53{$endif}{$endif};

implementation

uses
  LuaUtils, LuaPackage, BaseCrypto, synacode, uBaseUnit, htmlelements;

function crypto_hextostr(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, HexToStr(luaToString(L, 1)));
  Result := 1;
end;

function crypto_strtohexstr(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, StrToHexStr(luaToString(L, 1)));
  Result := 1;
end;

function crypto_md5hex(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MD5Hex(luaToString(L, 1)));
  Result := 1;
end;

function crypto_aesdecryptcbc(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, AESDecryptCBC(luaToString(L, 1), luaToString(L, 2), luaToString(L, 3)));
  Result := 1;
end;

function crypto_AESDecryptCBCSHA256Base64Pkcs7(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, AESDecryptCBCSHA256Base64Pkcs7(luaToString(L, 1), luaToString(L, 2), luaToString(L, 3)));
  Result := 1;
end;

function lua_encryptstring(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncryptString(luaToString(L, 1)));
  Result := 1;
end;

function lua_decryptstring(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, DecryptString(luaToString(L, 1)));
  Result := 1;
end;

function lua_htmldecode(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, HTMLDecode(luaToString(L, 1)));
  Result := 1;
end;

function lua_htmlencode(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EscapeHTML(luaToString(L, 1)));
  Result := 1;
end;

// -- synacode

function lua_decodeurl(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, DecodeURL(luaToString(L, 1)));
  Result := 1;
end;

function lua_encodeurl(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeURL(luaToString(L, 1)));
  Result := 1;
end;

function lua_decodeuu(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, DecodeUU(luaToString(L, 1)));
  Result := 1;
end;

function lua_encodeuu(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeUU(luaToString(L, 1)));
  Result := 1;
end;

function lua_encodeurlelement(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeURLElement(luaToString(L, 1)));
  Result := 1;
end;

function lua_decodebase64(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, DecodeBase64(luaToString(L, 1)));
  Result := 1;
end;

function lua_encodebase64(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, EncodeBase64(luaToString(L, 1)));
  Result := 1;
end;

function lua_crc16(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, Crc16(luaToString(L, 1)));
  Result := 1;
end;

function lua_crc32(L: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(L, Crc32(luaToString(L, 1)));
  Result := 1;
end;

function lua_md4(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MD4(luaToString(L, 1)));
  Result := 1;
end;

function lua_md5(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MD5(luaToString(L, 1)));
  Result := 1;
end;

function lua_hmac_md5(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, HMAC_MD5(luaToString(L, 1), luaToString(L, 2)));
  Result := 1;
end;

function lua_md5longhash(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, MD5LongHash(luaToString(L, 1), lua_tointeger(L, 2)));
  Result := 1;
end;

function lua_sha1(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SHA1(luaToString(L, 1)));
  Result := 1;
end;

function lua_hmac_sha1(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, HMAC_SHA1(luaToString(L, 1), luaToString(L, 2)));
  Result := 1;
end;

function lua_sha1longhash(L: Plua_State): Integer; cdecl;
begin
  lua_pushstring(L, SHA1LongHash(luaToString(L, 1), lua_tointeger(L, 2)));
  Result := 1;
end;

const
  cryptomethods: packed array [0..9] of luaL_Reg = (
    (name: 'EncryptString'; func: @lua_encryptstring),
    (name: 'DecryptString'; func: @lua_decryptstring),
    (name: 'HTMLDecode'; func: @lua_htmldecode),
    (name: 'HTMLEncode'; func: @lua_htmlencode),
    (name: 'HexToStr'; func: @crypto_hextostr),
    (name: 'StrToHexStr'; func: @crypto_strtohexstr),
    (name: 'MD5Hex'; func: @crypto_md5hex),
    (name: 'AESDecryptCBC'; func: @crypto_aesdecryptcbc),
    (name: 'AESDecryptCBCSHA256Base64Pkcs7'; func: @crypto_AESDecryptCBCSHA256Base64Pkcs7),
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

