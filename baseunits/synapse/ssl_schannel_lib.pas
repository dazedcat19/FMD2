{==============================================================================|
| Project : Ararat Synapse                                                     |
|==============================================================================|
| Content: SSPI / SChannel / Crypt32 interface declarations                    |
|==============================================================================|
| Copyright (c) 2026, contributed to Synapse project                           |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:   |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL      |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR  |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER  |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT          |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY   |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c) 2026.                    |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Claude (Anthropic) - AI-assisted implementation                            |
|==============================================================================|
| Description:                                                                 |
|   Windows-only unit that declares all types, constants and function          |
|   variables needed to call the Windows SChannel TLS implementation through   |
|   the SSPI interface and the Crypt32 certificate API.                        |
|                                                                              |
|   All functions are loaded dynamically at runtime from:                      |
|     secur32.dll  - SSPI / SChannel (AcquireCredentialsHandle, etc.)         |
|     crypt32.dll  - Certificate store API (PFXImportCertStore, etc.)         |
|                                                                              |
|   Unicode / ANSI dual-mode:                                                  |
|     On Delphi 2009+ (UNICODE defined) the W variants of API functions are    |
|     loaded and the un-suffixed names (e.g. SChannel_AcquireCredentialsHandle)|
|     are aliased to the W variables.                                          |
|     On pre-2009 Delphi (ANSI) the A variants are loaded and aliased.         |
|     Plugin code in ssl_schannel.pas always uses the un-suffixed names so it  |
|     compiles without changes on both compilers.                              |
|                                                                              |
|   Call InitSChannelInterface before using any function variable.             |
|   The unit finalization unloads the DLLs automatically.                      |
|==============================================================================|
| History:                                                                     |
|   2026  Initial version                                                      |
|==============================================================================}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
{$WARN SYMBOL_PLATFORM OFF}

unit ssl_schannel_lib;

{$IFNDEF MSWINDOWS}
  {$ERROR This unit is Windows-only (requires SChannel / SSPI).}
{$ENDIF}

interface

uses
  Windows;

// ---------------------------------------------------------------------------
// SSPI / SChannel constants
// ---------------------------------------------------------------------------
const
  { Credential use flags for AcquireCredentialsHandle }
  SECPKG_CRED_INBOUND              = $00000001;
  SECPKG_CRED_OUTBOUND             = $00000002;

  { InitializeSecurityContext request flags }
  ISC_REQ_REPLAY_DETECT            = $00000004;
  ISC_REQ_SEQUENCE_DETECT          = $00000008;
  ISC_REQ_CONFIDENTIALITY          = $00000010;
  ISC_REQ_ALLOCATE_MEMORY          = $00000100;
  ISC_REQ_EXTENDED_ERROR           = $00004000;
  ISC_REQ_STREAM                   = $00008000;
  ISC_REQ_MANUAL_CRED_VALIDATION   = $00080000;

  { AcceptSecurityContext request flags }
  ASC_REQ_REPLAY_DETECT            = $00000004;
  ASC_REQ_SEQUENCE_DETECT          = $00000008;
  ASC_REQ_CONFIDENTIALITY          = $00000010;
  ASC_REQ_ALLOCATE_MEMORY          = $00000100;
  ASC_REQ_EXTENDED_ERROR           = $00008000;
  ASC_REQ_STREAM                   = $00010000;

  { SecBuffer types }
  SECBUFFER_VERSION                = 0;
  SECBUFFER_EMPTY                  = 0;
  SECBUFFER_DATA                   = 1;
  SECBUFFER_TOKEN                  = 2;
  SECBUFFER_EXTRA                  = 5;
  SECBUFFER_STREAM_TRAILER         = 6;
  SECBUFFER_STREAM_HEADER          = 7;
  SECBUFFER_ALERT                  = 17;

  { SECURITY_STATUS success / informational codes }
  SEC_E_OK                         = HRESULT(0);
  SEC_I_CONTINUE_NEEDED            = HRESULT($00090312);
  SEC_I_COMPLETE_NEEDED            = HRESULT($00090313);
  SEC_I_COMPLETE_AND_CONTINUE      = HRESULT($00090314);
  SEC_I_CONTEXT_EXPIRED            = HRESULT($00090317);
  SEC_I_RENEGOTIATE                = HRESULT($00090321);

  { SECURITY_STATUS error codes }
  SEC_E_INCOMPLETE_MESSAGE         = HRESULT($80090318);
  SEC_E_INCOMPLETE_CREDENTIALS     = HRESULT($80090320);

  { SCHANNEL_CRED.dwVersion }
  SCHANNEL_CRED_VERSION            = $00000004;

  { SCHANNEL_CRED.dwFlags }
  SCH_CRED_NO_DEFAULT_CREDS        = $00000010;
  SCH_CRED_AUTO_CRED_VALIDATION    = $00000020;
  SCH_CRED_MANUAL_CRED_VALIDATION  = $00000080;
  SCH_CRED_NO_SERVERNAME_CHECK     = $00000004;

  { Enabled protocol bitmasks for SCHANNEL_CRED.grbitEnabledProtocols }
  SP_PROT_TLS1_SERVER              = $00000040;
  SP_PROT_TLS1_CLIENT              = $00000080;
  SP_PROT_TLS1_1_SERVER            = $00000100;
  SP_PROT_TLS1_1_CLIENT            = $00000200;
  SP_PROT_TLS1_2_SERVER            = $00000400;
  SP_PROT_TLS1_2_CLIENT            = $00000800;
  SP_PROT_TLS1_3_SERVER            = $00001000;
  SP_PROT_TLS1_3_CLIENT            = $00002000;

  { Package name passed to AcquireCredentialsHandle }
  UNISP_NAME                       = 'Microsoft Unified Security Protocol Provider';
  { Wide-string version – always use this with the W variant of the function,
    regardless of the Delphi build mode }
  UNISP_NAME_W : PWideChar         = 'Microsoft Unified Security Protocol Provider';

  { QueryContextAttributes attribute selectors }
  SECPKG_ATTR_SIZES                = 0;   { SecPkgContext_Sizes }
  SECPKG_ATTR_STREAM_SIZES         = 4;   { SecPkgContext_StreamSizes }
  SECPKG_ATTR_KEY_INFO             = 5;   { SecPkgContext_KeyInfo – protocol/cipher, old API }
  SECPKG_ATTR_CONNECTION_INFO      = $5A; { SecPkgContext_ConnectionInfo = 90 }
  SECPKG_ATTR_REMOTE_CERT_CONTEXT  = $53; { PCCERT_CONTEXT of peer certificate }

  { Crypt32 encoding flags }
  X509_ASN_ENCODING                = $00000001;
  PKCS_7_ASN_ENCODING              = $00010000;

  { CertCloseStore flags }
  CERT_CLOSE_STORE_FORCE_FLAG      = $00000001;

  { PFXImportCertStore flags }
  CRYPT_EXPORTABLE                 = $00000001;
  PKCS12_ALLOW_OVERWRITE_KEY       = $00004000;
  PKCS12_NO_PERSIST_KEY            = $00008000;

  { CertGetNameString type selectors }
  CERT_NAME_SIMPLE_DISPLAY_TYPE    = 4;
  CERT_NAME_RDN_TYPE               = 6;
  CERT_NAME_ISSUER_FLAG            = $00000001;

  { ALG_ID values – bulk encryption ciphers reported by SChannel in
    SecPkgContext_ConnectionInfo.aiCipher }
  CALG_DES                         = $6601;  { DES (56-bit) }
  CALG_RC2                         = $6602;  { RC2 block cipher }
  CALG_3DES                        = $6603;  { Triple DES (168-bit) }
  CALG_DESX                        = $6604;  { DESX }
  CALG_3DES_112                    = $6609;  { Two-key 3DES (112-bit) }
  CALG_RC5                         = $660D;  { RC5 }
  CALG_AES_128                     = $660E;  { AES 128-bit }
  CALG_AES_192                     = $660F;  { AES 192-bit }
  CALG_AES_256                     = $6610;  { AES 256-bit }
  CALG_AES                         = $6611;  { AES (generic, no specific key size) }
  CALG_RC4                         = $6801;  { RC4 stream cipher }
  CALG_SEAL                        = $6802;  { SEAL (not supported by SChannel) }
  { Note: TLS 1.3 uses AEAD ciphers (AES-GCM, ChaCha20) which SChannel
    reports as CALG_AES_128 or CALG_AES_256 with appropriate key size }

  { dwProtocol values returned in TSecPkgContext_ConnectionInfo.
    SChannel reports the CLIENT bit when acting as client and the SERVER bit
    when acting as server, so we match on both bits combined. }
  PROTO_TLS1_0_SERVER              = $00000040;
  PROTO_TLS1_0_CLIENT              = $00000080;
  PROTO_TLS1_1_SERVER              = $00000100;
  PROTO_TLS1_1_CLIENT              = $00000200;
  PROTO_TLS1_2_SERVER              = $00000400;
  PROTO_TLS1_2_CLIENT              = $00000800;
  PROTO_TLS1_3_SERVER              = $00001000;
  PROTO_TLS1_3_CLIENT              = $00002000;
  { Combined masks covering both client and server bits }
  PROTO_TLS1_0                     = $000000C0;
  PROTO_TLS1_1                     = $00000300;
  PROTO_TLS1_2                     = $00000C00;
  PROTO_TLS1_3                     = $00003000;

// ---------------------------------------------------------------------------
// SSPI types
// ---------------------------------------------------------------------------
type
  SECURITY_STATUS = HRESULT;

  PSecHandle = ^TSecHandle;
  TSecHandle = record
    dwLower: ULONG_PTR;
    dwUpper: ULONG_PTR;
  end;

  TCredHandle = TSecHandle;
  PCredHandle = ^TCredHandle;

  TCtxtHandle = TSecHandle;
  PCtxtHandle = ^TCtxtHandle;

  PTimeStamp = ^TTimeStamp;
  TTimeStamp = record
    LowPart:  DWORD;
    HighPart: LongInt;
  end;

  PSecBuffer = ^TSecBuffer;
  TSecBuffer = record
    cbBuffer:   ULONG;
    BufferType: ULONG;
    pvBuffer:   Pointer;
  end;

  PSecBufferDesc = ^TSecBufferDesc;
  TSecBufferDesc = record
    ulVersion: ULONG;
    cBuffers:  ULONG;
    pBuffers:  PSecBuffer;
  end;

  PCCERT_CONTEXT = Pointer;
  HCERTSTORE     = Pointer;

  PSCHANNEL_CRED = ^TSCHANNEL_CRED;
  TSCHANNEL_CRED = record
    dwVersion:               DWORD;
    cCreds:                  DWORD;
    paCred:                  ^PCCERT_CONTEXT;
    hRootStore:              HCERTSTORE;
    cMappers:                DWORD;
    aphMappers:              Pointer;
    cSupportedAlgs:          DWORD;
    palgSupportedAlgs:       Pointer;
    grbitEnabledProtocols:   DWORD;
    dwMinimumCipherStrength: DWORD;
    dwMaximumCipherStrength: DWORD;
    dwSessionLifespan:       DWORD;
    dwFlags:                 DWORD;
    dwCredFormat:            DWORD;
  end;

  PSecPkgContext_StreamSizes = ^TSecPkgContext_StreamSizes;
  TSecPkgContext_StreamSizes = record
    cbHeader:         ULONG;
    cbTrailer:        ULONG;
    cbMaximumMessage: ULONG;
    cBuffers:         ULONG;
    cbBlockSize:      ULONG;
  end;

  PSecPkgContext_ConnectionInfo = ^TSecPkgContext_ConnectionInfo;
  TSecPkgContext_ConnectionInfo = record
    dwProtocol:       DWORD;
    aiCipher:         DWORD;
    dwCipherStrength: DWORD;
    aiHash:           DWORD;
    dwHashStrength:   DWORD;
    aiExch:           DWORD;
    dwExchStrength:   DWORD;
  end;

  { Returned by SECPKG_ATTR_KEY_INFO – older API, works on XP/Vista era Windows }
  PSecPkgContext_KeyInfo = ^TSecPkgContext_KeyInfo;
  TSecPkgContext_KeyInfo = record
    sSignatureAlgorithmName : PWideChar;
    sEncryptAlgorithmName   : PWideChar;
    KeySize                 : ULONG;
    SignatureAlgorithm      : ULONG;
    EncryptAlgorithm        : ULONG;
  end;

  PCryptDataBlob = ^TCryptDataBlob;
  TCryptDataBlob = record
    cbData: DWORD;
    pbData: PByte;
  end;

  { Minimal CRYPT_INTEGER_BLOB – reused for serial number }
  PCryptIntegerBlob = ^TCryptIntegerBlob;
  TCryptIntegerBlob = record
    cbData: DWORD;
    pbData: PByte;
  end;

  { Minimal CERT_INFO – we only access dwVersion, SerialNumber and Subject.
    The real structure has many more fields; we declare just enough here. }
  PCertInfo = ^TCertInfo;
  TCertInfo = record
    dwVersion:    DWORD;
    SerialNumber: TCryptIntegerBlob;  { certificate serial number }
    SignatureAlgorithm: record        { CRYPT_ALGORITHM_IDENTIFIER – 2 fields }
      pszObjId:   PAnsiChar;
      Parameters: TCryptDataBlob;
    end;
    Issuer:       TCryptDataBlob;     { DER-encoded issuer DN }
    { NotBefore, NotAfter – FILETIME = 2x DWORD each }
    NotBefore:    record Lo, Hi: DWORD; end;
    NotAfter:     record Lo, Hi: DWORD; end;
    Subject:      TCryptDataBlob;     { DER-encoded subject DN }
    { remaining fields omitted – never accessed directly }
  end;

  { Minimal CERT_CONTEXT – only pCertInfo and cbCertEncoded/pbCertEncoded used }
  PCertContext = ^TCertContext;
  TCertContext = record
    dwCertEncodingType : DWORD;
    pbCertEncoded      : PByte;
    cbCertEncoded      : DWORD;
    pCertInfo          : PCertInfo;
    hCertStore         : HCERTSTORE;
  end;

// ---------------------------------------------------------------------------
// SSPI function-pointer types – W (Unicode) variants
// ---------------------------------------------------------------------------
type
  TAcquireCredentialsHandleW = function(
    pszPrincipal:     PWideChar;
    pszPackage:       PWideChar;
    fCredentialUse:   ULONG;
    pvLogonId:        Pointer;
    pAuthData:        Pointer;
    pGetKeyFn:        Pointer;
    pvGetKeyArgument: Pointer;
    phCredential:     PCredHandle;
    ptsExpiry:        PTimeStamp
  ): SECURITY_STATUS; stdcall;

  TInitializeSecurityContextW = function(
    phCredential:     PCredHandle;
    phContext:        PCtxtHandle;
    pszTargetName:    PWideChar;
    fContextReq:      ULONG;
    Reserved1:        ULONG;
    TargetDataRep:    ULONG;
    pInput:           PSecBufferDesc;
    Reserved2:        ULONG;
    phNewContext:     PCtxtHandle;
    pOutput:          PSecBufferDesc;
    pfContextAttr:    PULONG;
    ptsExpiry:        PTimeStamp
  ): SECURITY_STATUS; stdcall;

  TQueryContextAttributesW = function(
    phContext:    PCtxtHandle;
    ulAttribute:  ULONG;
    pBuffer:      Pointer
  ): SECURITY_STATUS; stdcall;

  TCertGetNameStringW = function(
    pCertContext:  PCCERT_CONTEXT;
    dwType:        DWORD;
    dwFlags:       DWORD;
    pvTypePara:    Pointer;
    pszNameString: PWideChar;
    cchNameString: DWORD
  ): DWORD; stdcall;

// ---------------------------------------------------------------------------
// SSPI function-pointer types – A (ANSI) variants
// ---------------------------------------------------------------------------
type
  TAcquireCredentialsHandleA = function(
    pszPrincipal:     PAnsiChar;
    pszPackage:       PAnsiChar;
    fCredentialUse:   ULONG;
    pvLogonId:        Pointer;
    pAuthData:        Pointer;
    pGetKeyFn:        Pointer;
    pvGetKeyArgument: Pointer;
    phCredential:     PCredHandle;
    ptsExpiry:        PTimeStamp
  ): SECURITY_STATUS; stdcall;

  TInitializeSecurityContextA = function(
    phCredential:     PCredHandle;
    phContext:        PCtxtHandle;
    pszTargetName:    PAnsiChar;
    fContextReq:      ULONG;
    Reserved1:        ULONG;
    TargetDataRep:    ULONG;
    pInput:           PSecBufferDesc;
    Reserved2:        ULONG;
    phNewContext:     PCtxtHandle;
    pOutput:          PSecBufferDesc;
    pfContextAttr:    PULONG;
    ptsExpiry:        PTimeStamp
  ): SECURITY_STATUS; stdcall;

  TQueryContextAttributesA = function(
    phContext:    PCtxtHandle;
    ulAttribute:  ULONG;
    pBuffer:      Pointer
  ): SECURITY_STATUS; stdcall;

  TCertGetNameStringA = function(
    pCertContext:  PCCERT_CONTEXT;
    dwType:        DWORD;
    dwFlags:       DWORD;
    pvTypePara:    Pointer;
    pszNameString: PAnsiChar;
    cchNameString: DWORD
  ): DWORD; stdcall;

// ---------------------------------------------------------------------------
// SSPI function-pointer types – char-neutral (same on A and W)
// ---------------------------------------------------------------------------
type
  TFreeCredentialsHandle = function(
    phCredential: PCredHandle
  ): SECURITY_STATUS; stdcall;

  TAcceptSecurityContext = function(
    phCredential:  PCredHandle;
    phContext:     PCtxtHandle;
    pInput:        PSecBufferDesc;
    fContextReq:   ULONG;
    TargetDataRep: ULONG;
    phNewContext:  PCtxtHandle;
    pOutput:       PSecBufferDesc;
    pfContextAttr: PULONG;
    ptsExpiry:     PTimeStamp
  ): SECURITY_STATUS; stdcall;

  TDeleteSecurityContext = function(
    phContext: PCtxtHandle
  ): SECURITY_STATUS; stdcall;

  TApplyControlToken = function(
    phContext: PCtxtHandle;
    pInput:    PSecBufferDesc
  ): SECURITY_STATUS; stdcall;

  TFreeContextBuffer = function(
    pvContextBuffer: Pointer
  ): SECURITY_STATUS; stdcall;

  TEncryptMessage = function(
    phContext:    PCtxtHandle;
    fQOP:         ULONG;
    pMessage:     PSecBufferDesc;
    MessageSeqNo: ULONG
  ): SECURITY_STATUS; stdcall;

  TDecryptMessage = function(
    phContext:    PCtxtHandle;
    pMessage:     PSecBufferDesc;
    MessageSeqNo: ULONG;
    pfQOP:        PULONG
  ): SECURITY_STATUS; stdcall;

  TPFXImportCertStore = function(
    pPFX:       PCryptDataBlob;
    szPassword: PWideChar;        { always W – no A variant exists }
    dwFlags:    DWORD
  ): HCERTSTORE; stdcall;

  TCertEnumCertificatesInStore = function(
    hCertStore:       HCERTSTORE;
    pPrevCertContext: PCCERT_CONTEXT
  ): PCCERT_CONTEXT; stdcall;

  TCertFreeCertificateContext = function(
    pCertContext: PCCERT_CONTEXT
  ): BOOL; stdcall;

  TCertCloseStore = function(
    hCertStore: HCERTSTORE;
    dwFlags:    DWORD
  ): BOOL; stdcall;

  TCertDuplicateCertificateContext = function(
    pCertContext: PCCERT_CONTEXT
  ): PCCERT_CONTEXT; stdcall;

  TCryptHashCertificate = function(
    hCryptProv:      Pointer;   { HCRYPTPROV – pass nil for default }
    Algid:           DWORD;     { ALG_ID }
    dwFlags:         DWORD;
    pbEncoded:       PByte;
    cbEncoded:       DWORD;
    pbComputedHash:  PByte;
    pcbComputedHash: PDWORD
  ): BOOL; stdcall;

// ---------------------------------------------------------------------------
// Global function-pointer variables – suffixed (W and A)
// Plugin code uses the un-suffixed aliases defined below.
// ---------------------------------------------------------------------------
var
  { secur32.dll – W variants }
  SChannel_AcquireCredentialsHandleW  : TAcquireCredentialsHandleW;
  SChannel_InitializeSecurityContextW : TInitializeSecurityContextW;
  { secur32.dll – A variants }
  SChannel_AcquireCredentialsHandleA  : TAcquireCredentialsHandleA;
  SChannel_InitializeSecurityContextA : TInitializeSecurityContextA;
  { secur32.dll – char-neutral }
  SChannel_FreeCredentialsHandle      : TFreeCredentialsHandle;
  { QueryContextAttributes has no A/W variants – always loaded as W }
  SChannel_QueryContextAttributes     : TQueryContextAttributesW;
  SChannel_AcceptSecurityContext      : TAcceptSecurityContext;
  SChannel_DeleteSecurityContext      : TDeleteSecurityContext;
  SChannel_ApplyControlToken          : TApplyControlToken;
  SChannel_FreeContextBuffer          : TFreeContextBuffer;
  SChannel_EncryptMessage             : TEncryptMessage;
  SChannel_DecryptMessage             : TDecryptMessage;
  { crypt32.dll – W variants }
  Crypt32_CertGetNameStringW          : TCertGetNameStringW;
  { crypt32.dll – A variants }
  Crypt32_CertGetNameStringA          : TCertGetNameStringA;
  { crypt32.dll – char-neutral }
  Crypt32_PFXImportCertStore              : TPFXImportCertStore;
  { Points to W or A variant; assigned in InitSChannelInterface }
  Crypt32_CertGetNameString               : Pointer;
  Crypt32_CertEnumCertificatesInStore     : TCertEnumCertificatesInStore;
  Crypt32_CertFreeCertificateContext      : TCertFreeCertificateContext;
  Crypt32_CertCloseStore                  : TCertCloseStore;
  Crypt32_CertDuplicateCertificateContext : TCertDuplicateCertificateContext;
  Crypt32_CryptHashCertificate            : TCryptHashCertificate;

// ---------------------------------------------------------------------------
// Initialisation / de-initialisation
// ---------------------------------------------------------------------------
function  InitSChannelInterface: Boolean;
procedure DeInitSChannelInterface;

implementation

var
  _hSecur32    : HMODULE = 0;
  _hCrypt32    : HMODULE = 0;
  _Initialized : Boolean = False;
  _InitResult  : Boolean = False;


function InitSChannelInterface: Boolean;

  function Load(hLib: HMODULE; const Name: string): Pointer;
  begin
    Result := GetProcAddress(hLib, PChar(Name));
  end;

begin
  if _Initialized then begin
    Result := _InitResult;
    Exit;
  end;
  _Initialized := True;
  _InitResult  := False;

  _hSecur32 := LoadLibrary('secur32.dll');
  _hCrypt32  := LoadLibrary('crypt32.dll');
  if (_hSecur32 = 0) or (_hCrypt32 = 0) then begin
    Result := False;
    Exit;
  end;

  { --- secur32.dll – suffixed variants --- }
  SChannel_AcquireCredentialsHandleW  := Load(_hSecur32, 'AcquireCredentialsHandleW');
  SChannel_InitializeSecurityContextW := Load(_hSecur32, 'InitializeSecurityContextW');
  SChannel_AcquireCredentialsHandleA  := Load(_hSecur32, 'AcquireCredentialsHandleA');
  SChannel_InitializeSecurityContextA := Load(_hSecur32, 'InitializeSecurityContextA');
  { char-neutral – no A/W split }
  SChannel_QueryContextAttributes     := Load(_hSecur32, 'QueryContextAttributesW');
  SChannel_FreeCredentialsHandle  := Load(_hSecur32, 'FreeCredentialsHandle');
  SChannel_AcceptSecurityContext  := Load(_hSecur32, 'AcceptSecurityContext');
  SChannel_DeleteSecurityContext  := Load(_hSecur32, 'DeleteSecurityContext');
  SChannel_ApplyControlToken      := Load(_hSecur32, 'ApplyControlToken');
  SChannel_FreeContextBuffer      := Load(_hSecur32, 'FreeContextBuffer');
  SChannel_EncryptMessage         := Load(_hSecur32, 'EncryptMessage');
  SChannel_DecryptMessage         := Load(_hSecur32, 'DecryptMessage');

  { --- crypt32.dll --- }
  Crypt32_CertGetNameStringW              := Load(_hCrypt32, 'CertGetNameStringW');
  Crypt32_CertGetNameStringA              := Load(_hCrypt32, 'CertGetNameStringA');
  Crypt32_PFXImportCertStore              := Load(_hCrypt32, 'PFXImportCertStore');
  Crypt32_CertEnumCertificatesInStore     := Load(_hCrypt32, 'CertEnumCertificatesInStore');
  Crypt32_CertFreeCertificateContext      := Load(_hCrypt32, 'CertFreeCertificateContext');
  Crypt32_CertCloseStore                  := Load(_hCrypt32, 'CertCloseStore');
  Crypt32_CertDuplicateCertificateContext := Load(_hCrypt32, 'CertDuplicateCertificateContext');
  Crypt32_CryptHashCertificate            := Load(_hCrypt32, 'CryptHashCertificate');

  { Require: W or A variant of the char-sensitive functions (per build mode),
    plus all char-neutral functions, plus crypt32 basics. }
  _InitResult :=
    { char-sensitive – W on Unicode Delphi, A on ANSI Delphi }
{$IFDEF UNICODE}
    Assigned(SChannel_AcquireCredentialsHandleW)  and
    Assigned(SChannel_InitializeSecurityContextW) and
    Assigned(Crypt32_CertGetNameStringW)           and
{$ELSE}
    Assigned(SChannel_AcquireCredentialsHandleA)  and
    Assigned(SChannel_InitializeSecurityContextA) and
    Assigned(Crypt32_CertGetNameStringA)           and
{$ENDIF}
    { char-neutral – single entry point regardless of Delphi mode }
    Assigned(SChannel_QueryContextAttributes)      and
    Assigned(SChannel_FreeCredentialsHandle)        and
    Assigned(SChannel_AcceptSecurityContext)        and
    Assigned(SChannel_DeleteSecurityContext)        and
    Assigned(SChannel_FreeContextBuffer)            and
    Assigned(SChannel_EncryptMessage)               and
    Assigned(SChannel_DecryptMessage)               and
    Assigned(Crypt32_PFXImportCertStore)            and
    Assigned(Crypt32_CertEnumCertificatesInStore)   and
    Assigned(Crypt32_CertFreeCertificateContext)    and
    Assigned(Crypt32_CertCloseStore);

  { Point Crypt32_CertGetNameString to W or A variant }
{$IFDEF UNICODE}
  Crypt32_CertGetNameString := @Crypt32_CertGetNameStringW;
{$ELSE}
  Crypt32_CertGetNameString := @Crypt32_CertGetNameStringA;
{$ENDIF}

  Result := _InitResult;
end;

procedure DeInitSChannelInterface;
begin
  SChannel_AcquireCredentialsHandleW  := nil;
  SChannel_InitializeSecurityContextW := nil;
  SChannel_AcquireCredentialsHandleA  := nil;
  SChannel_InitializeSecurityContextA := nil;
  SChannel_QueryContextAttributes     := nil;
  SChannel_FreeCredentialsHandle      := nil;
  SChannel_AcceptSecurityContext      := nil;
  SChannel_DeleteSecurityContext      := nil;
  SChannel_ApplyControlToken          := nil;
  SChannel_FreeContextBuffer          := nil;
  SChannel_EncryptMessage             := nil;
  SChannel_DecryptMessage             := nil;
  Crypt32_CertGetNameStringW          := nil;
  Crypt32_CertGetNameStringA          := nil;
  Crypt32_PFXImportCertStore              := nil;
  Crypt32_CertEnumCertificatesInStore     := nil;
  Crypt32_CertFreeCertificateContext      := nil;
  Crypt32_CertCloseStore                  := nil;
  Crypt32_CertDuplicateCertificateContext := nil;
  Crypt32_CryptHashCertificate            := nil;
  Crypt32_CertGetNameString          := nil;

  if _hSecur32 <> 0 then begin FreeLibrary(_hSecur32); _hSecur32 := 0; end;
  if _hCrypt32  <> 0 then begin FreeLibrary(_hCrypt32); _hCrypt32  := 0; end;

  _Initialized := False;
  _InitResult  := False;
end;

initialization

finalization
  DeInitSChannelInterface;

end.
