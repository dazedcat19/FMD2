{==============================================================================|
| Project : Ararat Synapse                                       | 002.000.001 |
|==============================================================================|
| Content: SSL support by YuOpenSSL v1.x                                       |
|==============================================================================|
| Copyright (c)1999-2021, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2005-2021.                |
| Portions created by Petr Fejfar are Copyright (c)2011-2012.                  |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

//requires YuOpenSSL-1.x – OpenSSL for Delphi without DLLs

{:@abstract(SSL plugin for OpenSSL)

Compatibility with OpenSSL versions:
1.1.0
1.1.1

OpenSSL libraries are loaded dynamicly - you not need OpenSSL librares even you
compile your application with this unit. SSL just not working when you not have
OpenSSL libraries.

This plugin does not have support for .NET!

For handling keys and certificates you can use this properties:

@link(TCustomSSL.CertificateFile) for PEM or ASN1 DER (cer) format. @br
@link(TCustomSSL.Certificate) for ASN1 DER format only. @br
@link(TCustomSSL.PrivateKeyFile) for PEM or ASN1 DER (key) format. @br
@link(TCustomSSL.PrivateKey) for ASN1 DER format only. @br
@link(TCustomSSL.CertCAFile) for PEM CA certificate bundle. @br
@link(TCustomSSL.PFXFile) for PFX format. @br
@link(TCustomSSL.PFX) for PFX format from binary string. @br

This plugin is capable to create Ad-Hoc certificates. When you start SSL/TLS
server without explicitly assigned key and certificate, then this plugin create
Ad-Hoc key and certificate for each incomming connection by self. It slowdown
accepting of new connections!
}

{$INCLUDE 'jedi.inc'}

{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit ssl_yuopenssl11{$IFDEF SUPPORTS_DEPRECATED} deprecated{$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use ssl_yuopenssl3 with OpenSSL 3.x instead'{$ENDIF}{$ENDIF};

interface

uses
  SysUtils, Classes,
  {$IFDEF DELPHI23_UP} AnsiStrings, {$ENDIF}
  blcksock, synsock, synautil,
  YuC, YuOpenSSL;

type
  {:@abstract(class implementing OpenSSL SSL plugin.)
   Instance of this class will be created for each @link(TTCPBlockSocket).
   You not need to create instance of this class, all is done by Synapse itself!}
  TSSLOpenSSL = class(TCustomSSL)
  protected
    FSsl: SSL_ptr;
    Fctx: SSL_CTX_ptr;
    function SSLCheck: Boolean;
    function SetSslKeys: boolean;
    function Init(server:Boolean): Boolean;
    function DeInit: Boolean;
    function Prepare(server:Boolean): Boolean;
    function LoadPFX(pfxdata: ansistring): Boolean;
    function CreateSelfSignedCert(Host: string): Boolean; override;
  public
    {:See @inherited}
    constructor Create(const Value: TTCPBlockSocket); override;
    destructor Destroy; override;
    {:See @inherited}
    function LibVersion: String; override;
    {:See @inherited}
    function LibName: String; override;
    {:See @inherited and @link(ssl_cryptlib) for more details.}
    function Connect: boolean; override;
    {:See @inherited and @link(ssl_cryptlib) for more details.}
    function Accept: boolean; override;
    {:See @inherited}
    function Shutdown: boolean; override;
    {:See @inherited}
    function BiShutdown: boolean; override;
    {:See @inherited}
    function SendBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:See @inherited}
    function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:See @inherited}
    function WaitingData: Integer; override;
    {:See @inherited}
    function GetSSLVersion: string; override;
    {:See @inherited}
    function GetPeerSubject: string; override;
    {:See @inherited}
    function GetPeerSerialNo: integer; override; {pf}
    {:See @inherited}
    function GetPeerIssuer: string; override;
    {:See @inherited}
    function GetPeerName: string; override;
    {:See @inherited}
    function GetPeerNameHash: cardinal; override; {pf}
    {:See @inherited}
    function GetPeerFingerprint: AnsiString; override;
    {:See @inherited}
    function GetCertInfo: string; override;
    {:See @inherited}
    function GetCipherName: string; override;
    {:See @inherited}
    function GetCipherBits: integer; override;
    {:See @inherited}
    function GetCipherAlgBits: integer; override;
    {:See @inherited}
    function GetVerifyCert: integer; override;
  end;

implementation

{==============================================================================}

function PasswordCallback(buf:PAnsiChar; size:Integer; rwflag:Integer; userdata: Pointer):Integer;
var
  Password: AnsiString;
begin
  Password := '';
  if TCustomSSL(userdata) is TCustomSSL then
    Password := TCustomSSL(userdata).KeyPassword;
  if Length(Password) > (Size - 1) then
    SetLength(Password, Size - 1);
  Result := Length(Password);
  {$IFDEF DELPHI23_UP}AnsiStrings.{$ENDIF}StrLCopy(buf, PAnsiChar(Password + #0), Result + 1);
end;

procedure _X509Free(x: Pointer);
begin
  YuOpenSSL.X509_free(x);
end;

{==============================================================================}

constructor TSSLOpenSSL.Create(const Value: TTCPBlockSocket);
begin
  inherited Create(Value);
  FCiphers := 'DEFAULT';
  FSsl := nil;
  Fctx := nil;
end;

destructor TSSLOpenSSL.Destroy;
begin
  DeInit;
  inherited Destroy;
end;

function TSSLOpenSSL.LibVersion: String;
begin
  Result := OpenSSL_version(0);
end;

function TSSLOpenSSL.LibName: String;
begin
  Result := 'ssl_yuopenssl11';
end;

function TSSLOpenSSL.SSLCheck: Boolean;
var
  s: AnsiString;
begin
  Result := true;
  FLastErrorDesc := '';
  FLastError := ERR_get_error;
  ERR_clear_error;
  if FLastError <> 0 then
  begin
    Result := False;
    s := StringOfChar(#0, 256);
    ERR_error_string_n(FLastError, PAnsiChar(s), Length(s));
    SetLength(s, StrLen(PAnsiChar(s)));
    FLastErrorDesc := s;
  end;
end;

function TSSLOpenSSL.CreateSelfSignedCert(Host: string): Boolean;
var
  pk: EVP_PKEY_ptr;
  x: X509_ptr;
  rsa: RSA_ptr;
  t: asn1_string_st_ptr;
  name: X509_NAME_ptr;
  b: BIO_ptr;
  xn, y: integer;
  s: AnsiString;
begin
  Result := True;
  pk := EVP_PKEY_new;
  x := X509_New;
  try
    rsa := RSA_generate_key(2048, $10001, nil, nil);
    Evp_Pkey_Assign(pk, EVP_PKEY_RSA, rsa);
    X509_set_version(x, 2);
//    ASN1_INTEGER_set(X509_get_serialNumber(x), 0);
    ASN1_INTEGER_set(X509_get_serialNumber(x), GetTick);
    t := asn1_string_st_ptr(ASN1_UTCTIME_new);
    try
      X509_gmtime_adj(t, -60 * 60 *24);
      X509_set1_notBefore(x, t);
      X509_gmtime_adj(t, 60 * 60 * 60 *24);
      X509_set_notAfter(x, t);
    finally
      ASN1_UTCTIME_free(ASN1_UTCTIME_ptr(t));
    end;
    X509_set_pubkey(x, pk);
    Name := X509_get_subject_name(x);
    X509_NAME_add_entry_by_txt(Name, 'C', $1001, 'CZ', -1, -1, 0);
    X509_NAME_add_entry_by_txt(Name, 'CN', $1001, PAnsiChar(AnsiString(host)), -1, -1, 0);
    X509_set_issuer_name(x, Name);
    X509_sign(x, pk, EVP_get_digestbyname('SHA256'));
    b := BIO_new(BIO_s_mem);
    try
      i2d_X509_bio(b, x);
      xn := BIO_ctrl_pending(b);
      SetLength(s, xn);
      y := BIO_read(b, Pointer(s), xn);
      if y > 0 then
        SetLength(s, y);
    finally
      BIO_free_all(b);
    end;
    FCertificate := s;
    b := BIO_new(BIO_s_mem);
    try
      i2d_PrivateKey_bio(b, pk);
      xn := BIO_ctrl_pending(b);
      SetLength(s, xn);
      y := BIO_read(b, Pointer(s), xn);
      if y > 0 then
        SetLength(s, y);
    finally
      BIO_free_all(b);
    end;
    FPrivatekey := s;
  finally
    X509_free(x);
    EVP_PKEY_free(pk);
  end;
end;

function TSSLOpenSSL.LoadPFX(pfxdata: Ansistring): Boolean;
var
  cert, pkey, ca: Pointer;
  certx: PAnsiChar;
  b: Pointer; // YuOpenSSL.BIO_ptr;
  p12: Pointer;
  i: Integer;
  Store: X509_STORE_ptr;
  iTotal: Integer;
begin
  Result := False;
  b := BIO_new(BIO_s_mem);
  try
    BIO_write(b, Pointer(pfxdata), Length(PfxData));
    p12 := d2i_PKCS12_bio(b, nil);
    if not Assigned(p12) then
      Exit;
    try
      cert := nil;
      pkey := nil;
      ca := nil;
      try {pf}
        if PKCS12_parse(p12, PAnsiChar(AnsiString(FKeyPassword)), pkey, cert, ca) > 0 then
          if SSL_CTX_use_certificate(Fctx, cert) > 0 then
            if SSL_CTX_use_PrivateKey(Fctx, pkey) > 0 then
              Result := True;
      {pf}

         if Result and (ca <> nil) then
         begin
            iTotal := OPENSSL_sk_num(ca);
            if iTotal > 0 then
            begin
              Store := SSL_CTX_get_cert_store(Fctx);
              for I := 0 to iTotal - 1 do
              begin
                certx := OPENSSL_sk_value(ca, I);
                if certx <> nil then
                begin
                  if X509_STORE_add_cert(Store, X509_ptr(certx)) = 0  then
                  begin
                    // already exists
                  end;
                 //X509_free(Cert);
                end;
              end;
            end;
         end;
      finally
        EVP_PKEY_free(pkey);
        X509_free(cert);
        OPENSSL_sk_pop_free(ca,_X509Free); // for ca=nil a new STACK was allocated...
      end;
      {/pf}
    finally
      PKCS12_free(p12);
    end;
  finally
    BIO_free_all(b);
  end;
end;

function TSSLOpenSSL.SetSslKeys: boolean;
var
  st: TFileStream;
  s: ansistring;
begin
  Result := False;
  if not assigned(FCtx) then
    Exit;
  try

    if FCertificateFile <> '' then
    begin
      s := AnsiString(FCertificateFile);
      if SSL_CTX_use_certificate_chain_file(FCtx, PAnsiChar(s)) <> 1 then
        if SSL_CTX_use_certificate_file(FCtx, PAnsiChar(s), SSL_FILETYPE_PEM) <> 1 then
          if SSL_CTX_use_certificate_file(FCtx, PAnsiChar(s), SSL_FILETYPE_ASN1) <> 1 then
            Exit;
    end;
    if FCertificate <> '' then
    begin
      s := AnsiString(FCertificate);
      if SSL_CTX_use_certificate_ASN1(FCtx, length(FCertificate), PAnsiChar(s)) <> 1 then
        Exit;
    end;
    SSLCheck;
    if FPrivateKeyFile <> '' then
    begin
      s := AnsiString(FPrivateKeyFile);
      if SSL_CTX_use_RSAPrivateKey_file(FCtx, PAnsiChar(s), SSL_FILETYPE_PEM) <> 1 then
        if SSL_CTX_use_RSAPrivateKey_file(FCtx, PAnsiChar(s), SSL_FILETYPE_ASN1) <> 1 then
          Exit;
    end;
    if FPrivateKey <> '' then
    begin
      s := AnsiString(FPrivateKey);
      if SSL_CTX_use_PrivateKey_ASN1(EVP_PKEY_RSA, FCtx, PAnsiChar(s), length(FPrivateKey)) <> 1 then
        Exit;
    end;
    SSLCheck;
    if FCertCAFile <> '' then
    begin
      s := AnsiString(FCertCAFile);
      if SSL_CTX_load_verify_locations(FCtx, PAnsiChar(s), '') <> 1 then
        Exit;
    end;
    if FPFXfile <> '' then
    begin
      try
        st := TFileStream.Create(FPFXfile, fmOpenRead	 or fmShareDenyNone);
        try
          s := ReadStrFromStream(st, st.Size);
        finally
          st.Free;
        end;
        if not LoadPFX(s) then
          Exit;
      except
        on Exception do
          Exit;
      end;
    end;
    if FPFX <> '' then
      if not LoadPFX(FPfx) then
        Exit;
    SSLCheck;
    Result := True;
  finally
    SSLCheck;
  end;
end;

function TSSLOpenSSL.Init(server:Boolean): Boolean;
var
  s: AnsiString;
begin
  Result := False;
  FLastErrorDesc := '';
  FLastError := 0;
  Fctx := SSL_CTX_new(TLS_method); // best common protocol
  if Fctx = nil then
  begin
    SSLCheck;
    Exit;
  end
  else
  begin
    case FSSLType of
      LT_TLSv1:
        begin
          SSL_CTX_set_min_proto_version(Fctx, TLS1_VERSION);
          SSL_CTX_set_max_proto_version(Fctx, TLS1_VERSION);
        end;
      LT_TLSv1_1:
        begin
          SSL_CTX_set_min_proto_version(Fctx, TLS1_1_VERSION);
          SSL_CTX_set_max_proto_version(Fctx, TLS1_1_VERSION);
        end;
      LT_TLSv1_2:
        begin
          SSL_CTX_set_min_proto_version(Fctx, TLS1_2_VERSION);
          SSL_CTX_set_max_proto_version(Fctx, TLS1_2_VERSION);
        end;
      LT_TLSv1_3:
        begin
          SSL_CTX_set_min_proto_version(Fctx, TLS1_3_VERSION);
          SSL_CTX_set_max_proto_version(Fctx, TLS1_3_VERSION);
        end;
    end;
    s := AnsiString(FCiphers);
    SSL_CTX_set_cipher_list(Fctx, PAnsiChar(s));
    if FVerifyCert then
      SSL_CTX_set_verify(FCtx, SSL_VERIFY_PEER, nil)
    else
      SSL_CTX_set_verify(FCtx, SSL_VERIFY_NONE, nil);
    SSL_CTX_set_default_passwd_cb(FCtx, PasswordCallback);
    SSL_CTX_set_default_passwd_cb_userdata(FCtx, self);

    if server and (FCertificateFile = '') and (FCertificate = '')
      and (FPFXfile = '') and (FPFX = '') then
    begin
      CreateSelfSignedcert(FSocket.ResolveIPToName(FSocket.GetRemoteSinIP));
    end;

    if not SetSSLKeys then
      Exit
    else
    begin
      Fssl := nil;
      Fssl := SSL_new(Fctx);
      if Fssl = nil then
      begin
        SSLCheck;
        exit;
      end;
    end;
  end;
  Result := true;
end;

function TSSLOpenSSL.DeInit: Boolean;
begin
  Result := True;
  if assigned (Fssl) then
    SSL_free(Fssl);
  Fssl := nil;
  if assigned (Fctx) then
  begin
    SSL_CTX_free(Fctx);
    Fctx := nil;
  end;
  FSSLEnabled := False;
end;

function TSSLOpenSSL.Prepare(server:Boolean): Boolean;
begin
  Result := false;
  DeInit;
  if Init(server) then
    Result := true
  else
    DeInit;
end;

function TSSLOpenSSL.Connect: boolean;
var
  x: integer;
  b: boolean;
  err: integer;
  s: AnsiString;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare(False) then
  begin
    if SSL_set_fd(FSsl, FSocket.Socket) < 1 then
    begin
      SSLCheck;
      Exit;
    end;
    if SNIHost <> '' then
    begin
      s := AnsiString(SNIHost);
      SSL_ctrl(Fssl, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, PAnsiChar(s));
      SSL_set1_host(Fssl, PAnsiChar(s));
    end;
    if FSocket.ConnectionTimeout <= 0 then //do blocking call of SSL_Connect
    begin
      x := SSL_connect(FSsl);
      if x < 1 then
      begin
        SSLcheck;
        Exit;
      end;
    end
    else //do non-blocking call of SSL_Connect
    begin
      b := Fsocket.NonBlockMode;
      Fsocket.NonBlockMode := true;
      repeat
        x := SSL_connect(FSsl);
        err := SSL_get_error(FSsl, x);
        if err = SSL_ERROR_WANT_READ then
          if not FSocket.CanRead(FSocket.ConnectionTimeout) then
            break;
        if err = SSL_ERROR_WANT_WRITE then
          if not FSocket.CanWrite(FSocket.ConnectionTimeout) then
            break;
      until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
      Fsocket.NonBlockMode := b;
      if err <> SSL_ERROR_NONE then
      begin
        SSLcheck;
        Exit;
      end;
    end;
  if FverifyCert then
    if (GetVerifyCert <> 0) or (not DoVerifyCert) then
      Exit;
    FSSLEnabled := True;
    Result := True;
  end;
end;

function TSSLOpenSSL.Accept: boolean;
var
  x: integer;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then
    Exit;
  if Prepare(True) then
  begin
    if SSL_set_fd(FSsl, FSocket.Socket) < 1 then
    begin
      SSLCheck;
      Exit;
    end;
    x := SSL_accept(FSsl);
    if x < 1 then
    begin
      SSLcheck;
      Exit;
    end;
    FSSLEnabled := True;
    Result := True;
  end;
end;

function TSSLOpenSSL.Shutdown: boolean;
begin
  if assigned(FSsl) then
    SSL_shutdown(FSsl);
  DeInit;
  Result := True;
end;

function TSSLOpenSSL.BiShutdown: boolean;
var
  x: integer;
begin
  if assigned(FSsl) then
  begin
    x := SSL_shutdown(FSsl);
    if x = 0 then
    begin
      Synsock.Shutdown(FSocket.Socket, 1);
      SSL_shutdown(FSsl);
    end;
  end;
  DeInit;
  Result := True;
end;

function TSSLOpenSSL.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  err: integer;
begin
  FLastError := 0;
  FLastErrorDesc := '';
  repeat
    Result := SSL_write(FSsl, Buffer , Len);
    err := SSL_get_error(FSsl, Result);
  until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
  if err = SSL_ERROR_ZERO_RETURN then
    Result := 0
  else
    if (err <> 0) then
      FLastError := err;
end;

function TSSLOpenSSL.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  err: integer;
begin
  FLastError := 0;
  FLastErrorDesc := '';
  repeat
    Result := SSL_read(FSsl, Buffer , Len);
    err := SSL_get_error(FSsl, Result);
  until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
  if err = SSL_ERROR_ZERO_RETURN then
    Result := 0
  {pf}// Verze 1.1.0 byla s else tak jak to ted mam,
      // ve verzi 1.1.1 bylo ELSE zruseno, ale pak je SSL_ERROR_ZERO_RETURN
      // propagovano jako Chyba.
  {pf} else {/pf} if (err <> 0) then
    FLastError := err;
end;

function TSSLOpenSSL.WaitingData: Integer;
begin
  Result := SSL_pending(Fssl);
end;

function TSSLOpenSSL.GetSSLVersion: string;
begin
  if not assigned(FSsl) then
    Result := ''
  else
    Result := SSL_get_version(FSsl);
end;

function TSSLOpenSSL.GetPeerSubject: string;
var
  cert: Pointer;
  s: ansistring;
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSL_get_peer_certificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
  SetLength(s, 4096);
  Result := X509_NAME_oneline(X509_get_subject_name(cert), PAnsiChar(s), Length(s));
  X509_free(cert);
end;

function TSSLOpenSSL.GetPeerSerialNo: integer; {pf}
var
  cert: Pointer;
  SN:   ASN1_INTEGER_ptr;
begin
  if not assigned(FSsl) then
  begin
    Result := -1;
    Exit;
  end;
  cert := SSL_get_peer_certificate(Fssl);
  try
    if not assigned(cert) then
    begin
      Result := -1;
      Exit;
    end;
    SN := X509_get_serialNumber(cert);
    Result := ASN1_INTEGER_get(SN);
  finally
    X509_free(cert);
  end;
end;

function TSSLOpenSSL.GetPeerName: string;
var
  s: ansistring;
begin
  s := GetPeerSubject;
  s := SeparateRight(s, '/CN=');
  Result := Trim(SeparateLeft(s, '/'));
end;

function TSSLOpenSSL.GetPeerNameHash: cardinal; {pf}
var
  cert: Pointer;
begin
  if not assigned(FSsl) then
  begin
    Result := 0;
    Exit;
  end;
  cert := SSL_get_peer_certificate(Fssl);
  try
    if not assigned(cert) then
    begin
      Result := 0;
      Exit;
    end;
    Result := X509_NAME_hash(X509_get_subject_name(cert));
  finally
    X509_free(cert);
  end;
end;

function TSSLOpenSSL.GetPeerIssuer: string;
var
  cert: Pointer;
  s: ansistring;
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSL_get_peer_certificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
  SetLength(s, 4096);
  Result := X509_NAME_oneline(X509_get_issuer_name(cert), PAnsiChar(s), Length(s));
  X509_free(cert);
end;

function TSSLOpenSSL.GetPeerFingerprint: AnsiString;
var
  cert: Pointer;
  x: integer;
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSL_get_peer_certificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, EVP_MAX_MD_SIZE);
  X509_digest(cert, EVP_get_digestbyname('SHA1'), PAnsiChar(Result), @x); //was MD5 before
  SetLength(Result, x);
  X509_free(cert);
end;

function TSSLOpenSSL.GetCertInfo: string;
var
  cert: Pointer;
  x, y: integer;
  b: Pointer;
  s: AnsiString;
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSL_get_peer_certificate(Fssl);
  if not assigned(cert) then
  begin
    Result := '';
    Exit;
  end;
  try {pf}
    b := BIO_new(BIO_s_mem);
    try
      X509_print(b, cert);
      x := BIO_ctrl_pending(b);
      SetLength(s,x);
      y := BIO_read(b,Pointer(s),x);
      if y > 0 then
        SetLength(s, y);
      Result := ReplaceString(s, LF, CRLF);
    finally
      BIO_free_all(b);
    end;
  {pf}
  finally
    X509_free(cert);
  end;
  {/pf}
end;

function TSSLOpenSSL.GetCipherName: string;
begin
  if not assigned(FSsl) then
    Result := ''
  else
    Result := SSL_CIPHER_get_name(SSL_get_current_cipher(FSsl));
end;

function TSSLOpenSSL.GetCipherBits: integer;
var
  x: integer;
begin
  if not assigned(FSsl) then
    Result := 0
  else
    Result := SSL_CIPHER_get_bits(SSL_get_current_cipher(FSsl), @x);
end;

function TSSLOpenSSL.GetCipherAlgBits: integer;
begin
  if not assigned(FSsl) then
    Result := 0
  else
    SSL_CIPHER_get_bits(SSL_get_current_cipher(FSsl), @Result);
end;

function TSSLOpenSSL.GetVerifyCert: integer;
begin
  if not assigned(FSsl) then
    Result := 1
  else
    Result := SSL_get_verify_result(FSsl);
end;

{==============================================================================}

initialization
  SSLImplementation := TSSLOpenSSL;

end.
