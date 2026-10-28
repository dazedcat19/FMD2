{==============================================================================|
| Project : Ararat Synapse                                                     |
|==============================================================================|
| Content: SSL plugin for Windows SChannel (SSPI)                              |
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
|   Synapse TCustomSSL plugin that uses the Windows built-in SChannel TLS      |
|   implementation via the SSPI interface.  No third-party DLLs are required. |
|                                                                              |
|   All Windows API declarations live in ssl_schannel_lib.pas; this unit       |
|   contains only the plugin class and its logic.                              |
|                                                                              |
|   Supported features:                                                        |
|     Client mode      - TLS 1.0 / 1.1 / 1.2 / 1.3 (as supported by the OS)  |
|     Server mode      - requires a certificate supplied via PFXFile or        |
|                        PrivateKeyFile (both treated as PKCS#12 / PFX)        |
|     SNI              - honours the SNIHost property                          |
|     Cert validation  - delegates to the Windows certificate chain engine     |
|     Certificate info - subject, issuer, common name, TLS version, cipher     |
|     Client cert      - mutual TLS via PFXFile/CertificateFile                |
|     EOF signalling   - FEOF flag set on close_notify and TCP close           |
|                                                                              |
|   Usage:                                                                     |
|     Add ssl_schannel to your project's uses clause.                          |
|     The unit's initialization section registers TSSLSChannel automatically.  |
|                                                                              |
|   Server-mode certificate setup:                                             |
|     SSL.PFXFile     := 'server.pfx';                                         |
|     SSL.KeyPassword := 'secret';                                             |
|     (or use SSL.PrivateKeyFile with a PFX file if PFXFile is empty)          |
|==============================================================================|
| History:                                                                     |
|   2026  Initial version                                                      |
|==============================================================================}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
{$WARN SYMBOL_PLATFORM OFF}

unit ssl_schannel;

interface

{$IFNDEF MSWINDOWS}
  {$ERROR This unit is Windows-only (requires SChannel / SSPI).}
{$ENDIF}

uses
  Windows, SysUtils, Classes,
  blcksock, synsock, synautil,
  ssl_schannel_lib;

type
  {:@abstract(SSL plugin class implementing TCustomSSL via Windows SChannel.)
    An instance is created automatically for each TTCPBlockSocket when this
    unit is in the uses clause.  You do not need to instantiate it yourself.}
  TSSLSChannel = class(TCustomSSL)
  private
    FCredHandle   : TCredHandle;
    FCtxtHandle   : TCtxtHandle;
    FCredValid    : Boolean;
    FCtxtValid    : Boolean;
    FStreamSizes  : TSecPkgContext_StreamSizes;
    FSizesValid   : Boolean;

    { Timeout in ms for blocking data reads in RecvBuffer.
      -1 = block indefinitely (same behaviour as OpenSSL plugin).
      Can be changed at any time via the DataTimeout property. }
    FDataTimeout  : Integer;

    { Cached connection info – populated immediately after handshake }
    FConnInfo     : TSecPkgContext_ConnectionInfo;
    FConnInfoValid: Boolean;
    { Cached key info – fallback for older Windows where CONNECTION_INFO fails }
    FKeyInfo      : TSecPkgContext_KeyInfo;
    FKeyInfoValid : Boolean;

    { Undecrypted bytes received from the socket but not yet processed }
    FRawBuf       : AnsiString;
    { Decrypted application bytes waiting to be returned by RecvBuffer }
    FPlainBuf     : AnsiString;

    { Server certificate context loaded from the PFX file }
    FServerCert       : PCCERT_CONTEXT;
    { In-memory certificate store created from the PFX - kept alive while the
      TLS context is active so the certificate chain remains accessible }
    FPFXStore         : HCERTSTORE;

    { Client certificate context for mutual TLS (mTLS) }
    FClientCert       : PCCERT_CONTEXT;
    { Certificate store that owns FClientCert }
    FClientCertStore  : HCERTSTORE;

    { ---- internal helpers ---- }
    procedure ClearHandles;
    procedure FreeServerCert;

    { Returns True when Status indicates a fatal SSPI error.
      Populates FLastError / FLastErrorDesc on failure. }
    function  CheckSSPI(Status: SECURITY_STATUS): Boolean;

    { Load the first certificate from FPFXFile (falls back to FPrivateKeyFile) }
    function  LoadServerCertFromPFX: Boolean;
    { Load client certificate for mTLS from CertificateFile / Certificate / PFXFile }
    function  LoadClientCert: Boolean;
    { Free FClientCert and FClientCertStore }
    procedure FreeClientCert;

    { Send all bytes in Data through the raw (unencrypted) socket }
    function  SendRaw(const Data: AnsiString): Boolean;

    { Receive up to one socket-buffer's worth of raw data into FRawBuf.
      TimeoutMs is passed to TTCPBlockSocket.CanRead. }
    function  ReadRaw(TimeoutMs: Integer): Boolean;

    { Encrypt Len bytes from Buffer and send them as one or more TLS records }
    function  EncryptAndSend(Buffer: Pointer; Len: Integer): Integer;

    { Attempt to decrypt everything currently in FRawBuf into FPlainBuf.
      Returns True if at least one byte was added to FPlainBuf. }
    function  DecryptAvailable: Boolean;

    { TLS handshake - client side }
    function  DoClientHandshake: Boolean;
    { TLS handshake - server side }
    function  DoServerHandshake: Boolean;

    { Retrieve the peer certificate context; caller must free the result with
      Crypt32_CertFreeCertificateContext when done. }
    function  GetPeerCertContext: PCCERT_CONTEXT;

  public
    function GetSSLVersion      : string;   override;
    function GetPeerSubject     : string;   override;
    function GetPeerIssuer      : string;   override;
    function GetPeerName        : string;   override;
    function GetPeerSerialNo    : integer;  override;
    function GetPeerNameHash    : cardinal; override;
    function GetPeerFingerprint : string;   override;
    function GetCertInfo        : string;   override;
    function GetCipherName      : string;   override;
    function GetCipherBits      : integer;  override;
    function GetCipherAlgBits   : integer;  override;
    function GetVerifyCert      : integer;  override;

    constructor Create(const Value: TTCPBlockSocket); override;
    destructor  Destroy; override;

    {:Returns the library version string ("Windows SChannel").}
    function LibVersion  : String;  override;
    {:Returns the library name tag ("ssl_schannel").}
    function LibName     : String;  override;

    {:Perform a TLS client handshake on the already-connected socket.}
    function Connect     : boolean; override;
    {:Perform a TLS server handshake on the already-accepted socket.}
    function Accept      : boolean; override;
    {:Send a TLS close_notify alert and tear down the security context.}
    function Shutdown    : boolean; override;
    {:Identical to Shutdown for SChannel (bidirectional shutdown is handled
      internally by the close_notify sequence).}
    function BiShutdown  : boolean; override;

    {:Encrypt and transmit Len bytes from Buffer.}
    function SendBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:Receive and decrypt up to Len bytes into Buffer.}
    function RecvBuffer(Buffer: TMemory; Len: Integer): Integer; override;
    {:Return the number of decrypted bytes currently waiting in the internal
      buffer (does NOT read from the socket).}
    function WaitingData     : Integer; override;
    function ImplementsEOF   : boolean; override;

    {:Timeout in milliseconds for blocking reads during data transfer
     (i.e. inside RecvBuffer). Use -1 to block indefinitely, which matches
     the behaviour of the OpenSSL plugin and is the default.
     The handshake always uses its own fixed timeout (HANDSHAKE_TIMEOUT_MS).}
    property DataTimeout: Integer read FDataTimeout write FDataTimeout;
  end;

implementation

const
  { Timeout in milliseconds for each read during TLS handshake.
    FSocket.ConnectionTimeout defaults to 0 which means no waiting,
    so we use a fixed value here. }
  HANDSHAKE_TIMEOUT_MS = 30000;

// ===========================================================================
// Helpers
// ===========================================================================

procedure TSSLSChannel.ClearHandles;
begin
  if FCtxtValid then begin
    SChannel_DeleteSecurityContext(@FCtxtHandle);
    FCtxtValid := False;
  end;
  if FCredValid then begin
    SChannel_FreeCredentialsHandle(@FCredHandle);
    FCredValid := False;
  end;
  FSizesValid    := False;
  FConnInfoValid := False;
  FKeyInfoValid  := False;
  if FKeyInfo.sSignatureAlgorithmName <> nil then begin
    SChannel_FreeContextBuffer(FKeyInfo.sSignatureAlgorithmName);
    FKeyInfo.sSignatureAlgorithmName := nil;
  end;
  if FKeyInfo.sEncryptAlgorithmName <> nil then begin
    SChannel_FreeContextBuffer(FKeyInfo.sEncryptAlgorithmName);
    FKeyInfo.sEncryptAlgorithmName := nil;
  end;
  FillChar(FConnInfo, SizeOf(FConnInfo), 0);
  FillChar(FKeyInfo,  SizeOf(FKeyInfo),  0);
  FRawBuf   := '';
  FPlainBuf := '';
end;

procedure TSSLSChannel.FreeServerCert;
begin
  if Assigned(FServerCert) then begin
    Crypt32_CertFreeCertificateContext(FServerCert);
    FServerCert := nil;
  end;
  if Assigned(FPFXStore) then begin
    Crypt32_CertCloseStore(FPFXStore, CERT_CLOSE_STORE_FORCE_FLAG);
    FPFXStore := nil;
  end;
end;

procedure TSSLSChannel.FreeClientCert;
begin
  if Assigned(FClientCert) then begin
    Crypt32_CertFreeCertificateContext(FClientCert);
    FClientCert := nil;
  end;
  if Assigned(FClientCertStore) then begin
    Crypt32_CertCloseStore(FClientCertStore, CERT_CLOSE_STORE_FORCE_FLAG);
    FClientCertStore := nil;
  end;
end;

function TSSLSChannel.LoadClientCert: Boolean;
{ Load a client certificate for mutual TLS.
  Priority:
    1. FPFXFile / FPfx  (PKCS#12 binary)
    2. FCertificateFile (also treated as PFX for SChannel)
  Sets FClientCert + FClientCertStore; returns True on success. }
var
  PFXPath : string;
  RawData : AnsiString;
  Blob    : TCryptDataBlob;
  WPass   : WideString;
  Stream  : TFileStream;
begin
  Result := False;
  FreeClientCert;

  { Prefer explicit PFX properties, fall back to CertificateFile }
  PFXPath := FPFXFile;
  if PFXPath = '' then PFXPath := FCertificateFile;
  if PFXPath = '' then begin
    if FPFX <> '' then begin
      { In-memory PFX blob supplied via the PFX property }
      Blob.cbData := Length(FPFX);
      Blob.pbData := PByte(PAnsiChar(FPFX));
      WPass := WideString(FKeyPassword);
      FClientCertStore := Crypt32_PFXImportCertStore(
        @Blob, PWideChar(WPass),
        PKCS12_ALLOW_OVERWRITE_KEY or PKCS12_NO_PERSIST_KEY or CRYPT_EXPORTABLE);
      if Assigned(FClientCertStore) then
        FClientCert := Crypt32_CertEnumCertificatesInStore(FClientCertStore, nil);
      Result := Assigned(FClientCert);
      Exit;
    end;
    Exit;  { no client certificate configured }
  end;

  try
    Stream := TFileStream.Create(PFXPath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(RawData, Stream.Size);
      Stream.ReadBuffer(RawData[1], Stream.Size);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do begin
      FLastError     := -1;
      FLastErrorDesc := 'SChannel client cert: cannot read file "' +
                        PFXPath + '": ' + E.Message;
      Exit;
    end;
  end;

  Blob.cbData := Length(RawData);
  Blob.pbData := PByte(PAnsiChar(RawData));
  WPass := WideString(FKeyPassword);

  FClientCertStore := Crypt32_PFXImportCertStore(
    @Blob, PWideChar(WPass),
    PKCS12_ALLOW_OVERWRITE_KEY or PKCS12_NO_PERSIST_KEY or CRYPT_EXPORTABLE);

  if not Assigned(FClientCertStore) then begin
    FLastError     := Integer(GetLastError);
    FLastErrorDesc := 'SChannel client cert: PFXImportCertStore failed: ' +
                      SysErrorMessage(GetLastError);
    Exit;
  end;

  FClientCert := Crypt32_CertEnumCertificatesInStore(FClientCertStore, nil);
  Result := Assigned(FClientCert);
end;

function TSSLSChannel.CheckSSPI(Status: SECURITY_STATUS): Boolean;
{ Returns True when Status is a fatal error (i.e. not one of the expected
  success / informational / "need more data" codes). }
begin
  Result := (Status <> SEC_E_OK)                  and
            (Status <> SEC_I_CONTINUE_NEEDED)      and
            (Status <> SEC_I_COMPLETE_NEEDED)       and
            (Status <> SEC_I_COMPLETE_AND_CONTINUE) and
            (Status <> SEC_E_INCOMPLETE_MESSAGE);
  if Result then begin
    FLastError     := Integer(Status);
    FLastErrorDesc := SysErrorMessage(DWORD(Status));
  end;
end;

function TSSLSChannel.LoadServerCertFromPFX: Boolean;
var
  PFXPath : string;
  RawData : AnsiString;
  Blob    : TCryptDataBlob;
  WPass   : WideString;
  Stream  : TFileStream;
begin
  Result := False;
  FreeServerCert;

  PFXPath := FPFXFile;
  if PFXPath = '' then
    PFXPath := FPrivateKeyFile;
  if PFXPath = '' then begin
    FLastError     := -1;
    FLastErrorDesc := 'SChannel server mode: no PFX certificate file specified. ' +
                      'Set PFXFile (and KeyPassword) or PrivateKeyFile.';
    Exit;
  end;

  try
    Stream := TFileStream.Create(PFXPath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(RawData, Stream.Size);
      Stream.ReadBuffer(RawData[1], Stream.Size);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do begin
      FLastError     := -1;
      FLastErrorDesc := 'SChannel: cannot read PFX file "' + PFXPath + '": ' + E.Message;
      Exit;
    end;
  end;

  Blob.cbData := Length(RawData);
  Blob.pbData := PByte(PAnsiChar(RawData));
  WPass := WideString(FKeyPassword);

  FPFXStore := Crypt32_PFXImportCertStore(
    @Blob, PWideChar(WPass),
    PKCS12_ALLOW_OVERWRITE_KEY or PKCS12_NO_PERSIST_KEY or CRYPT_EXPORTABLE);

  if not Assigned(FPFXStore) then begin
    FLastError     := Integer(GetLastError);
    FLastErrorDesc := 'SChannel: PFXImportCertStore failed (wrong password?): ' +
                      SysErrorMessage(GetLastError);
    Exit;
  end;

  { Pick the first (and usually only) certificate in the store }
  FServerCert := Crypt32_CertEnumCertificatesInStore(FPFXStore, nil);
  Result := Assigned(FServerCert);
  if not Result then begin
    FLastError     := -1;
    FLastErrorDesc := 'SChannel: PFX store contains no certificates.';
  end;
end;

// ===========================================================================
// Low-level send / receive
// ===========================================================================

function TSSLSChannel.SendRaw(const Data: AnsiString): Boolean;
var
  Total, Sent: Integer;
begin
  Result := False;
  Total  := 0;
  while Total < Length(Data) do begin
    Sent := synsock.Send(FSocket.Socket,
                         Pointer(PAnsiChar(Data) + Total),
                         Length(Data) - Total, 0);
    if Sent <= 0 then Exit;
    Inc(Total, Sent);
  end;
  Result := True;
end;

function TSSLSChannel.ReadRaw(TimeoutMs: Integer): Boolean;
{ Read all currently available data from the socket into FRawBuf.
  Waits up to TimeoutMs for the first byte, then drains everything that
  arrives within a short inter-segment window (20 ms).
  TLS handshake messages often arrive in multiple TCP segments; reading
  only one Recv worth of data per call causes SEC_E_INCOMPLETE_MESSAGE
  loops that stall until ConnectionTimeout fires. }
var
  Buf    : array[0..16383] of Byte;
  n      : Integer;
  OldLen : Integer;
begin
  Result := False;
  { Wait for at least one byte with the caller-supplied timeout }
  if not FSocket.CanRead(TimeoutMs) then Exit;
  repeat
    n := synsock.Recv(FSocket.Socket, Pointer(@Buf[0]), SizeOf(Buf), 0);
    if n > 0 then begin
      OldLen := Length(FRawBuf);
      SetLength(FRawBuf, OldLen + n);
      Move(Buf[0], FRawBuf[OldLen + 1], n);
      Result := True;
    end else
      Break;
    { Keep draining: give the next segment a short window to arrive }
  until not FSocket.CanRead(20);
end;

// ===========================================================================
// TLS client handshake
// ===========================================================================

function PWideCharOrNil(const s: WideString): PWideChar;
{ Returns nil when s is empty (suppresses SNI), or a pointer to the wide
  string data when s is non-empty (sends SNI in the TLS ClientHello). }
begin
  if s = '' then Result := nil
  else Result := PWideChar(s);
end;

function TSSLSChannel.DoClientHandshake: Boolean;
var
  Cred      : TSCHANNEL_CRED;
  ts        : TTimeStamp;
  Status    : SECURITY_STATUS;
  InBufs    : array[0..1] of TSecBuffer;
  OutBufs   : array[0..0] of TSecBuffer;
  InDesc    : TSecBufferDesc;
  OutDesc   : TSecBufferDesc;
  CtxAttr   : ULONG;
  WTarget   : WideString;
  Token     : AnsiString;
  FirstCall : Boolean;
begin
  Result := False;

  { Build SChannel credential }
  FillChar(Cred, SizeOf(Cred), 0);
  Cred.dwVersion := SCHANNEL_CRED_VERSION;
  Cred.grbitEnabledProtocols :=
    SP_PROT_TLS1_CLIENT   or SP_PROT_TLS1_1_CLIENT or
    SP_PROT_TLS1_2_CLIENT or SP_PROT_TLS1_3_CLIENT;

  if FVerifyCert then
    Cred.dwFlags := SCH_CRED_AUTO_CRED_VALIDATION
  else
    Cred.dwFlags := SCH_CRED_MANUAL_CRED_VALIDATION or
                    SCH_CRED_NO_SERVERNAME_CHECK;

  { Optionally attach a client certificate for mutual TLS.
    LoadClientCert returns False (and leaves FClientCert=nil) when no
    client certificate is configured – that is perfectly normal for plain TLS. }
  if (FCertificateFile <> '') or (FCertificate <> '') or
     (FPFXFile <> '')         or (FPFX <> '') then
    LoadClientCert;  { failure is non-fatal for client-side handshake }

  if Assigned(FClientCert) then begin
    Cred.cCreds := 1;
    Cred.paCred := @FClientCert;
  end;

  { AcquireCredentialsHandle – always use W variant directly;
    the package name string must be wide on all Delphi versions }
  Status := SChannel_AcquireCredentialsHandleW(
    nil, UNISP_NAME_W, SECPKG_CRED_OUTBOUND,
    nil, @Cred, nil, nil, @FCredHandle, @ts);
  if CheckSSPI(Status) then Exit;
  FCredValid := True;

  { SNI / target name:
      SNIHost set   → send that name in the TLS SNI extension
      SNIHost empty → pass nil, no SNI extension is sent to the server
    pszTargetName is also used by SChannel for certificate verification and
    session caching; when nil, certificate CN/SAN checking is skipped
    (the FVerifyCert flag already controls whether we care about that). }
  if SNIHost <> '' then
    WTarget := WideString(SNIHost)
  else
    WTarget := '';   { empty = no SNI }

  FirstCall := True;

  repeat
    { Output token buffer – SChannel allocates the memory via ISC_REQ_ALLOCATE_MEMORY }
    OutBufs[0].BufferType := SECBUFFER_TOKEN;
    OutBufs[0].cbBuffer   := 0;
    OutBufs[0].pvBuffer   := nil;
    OutDesc.ulVersion := SECBUFFER_VERSION;
    OutDesc.cBuffers  := 1;
    OutDesc.pBuffers  := @OutBufs[0];

    if FirstCall then begin
      { First call: no existing context, no input buffers }
      Status := SChannel_InitializeSecurityContextW(
        @FCredHandle, nil,
        { nil = no SNI; non-nil = send SNI and use for cert verification }
        { nil suppresses SNI; non-nil sends SNI and enables cert name check }
        PWideCharOrNil(WTarget),
        ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or
        ISC_REQ_CONFIDENTIALITY or ISC_REQ_EXTENDED_ERROR or
        ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM,
        0, 0, nil, 0,
        @FCtxtHandle, @OutDesc, @CtxAttr, @ts);
      FCtxtValid := True;
      FirstCall  := False;
    end else begin
      { Subsequent calls: pass all accumulated raw data to SChannel }
      InBufs[0].BufferType := SECBUFFER_TOKEN;
      InBufs[0].cbBuffer   := Length(FRawBuf);
      InBufs[0].pvBuffer   := PAnsiChar(FRawBuf);
      InBufs[1].BufferType := SECBUFFER_EMPTY;
      InBufs[1].cbBuffer   := 0;
      InBufs[1].pvBuffer   := nil;
      InDesc.ulVersion := SECBUFFER_VERSION;
      InDesc.cBuffers  := 2;
      InDesc.pBuffers  := @InBufs[0];

      Status := SChannel_InitializeSecurityContextW(
        @FCredHandle, @FCtxtHandle,
        PWideCharOrNil(WTarget),
        ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or
        ISC_REQ_CONFIDENTIALITY or ISC_REQ_EXTENDED_ERROR or
        ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM,
        0, 0, @InDesc, 0,
        @FCtxtHandle, @OutDesc, @CtxAttr, @ts);

      { SECBUFFER_EXTRA – bytes SChannel did not consume; keep for next round }
      if InBufs[1].BufferType = SECBUFFER_EXTRA then
        FRawBuf := Copy(FRawBuf,
                        Length(FRawBuf) - Integer(InBufs[1].cbBuffer) + 1,
                        InBufs[1].cbBuffer)
      else
        FRawBuf := '';
    end;

    { Forward any output token to the server before checking status }
    if (OutBufs[0].cbBuffer > 0) and Assigned(OutBufs[0].pvBuffer) then begin
      SetString(Token, PAnsiChar(OutBufs[0].pvBuffer), OutBufs[0].cbBuffer);
      SChannel_FreeContextBuffer(OutBufs[0].pvBuffer);
      OutBufs[0].pvBuffer := nil;
      if not SendRaw(Token) then Exit;
    end;

    if Status = SEC_E_INCOMPLETE_MESSAGE then begin
      { Not enough data yet – read more from socket and retry }
      if not ReadRaw(HANDSHAKE_TIMEOUT_MS) then Exit;
      Continue;
    end;

    if CheckSSPI(Status) then Exit;

    { SEC_I_CONTINUE_NEEDED: we sent our token, now read server response }
    if Status = SEC_I_CONTINUE_NEEDED then
      if not ReadRaw(HANDSHAKE_TIMEOUT_MS) then Exit;

  until Status = SEC_E_OK;

  { Query stream framing sizes needed by EncryptAndSend }
  Status := SChannel_QueryContextAttributes(
    @FCtxtHandle, SECPKG_ATTR_STREAM_SIZES, @FStreamSizes);
  if CheckSSPI(Status) then Exit;
  FSizesValid := True;

  { Cache connection info immediately after handshake }
  FConnInfoValid := SChannel_QueryContextAttributes(
    @FCtxtHandle, SECPKG_ATTR_CONNECTION_INFO, @FConnInfo) = SEC_E_OK;
  if not FConnInfoValid then
    FKeyInfoValid := SChannel_QueryContextAttributes(
      @FCtxtHandle, SECPKG_ATTR_KEY_INFO, @FKeyInfo) = SEC_E_OK;

  FEOF        := False;
  FSSLEnabled := True;
  Result      := True;
end;

// ===========================================================================
// TLS server handshake
// ===========================================================================

function TSSLSChannel.DoServerHandshake: Boolean;
var
  Cred      : TSCHANNEL_CRED;
  ts        : TTimeStamp;
  Status    : SECURITY_STATUS;
  InBufs    : array[0..1] of TSecBuffer;
  OutBufs   : array[0..0] of TSecBuffer;
  InDesc    : TSecBufferDesc;
  OutDesc   : TSecBufferDesc;
  CtxAttr   : ULONG;
  Token     : AnsiString;
  FirstCall : Boolean;
begin
  Result := False;

  if not Assigned(FServerCert) then
    if not LoadServerCertFromPFX then Exit;

  FillChar(Cred, SizeOf(Cred), 0);
  Cred.dwVersion := SCHANNEL_CRED_VERSION;
  Cred.cCreds    := 1;
  Cred.paCred    := @FServerCert;
  Cred.grbitEnabledProtocols :=
    SP_PROT_TLS1_SERVER   or SP_PROT_TLS1_1_SERVER or
    SP_PROT_TLS1_2_SERVER or SP_PROT_TLS1_3_SERVER;
  Cred.dwFlags := SCH_CRED_NO_DEFAULT_CREDS;

  Status := SChannel_AcquireCredentialsHandleW(
    nil, UNISP_NAME_W, SECPKG_CRED_INBOUND,
    nil, @Cred, nil, nil, @FCredHandle, @ts);
  if CheckSSPI(Status) then Exit;
  FCredValid := True;

  { Read the initial ClientHello }
  if not ReadRaw(HANDSHAKE_TIMEOUT_MS) then Exit;

  FirstCall := True;

  repeat
    InBufs[0].BufferType := SECBUFFER_TOKEN;
    InBufs[0].cbBuffer   := Length(FRawBuf);
    InBufs[0].pvBuffer   := PAnsiChar(FRawBuf);
    InBufs[1].BufferType := SECBUFFER_EMPTY;
    InBufs[1].cbBuffer   := 0;
    InBufs[1].pvBuffer   := nil;
    InDesc.ulVersion := SECBUFFER_VERSION;
    InDesc.cBuffers  := 2;
    InDesc.pBuffers  := @InBufs[0];

    OutBufs[0].BufferType := SECBUFFER_TOKEN;
    OutBufs[0].cbBuffer   := 0;
    OutBufs[0].pvBuffer   := nil;
    OutDesc.ulVersion := SECBUFFER_VERSION;
    OutDesc.cBuffers  := 1;
    OutDesc.pBuffers  := @OutBufs[0];

    if FirstCall then begin
      Status := SChannel_AcceptSecurityContext(
        @FCredHandle, nil, @InDesc,
        ASC_REQ_SEQUENCE_DETECT or ASC_REQ_REPLAY_DETECT or
        ASC_REQ_CONFIDENTIALITY or ASC_REQ_EXTENDED_ERROR or
        ASC_REQ_ALLOCATE_MEMORY or ASC_REQ_STREAM,
        0, @FCtxtHandle, @OutDesc, @CtxAttr, @ts);
      FCtxtValid := True;
      FirstCall  := False;
    end else begin
      Status := SChannel_AcceptSecurityContext(
        @FCredHandle, @FCtxtHandle, @InDesc,
        ASC_REQ_SEQUENCE_DETECT or ASC_REQ_REPLAY_DETECT or
        ASC_REQ_CONFIDENTIALITY or ASC_REQ_EXTENDED_ERROR or
        ASC_REQ_ALLOCATE_MEMORY or ASC_REQ_STREAM,
        0, @FCtxtHandle, @OutDesc, @CtxAttr, @ts);
    end;

    { Preserve any leftover bytes not consumed by this handshake step }
    if InBufs[1].BufferType = SECBUFFER_EXTRA then
      FRawBuf := Copy(FRawBuf,
                      Length(FRawBuf) - Integer(InBufs[1].cbBuffer) + 1,
                      InBufs[1].cbBuffer)
    else
      FRawBuf := '';

    { Forward the server's handshake token to the client }
    if (OutBufs[0].cbBuffer > 0) and Assigned(OutBufs[0].pvBuffer) then begin
      SetString(Token, PAnsiChar(OutBufs[0].pvBuffer), OutBufs[0].cbBuffer);
      SChannel_FreeContextBuffer(OutBufs[0].pvBuffer);
      OutBufs[0].pvBuffer := nil;
      if not SendRaw(Token) then Exit;
    end;

    if Status = SEC_E_INCOMPLETE_MESSAGE then begin
      if not ReadRaw(HANDSHAKE_TIMEOUT_MS) then Exit;
      Continue;
    end;

    if CheckSSPI(Status) then Exit;

    if Status = SEC_I_CONTINUE_NEEDED then
      if not ReadRaw(HANDSHAKE_TIMEOUT_MS) then Exit;

  until Status = SEC_E_OK;

  Status := SChannel_QueryContextAttributes(
    @FCtxtHandle, SECPKG_ATTR_STREAM_SIZES, @FStreamSizes);
  if CheckSSPI(Status) then Exit;
  FSizesValid := True;

  { Cache connection info immediately after handshake }
  FConnInfoValid := SChannel_QueryContextAttributes(
    @FCtxtHandle, SECPKG_ATTR_CONNECTION_INFO, @FConnInfo) = SEC_E_OK;
  if not FConnInfoValid then
    FKeyInfoValid := SChannel_QueryContextAttributes(
      @FCtxtHandle, SECPKG_ATTR_KEY_INFO, @FKeyInfo) = SEC_E_OK;

  FEOF        := False;
  FSSLEnabled := True;
  Result      := True;
end;

// ===========================================================================
// Encrypt / Decrypt
// ===========================================================================

function TSSLSChannel.EncryptAndSend(Buffer: Pointer; Len: Integer): Integer;
var
  MsgBuf    : AnsiString;
  Bufs      : array[0..3] of TSecBuffer;
  Desc      : TSecBufferDesc;
  Status    : SECURITY_STATUS;
  Offset    : Integer;
  ChunkLen  : Integer;
  TotalSent : Integer;
begin
  Result := -1;
  if not FSizesValid then Exit;

  TotalSent := 0;
  Offset    := 0;

  while Offset < Len do begin
    ChunkLen := Len - Offset;
    if ChunkLen > Integer(FStreamSizes.cbMaximumMessage) then
      ChunkLen := Integer(FStreamSizes.cbMaximumMessage);

    { Contiguous buffer layout: [TLS header][plaintext][TLS trailer] }
    SetLength(MsgBuf,
      Integer(FStreamSizes.cbHeader) + ChunkLen + Integer(FStreamSizes.cbTrailer));

    Move(PAnsiChar(Buffer)[Offset],
         MsgBuf[Integer(FStreamSizes.cbHeader) + 1],
         ChunkLen);

    Bufs[0].BufferType := SECBUFFER_STREAM_HEADER;
    Bufs[0].cbBuffer   := FStreamSizes.cbHeader;
    Bufs[0].pvBuffer   := PAnsiChar(MsgBuf);

    Bufs[1].BufferType := SECBUFFER_DATA;
    Bufs[1].cbBuffer   := ULONG(ChunkLen);
    Bufs[1].pvBuffer   := PAnsiChar(MsgBuf) + FStreamSizes.cbHeader;

    Bufs[2].BufferType := SECBUFFER_STREAM_TRAILER;
    Bufs[2].cbBuffer   := FStreamSizes.cbTrailer;
    Bufs[2].pvBuffer   := PAnsiChar(MsgBuf) + FStreamSizes.cbHeader + ChunkLen;

    Bufs[3].BufferType := SECBUFFER_EMPTY;
    Bufs[3].cbBuffer   := 0;
    Bufs[3].pvBuffer   := nil;

    Desc.ulVersion := SECBUFFER_VERSION;
    Desc.cBuffers  := 4;
    Desc.pBuffers  := @Bufs[0];

    Status := SChannel_EncryptMessage(@FCtxtHandle, 0, @Desc, 0);
    if CheckSSPI(Status) then Exit;

    { Actual encrypted sizes may differ from the originals }
    SetLength(MsgBuf, Bufs[0].cbBuffer + Bufs[1].cbBuffer + Bufs[2].cbBuffer);
    if not SendRaw(MsgBuf) then Exit;

    Inc(TotalSent, ChunkLen);
    Inc(Offset,    ChunkLen);
  end;

  Result := TotalSent;
end;

function TSSLSChannel.DecryptAvailable: Boolean;
{ Decrypt as many complete TLS records as possible from FRawBuf into FPlainBuf.
  Returns True if at least one plaintext byte was produced.
  Sets FEOF = True when the peer sends a close_notify or on fatal errors. }
var
  Bufs      : array[0..3] of TSecBuffer;
  Desc      : TSecBufferDesc;
  Status    : SECURITY_STATUS;
  i         : Integer;
  OldLen    : Integer;
  ExtraData : AnsiString;
begin
  Result := False;

  while Length(FRawBuf) > 0 do begin
    Bufs[0].BufferType := SECBUFFER_DATA;
    Bufs[0].cbBuffer   := Length(FRawBuf);
    Bufs[0].pvBuffer   := PAnsiChar(FRawBuf);
    for i := 1 to 3 do begin
      Bufs[i].BufferType := SECBUFFER_EMPTY;
      Bufs[i].cbBuffer   := 0;
      Bufs[i].pvBuffer   := nil;
    end;
    Desc.ulVersion := SECBUFFER_VERSION;
    Desc.cBuffers  := 4;
    Desc.pBuffers  := @Bufs[0];

    Status := SChannel_DecryptMessage(@FCtxtHandle, @Desc, 0, nil);

    if Status = SEC_E_INCOMPLETE_MESSAGE then
      Break;  { need more bytes from the socket }

    if CheckSSPI(Status) then begin
      { Fatal decryption error – treat as EOF so callers don't loop forever }
      FEOF := True;
      Break;
    end;

    { Collect plaintext and note any leftover ciphertext.
      Binary-safe: use Move, NOT AnsiString cast which stops at null bytes. }
    ExtraData := '';
    for i := 0 to 3 do begin
      if Bufs[i].BufferType = SECBUFFER_DATA then begin
        OldLen := Length(FPlainBuf);
        SetLength(FPlainBuf, OldLen + Integer(Bufs[i].cbBuffer));
        Move(Bufs[i].pvBuffer^, FPlainBuf[OldLen + 1], Bufs[i].cbBuffer);
        Result := True;
      end;
      if Bufs[i].BufferType = SECBUFFER_EXTRA then
        SetString(ExtraData, PAnsiChar(Bufs[i].pvBuffer), Bufs[i].cbBuffer);
    end;
    FRawBuf := ExtraData;

    if Status = SEC_I_CONTEXT_EXPIRED then begin
      { Peer sent TLS close_notify – signal EOF to the application layer }
      FEOF := True;
      Break;
    end;

    if Status = SEC_I_RENEGOTIATE then begin
      { Server requests TLS renegotiation – perform a new handshake on the
        existing connection. FRawBuf may already contain the first bytes of
        the new handshake (SECBUFFER_EXTRA from the last DecryptMessage). }
      FSizesValid    := False;
      FConnInfoValid := False;
      FKeyInfoValid  := False;
      FSSLEnabled    := False;
      if DoClientHandshake then
        { Handshake succeeded – continue decrypting any leftover data }
      else begin
        FEOF := True;
        Break;
      end;
    end;
  end;
end;

// ===========================================================================
// Constructor / Destructor
// ===========================================================================

constructor TSSLSChannel.Create(const Value: TTCPBlockSocket);
begin
  inherited Create(Value);
  FDataTimeout := -1;  { block indefinitely, like OpenSSL plugin }
  FCredValid  := False;
  FCtxtValid  := False;
  FSizesValid := False;
  FServerCert      := nil;
  FPFXStore        := nil;
  FClientCert      := nil;
  FClientCertStore := nil;
  FConnInfoValid   := False;
  FKeyInfoValid    := False;
  FillChar(FConnInfo, SizeOf(FConnInfo), 0);
  FillChar(FKeyInfo,  SizeOf(FKeyInfo),  0);
  FRawBuf          := '';
  FPlainBuf   := '';
  FillChar(FCredHandle,  SizeOf(FCredHandle),  0);
  FillChar(FCtxtHandle,  SizeOf(FCtxtHandle),  0);
  FillChar(FStreamSizes, SizeOf(FStreamSizes), 0);
end;

destructor TSSLSChannel.Destroy;
begin
  ClearHandles;
  FreeServerCert;
  FreeClientCert;
  inherited Destroy;
end;

// ===========================================================================
// Public interface
// ===========================================================================

function TSSLSChannel.LibVersion: String;
begin
  Result := 'Windows SChannel';
end;

function TSSLSChannel.LibName: String;
begin
  Result := 'ssl_schannel';
end;

function TSSLSChannel.Connect: boolean;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then Exit;
  FLastError     := 0;
  FLastErrorDesc := '';
  ClearHandles;
  Result := DoClientHandshake;
end;

function TSSLSChannel.Accept: boolean;
begin
  Result := False;
  if FSocket.Socket = INVALID_SOCKET then Exit;
  FLastError     := 0;
  FLastErrorDesc := '';
  ClearHandles;
  Result := DoServerHandshake;
end;

function TSSLSChannel.Shutdown: boolean;
var
  ShutdownToken : DWORD;
  InBufs        : array[0..0] of TSecBuffer;
  InDesc        : TSecBufferDesc;
  OutBufs       : array[0..0] of TSecBuffer;
  OutDesc       : TSecBufferDesc;
  CtxAttr       : ULONG;
  ts            : TTimeStamp;
  Token         : AnsiString;
begin
  Result := True;

  if FCtxtValid and Assigned(SChannel_ApplyControlToken) then begin
    ShutdownToken := 1;  { SCHANNEL_SHUTDOWN }
    InBufs[0].BufferType := SECBUFFER_TOKEN;
    InBufs[0].cbBuffer   := SizeOf(ShutdownToken);
    InBufs[0].pvBuffer   := @ShutdownToken;
    InDesc.ulVersion := SECBUFFER_VERSION;
    InDesc.cBuffers  := 1;
    InDesc.pBuffers  := @InBufs[0];
    SChannel_ApplyControlToken(@FCtxtHandle, @InDesc);

    OutBufs[0].BufferType := SECBUFFER_TOKEN;
    OutBufs[0].cbBuffer   := 0;
    OutBufs[0].pvBuffer   := nil;
    OutDesc.ulVersion := SECBUFFER_VERSION;
    OutDesc.cBuffers  := 1;
    OutDesc.pBuffers  := @OutBufs[0];

    SChannel_InitializeSecurityContextW(
      @FCredHandle, @FCtxtHandle, nil,
      ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_SEQUENCE_DETECT or
      ISC_REQ_REPLAY_DETECT   or ISC_REQ_CONFIDENTIALITY or ISC_REQ_STREAM,
      0, 0, nil, 0, @FCtxtHandle, @OutDesc, @CtxAttr, @ts);

    if (OutBufs[0].cbBuffer > 0) and Assigned(OutBufs[0].pvBuffer) then begin
      SetString(Token, PAnsiChar(OutBufs[0].pvBuffer), OutBufs[0].cbBuffer);
      SChannel_FreeContextBuffer(OutBufs[0].pvBuffer);
      SendRaw(Token);   { best-effort during shutdown }
    end;
  end;

  ClearHandles;
  FreeServerCert;
  FreeClientCert;
  FSSLEnabled := False;
end;

function TSSLSChannel.BiShutdown: boolean;
begin
  Result := Shutdown;
end;

function TSSLSChannel.SendBuffer(Buffer: TMemory; Len: Integer): Integer;
begin
  FLastError     := 0;
  FLastErrorDesc := '';
  if not FSSLEnabled then begin
    Result := -1;
    Exit;
  end;
  Result := EncryptAndSend(Buffer, Len);
end;

function TSSLSChannel.RecvBuffer(Buffer: TMemory; Len: Integer): Integer;
var
  Got     : Integer;
  Timeout : Integer;
begin
  FLastError     := 0;
  FLastErrorDesc := '';
  Result := 0;
  if not FSSLEnabled then Exit;

  { If EOF was already signalled (close_notify or TCP close), report it }
  if FEOF then Exit;

  { Opportunistically decrypt anything already in FRawBuf }
  if Length(FPlainBuf) = 0 then begin
    if (Length(FRawBuf) > 0) or FSocket.CanRead(0) then
      ReadRaw(0);
    DecryptAvailable;
  end;

  { Block until at least some plaintext is available.
    FDataTimeout = -1 means block indefinitely (same as OpenSSL plugin);
    set DataTimeout to a positive value for a finite receive timeout. }
  if Length(FPlainBuf) = 0 then begin
    Timeout := FDataTimeout;
    if not ReadRaw(Timeout) then begin
      { ReadRaw returns False either on timeout or on TCP close (Recv=0).
        Check whether the socket was actually closed by the peer. }
      if FSocket.LastError <> 0 then
        FEOF := True;
      Exit;
    end;
    DecryptAvailable;
    { DecryptAvailable may have set FEOF (close_notify); if no plaintext
      was produced we still return 0 to signal EOF to the caller. }
    if FEOF and (Length(FPlainBuf) = 0) then Exit;
  end;

  Got := Length(FPlainBuf);
  if Got > Len then Got := Len;
  if Got > 0 then begin
    Move(FPlainBuf[1], Buffer^, Got);
    Delete(FPlainBuf, 1, Got);
  end;
  Result := Got;
end;

function TSSLSChannel.WaitingData: Integer;
begin
  if Length(FRawBuf) > 0 then
    DecryptAvailable;
  Result := Length(FPlainBuf);
end;

function TSSLSChannel.ImplementsEOF: boolean;
begin
  Result := True;
end;

// ===========================================================================
// Certificate and cipher-suite information
// ===========================================================================

function TSSLSChannel.GetPeerCertContext: PCCERT_CONTEXT;
var
  Cert   : PCCERT_CONTEXT;
  Status : SECURITY_STATUS;
begin
  Result := nil;
  if not FCtxtValid then Exit;
  Status := SChannel_QueryContextAttributes(
    @FCtxtHandle, SECPKG_ATTR_REMOTE_CERT_CONTEXT, @Cert);
  if Status = SEC_E_OK then
    Result := Cert;
  { Caller is responsible for freeing with Crypt32_CertFreeCertificateContext }
end;

function TSSLSChannel.GetPeerSubject: string;
var
  Cert : PCCERT_CONTEXT;
  Buf  : array[0..1023] of Char;
begin
  Result := '';
  Cert := GetPeerCertContext;
  if not Assigned(Cert) then Exit;
  try
    if Assigned(Crypt32_CertGetNameString) then begin
{$IFDEF UNICODE}
      TCertGetNameStringW(Crypt32_CertGetNameString)(Cert, CERT_NAME_RDN_TYPE, 0, nil, Buf, 1024);
{$ELSE}
      TCertGetNameStringA(Crypt32_CertGetNameString)(Cert, CERT_NAME_RDN_TYPE, 0, nil, Buf, 1024);
{$ENDIF}
      Result := Buf;
    end;
  finally
    Crypt32_CertFreeCertificateContext(Cert);
  end;
end;

function TSSLSChannel.GetPeerIssuer: string;
var
  Cert : PCCERT_CONTEXT;
  Buf  : array[0..1023] of Char;
begin
  Result := '';
  Cert := GetPeerCertContext;
  if not Assigned(Cert) then Exit;
  try
    if Assigned(Crypt32_CertGetNameString) then begin
{$IFDEF UNICODE}
      TCertGetNameStringW(Crypt32_CertGetNameString)(Cert, CERT_NAME_RDN_TYPE, CERT_NAME_ISSUER_FLAG, nil, Buf, 1024);
{$ELSE}
      TCertGetNameStringA(Crypt32_CertGetNameString)(Cert, CERT_NAME_RDN_TYPE, CERT_NAME_ISSUER_FLAG, nil, Buf, 1024);
{$ENDIF}
      Result := Buf;
    end;
  finally
    Crypt32_CertFreeCertificateContext(Cert);
  end;
end;

function TSSLSChannel.GetPeerName: string;
var
  Cert : PCCERT_CONTEXT;
  Buf  : array[0..1023] of Char;
begin
  Result := '';
  Cert := GetPeerCertContext;
  if not Assigned(Cert) then Exit;
  try
    if Assigned(Crypt32_CertGetNameString) then begin
{$IFDEF UNICODE}
      TCertGetNameStringW(Crypt32_CertGetNameString)(Cert, CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, nil, Buf, 1024);
{$ELSE}
      TCertGetNameStringA(Crypt32_CertGetNameString)(Cert, CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, nil, Buf, 1024);
{$ENDIF}
      Result := Buf;
    end;
  finally
    Crypt32_CertFreeCertificateContext(Cert);
  end;
end;

function TSSLSChannel.GetPeerSerialNo: integer;
{ Returns the certificate serial number as a 32-bit integer (low-order bytes).
  Serial numbers can be arbitrarily large; we return the low 4 bytes which is
  sufficient for the TCustomSSL interface contract. }
var
  Cert     : PCCERT_CONTEXT;
  Ctx      : PCertContext absolute Cert;
  SN       : PCryptIntegerBlob;
  i        : Integer;
  ByteVal  : Byte;
begin
  Result := -1;
  Cert := GetPeerCertContext;
  if not Assigned(Cert) then Exit;
  try
    if not Assigned(Ctx.pCertInfo) then Exit;
    SN := @Ctx.pCertInfo^.SerialNumber;
    if (SN.cbData = 0) or not Assigned(SN.pbData) then Exit;
    { Serial number bytes are in little-endian (least-significant first) order
      as stored in the CERT_INFO structure. }
    Result := 0;
    for i := 0 to 3 do begin
      if DWORD(i) >= SN.cbData then Break;
      ByteVal := PByte(PAnsiChar(SN.pbData) + i)^;
      Result := Result or (Integer(ByteVal) shl (i * 8));
    end;
  finally
    Crypt32_CertFreeCertificateContext(Cert);
  end;
end;

function TSSLSChannel.GetPeerNameHash: cardinal;
{ Returns the first 4 bytes of the SHA-1 hash of the DER-encoded Subject DN,
  interpreted as a little-endian cardinal.
  This is compatible with the OpenSSL X509_NAME_hash algorithm for certificates
  that use standard DER encoding without OpenSSL's additional canonicalisation.
  For exact OpenSSL compatibility on normalised names the result may differ,
  but for the purpose of fast certificate identity checks it is sufficient. }
const
  CALG_SHA1 = $8004;
var
  Cert    : PCCERT_CONTEXT;
  Ctx     : PCertContext absolute Cert;
  HashBuf : array[0..19] of Byte;
  HashLen : DWORD;
begin
  Result := 0;
  if not Assigned(Crypt32_CryptHashCertificate) then Exit;
  Cert := GetPeerCertContext;
  if not Assigned(Cert) then Exit;
  try
    if not Assigned(Ctx.pCertInfo) then Exit;
    with Ctx.pCertInfo^.Subject do begin
      if (cbData = 0) or not Assigned(pbData) then Exit;
      HashLen := SizeOf(HashBuf);
      if Crypt32_CryptHashCertificate(
           nil, CALG_SHA1, 0,
           pbData, cbData,
           @HashBuf[0], @HashLen) and (HashLen >= 4) then
        { First 4 bytes in little-endian order, same convention as OpenSSL }
        Result := PCardinal(@HashBuf[0])^;
    end;
  finally
    Crypt32_CertFreeCertificateContext(Cert);
  end;
end;

function TSSLSChannel.GetPeerFingerprint: string;
{ Returns the SHA-1 fingerprint of the peer certificate as a lowercase hex
  string (40 hex chars), matching the format used by ssl_openssl. }
const
  CALG_SHA1 = $8004;
var
  Cert     : PCCERT_CONTEXT;
  Ctx      : PCertContext absolute Cert;
  HashBuf  : array[0..19] of Byte;  { SHA-1 is always 20 bytes }
  HashLen  : DWORD;
  i        : Integer;
  HexDigit : string;
begin
  Result := '';
  if not Assigned(Crypt32_CryptHashCertificate) then Exit;
  Cert := GetPeerCertContext;
  if not Assigned(Cert) then Exit;
  try
    HashLen := SizeOf(HashBuf);
    if Crypt32_CryptHashCertificate(
         nil, CALG_SHA1, 0,
         Ctx.pbCertEncoded, Ctx.cbCertEncoded,
         @HashBuf[0], @HashLen) then begin
      Result := '';
      for i := 0 to Integer(HashLen) - 1 do begin
        HexDigit := IntToHex(HashBuf[i], 2);
        Result := Result + LowerCase(HexDigit);
      end;
    end;
  finally
    Crypt32_CertFreeCertificateContext(Cert);
  end;
end;

function TSSLSChannel.GetCertInfo: string;
begin
  Result := 'Subject: ' + GetPeerSubject + #13#10 +
            'Issuer: '  + GetPeerIssuer;
end;

function TSSLSChannel.GetSSLVersion: string;
begin
  Result := '';
  if not FConnInfoValid then Exit;
  if FConnInfo.dwProtocol and PROTO_TLS1_3 <> 0 then
    Result := 'TLS 1.3'
  else if FConnInfo.dwProtocol and PROTO_TLS1_2 <> 0 then
    Result := 'TLS 1.2'
  else if FConnInfo.dwProtocol and PROTO_TLS1_1 <> 0 then
    Result := 'TLS 1.1'
  else if FConnInfo.dwProtocol and PROTO_TLS1_0 <> 0 then
    Result := 'TLS 1.0'
  else
    Result := 'Protocol 0x' + IntToHex(FConnInfo.dwProtocol, 8);
end;

function TSSLSChannel.GetCipherName: string;
begin
  Result := '';
  if FConnInfoValid then begin
    case FConnInfo.aiCipher of
      CALG_DES     : Result := 'DES';
      CALG_RC2     : Result := 'RC2';
      CALG_3DES    : Result := '3DES';
      CALG_DESX    : Result := 'DESX';
      CALG_3DES_112: Result := '3DES-112';
      CALG_RC5     : Result := 'RC5';
      CALG_AES_128 : Result := 'AES-128';
      CALG_AES_192 : Result := 'AES-192';
      CALG_AES_256 : Result := 'AES-256';
      CALG_AES     : Result := 'AES-' + IntToStr(FConnInfo.dwCipherStrength);
      CALG_RC4     : Result := 'RC4';
      CALG_SEAL    : Result := 'SEAL';
    else
      Result := 'ALG_ID 0x' + IntToHex(FConnInfo.aiCipher, 4);
    end;
  end else if FKeyInfoValid and (FKeyInfo.sEncryptAlgorithmName <> nil) then
    Result := FKeyInfo.sEncryptAlgorithmName;
end;

function TSSLSChannel.GetCipherBits: integer;
begin
  if FConnInfoValid then
    Result := Integer(FConnInfo.dwCipherStrength)
  else if FKeyInfoValid then
    Result := Integer(FKeyInfo.KeySize)
  else
    Result := 0;
end;

function TSSLSChannel.GetCipherAlgBits: integer;
begin
  Result := GetCipherBits;
end;

function TSSLSChannel.GetVerifyCert: integer;
begin
  { Convention: 0 = OK (matches ssl_openssl).
    If Connect / Accept succeeded, SChannel's chain validation passed. }
  if FSSLEnabled then Result := 0 else Result := 1;
end;

// ===========================================================================
// Unit initialisation
// ===========================================================================

initialization
  if InitSChannelInterface then
    SSLImplementation := TSSLSChannel;

end.
