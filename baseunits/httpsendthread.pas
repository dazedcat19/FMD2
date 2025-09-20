unit httpsendthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, synautil, synacode, ssl_openssl3, blcksock,
  BaseThread, httpcookiemanager, dateutils, strutils,
  GZIPUtils, BrotliDec;

const

  HTTPFormatSettings :TFormatSettings = (
    CurrencyFormat            :1;
    NegCurrFormat             :5;
    ThousandSeparator         :',';
    DecimalSeparator          :'.';
    CurrencyDecimals          :2;
    DateSeparator             :'/';
    TimeSeparator             :':';
    ListSeparator             :',';
    CurrencyString            :'$';
    ShortDateFormat           :'m/d/y';
    LongDateFormat            :'dd" "mmmm" "yyyy';
    TimeAMString              :'AM';
    TimePMString              :'PM';
    ShortTimeFormat           :'hh:nn';
    LongTimeFormat            :'hh:nn:ss';
    ShortMonthNames           :('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    LongMonthNames            :('January', 'February', 'March', 'April', 'May',
                                'June', 'July', 'August', 'September', 'October',
                                'November', 'December');
    ShortDayNames             :('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    LongDayNames              :('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                                'Thursday', 'Friday', 'Saturday');
    TwoDigitYearCenturyWindow :50;
  );

  // https://tools.ietf.org/html/rfc2616#section-3.3.1
  HTTPCookieExpiresFormat = 'ddd, dd-mmm-yy hh:nn:ss';

type

  { THTTPQueue }

  THTTPQueue = class
  private
    FGuardian: TRTLCriticalSection;
    FActiveConnections: Cardinal;
  public
    MaxConnections: Cardinal;
    constructor Create;
    destructor Destroy; override;
    procedure AddConnection;
    procedure DoneConnection; inline;
    property ActiveConnections: Cardinal read FActiveConnections;
  end;

  THTTPSendThread = class;

  THTTPSendThreadEvent = procedure(const AHTTP: THTTPSendThread) of object;

  THTTPMethodEvent = procedure(const AHTTP: THTTPSendThread; var Method, URL: String);

  THTTPRequestEvent = function(const AHTTP: THTTPSendThread; const Method, URL: String; const Response: TObject = nil): Boolean of object;

  THTTPMethodRedirectEvent = procedure(const AHTTP: THTTPSendThread; const URL: String) of object;

  { THTTPSendThread }

  THTTPSendThread = class(THTTPSend)
  private
    FRunning: Boolean;
    FURL: String;
    FOwner: TBaseThread;
    FRetryCount: Integer;
    FCompress: Boolean;
    FFollowRedirection: Boolean;
    FMaxRedirect: Integer;
    FAllowServerErrorResponse: Boolean;
    FEnabledCookies: Boolean;
    FClearCookies: Boolean;
    FOwnerCustomTerminate: TNotifyEvent;
    procedure SetTimeout(AValue: Integer);
    procedure OnOwnerTerminate(Sender: TObject);
  protected
    procedure NormalizeHeaders;
    procedure SetHTTPCookies;
    function InternalHTTPRequest(const Method, URL: String; const Response: TObject = nil): Boolean;
    function DefaultHTTPRequest(const Method, URL: String; const Response: TObject = nil): Boolean;
  public
    constructor Create(const AOwner: TBaseThread = nil);
    destructor Destroy; override;
    procedure ParseHTTPCookies; inline;
    function HTTPMethod(const Method, URL: string): Boolean;
    function HTTPRequest(const Method, URL: String; const Response: TObject = nil): Boolean;
    function HEAD(const URL: String; const Response: TObject = nil): Boolean;
    function GET(const URL: String; const Response: TObject = nil): Boolean;
    function POST(const URL: String; const POSTData: String = ''; const Response: TObject = nil): Boolean;
    function XHR(const URL: String; const Response: TObject = nil): Boolean;
    function GetCookies: String;
    procedure MergeCookies(const ACookies: String);
    procedure AddServerCookies(const AURL, ACookies: String; const AServerDate: TDateTime); inline;
    function GetLastModified: TDateTime;
    function GetOriginalFileName: String;
    function ThreadTerminated: Boolean; inline;
    procedure Stop; inline;
    procedure RemoveCookie(const CookieName: String);
    procedure SetProxy(const ProxyType, Host, Port, User, Pass: String);
    procedure GetProxy(var ProxyType, Host, Port, User, Pass: String);
    procedure SetNoProxy;
    procedure SetDefaultProxy;
    procedure AcceptImage; inline;
    procedure Reset;
    procedure ResetBasic;
    procedure ClearCookies; inline;
    procedure ClearCookiesStorage; inline;
    procedure SaveDocumentToFile(const AFileName: String; const ATryOriginalFileName: Boolean = False; const ALastModified: TDateTime = -1);
    property Timeout: Integer read FTimeout write SetTimeout;
    property RetryCount: Integer read FRetryCount write FRetryCount;
    property Compress: Boolean read FCompress write FCompress;
    property FollowRedirection: Boolean read FFollowRedirection write FFollowRedirection;
    property AllowServerErrorResponse: Boolean read FAllowServerErrorResponse write FAllowServerErrorResponse;
    property Thread: TBaseThread read FOwner;
    property MaxRedirect: Integer read FMaxRedirect write FMaxRedirect;
    property LastURL: String read FURL;
    property EnabledCookies: Boolean read FEnabledCookies write FEnabledCookies;
    property Running: Boolean read FRunning;
  public
    OnBeforeHTTPMethod: THTTPMethodEvent;
    OnAfterHTTPMethod: THTTPMethodEvent;
    OnAfterSetHTTPCookies: THTTPSendThreadEvent;
    OnHTTPRequest: THTTPRequestEvent;
    OnRedirected: THTTPMethodRedirectEvent;
    CookieManager: THTTPCookieManager;
    ConnectionsQueue: THTTPQueue;
    LuaHandler: Pointer;
  end;

  TKeyValuePair = array[0..1] of String;

function KeyVal(const AKey, AValue: String): TKeyValuePair;
function QueryString(KeyValuePairs: array of TKeyValuePair): String;
function SetDefaultProxy(const ProxyType, Host, Port, User, Pass: String): Boolean;
procedure SetDefaultProxyAndApply(const ProxyType, Host, Port, User, Pass: String);
procedure SetDefaultTimeoutAndApply(const ATimeout: Integer);
procedure SetDefaultRetryCountAndApply(const ARetryCount: Integer);

function MaybeEncodeURL(const AValue: String): String;
procedure SplitURL(const AURL: String; const AHost, APath: PString;
  const AIncludeProtocol: Boolean = True; const AIncludePort: Boolean = True);

function FormatByteSize(const ABytes: Integer; AShowPerSecond: Boolean = False): String;

const
  UserAgentSynapse   = 'Mozilla/4.0 (compatible; Synapse)';
  UserAgentCURL      = 'curl/7.70.0';
  UserAgentDefault   = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36'; // Chrome default at 04.03.2025
  HeartBeatRate      = 1000;

var
  DefaultUserAgent: String = UserAgentDefault;
  DefaultRetryCount: Integer = 0;
  DefaultTimeout: Integer = 15000;
  DefaultProxyType: String = '';
  DefaultProxyHost: String = '';
  DefaultProxyPort: String = '';
  DefaultProxyUser: String = '';
  DefaultProxyPass: String = '';

implementation

var
  ALLHTTPSendThread: TFPList;
  CS_ALLHTTPSendThread: TRTLCriticalSection;

function poschar(const c:char;const s:string;const offset:cardinal=1;const escapechars:string=''):integer;
var
  i:integer;
begin
  for i:=offset to length(s) do
  begin
    if s[i] in escapechars then Break;
    if s[i]=c then Exit(i);
  end;
  Result:=0;
end;

procedure SplitURL(const AURL: String; const AHost, APath: PString;
  const AIncludeProtocol: Boolean; const AIncludePort: Boolean);

procedure cleanuri(var u:string);
begin
  while (Length(u)<>0) and (u[1] in ['.',':','/']) do
    Delete(u,1,1);
end;

var
  iurl,ihost,ipath,iproto,iport: String;
  p,q: Integer;
begin
  if (AHost=nil) and (APath=nil) then Exit;
  if Assigned(AHost) then AHost^:='';
  if Assigned(APath) then APath^:='';
  iurl:=Trim(AURL);
  if iurl='' then Exit;
  ihost:='';
  ipath:='';
  iproto:='';
  iport:='';
  if iurl[1]='/' then
    if Length(iurl)=1 then Exit
    else
    if iurl[2]<>'/' then
    begin
      if Assigned(APath) then APath^:=iurl;
      Exit;
    end;
  p:=poschar(':',iurl,1,'/');
  if (p<>0) and (p+2<Length(iurl)) and (iurl[P+1]+iurl[p+2]='//') then
  begin
    iproto:=Copy(iurl,1,p-1);
    Delete(iurl,1,p+2);
    p:=poschar(':',iurl,1,'/');
  end;
  q:=0;
  if (p<>0) and (p<Length(iurl)) and (iurl[P+1] in ['0'..'9']) then
  begin
    for q:=p+1 to Length(iurl) do
      if not (iurl[q] in ['0'..'9']) then Break;
    if q=Length(iurl) then Inc(q);
    iport:=Copy(iurl,p+1,q-p-1);
    delete(iurl,p,q-p);
  end;
  cleanuri(iurl);
  p:=poschar('.',iurl,1,'/');
  q:=poschar('/',iurl);
  if (p<>0) and (p<Length(iurl)) then
    if p<q then
    begin
      ihost:=Copy(iurl,1,q-1);
      Delete(iurl,1,q-1);
      cleanuri(iurl);
    end
    else
    if q=0 then
    begin
      q:=poschar('.',iurl,p+1);
      if (q<>0) and (q<Length(iurl)) then
      begin
        ihost:=iurl;
        iurl:='';
      end;
    end;
  if (ihost='') and (iurl<>'') and ((iproto<>'') or (iport<>'')) then
  begin
    ihost:=iurl;
    iurl:='';
  end;
  if ihost<>'' then
  begin
    if AIncludeProtocol then
    begin
      if iproto<>'' then ihost:=iproto+'://'+ihost
      else ihost:='https://'+ihost;
    end;
    if AIncludePort and (iport<>'') then
      ihost:=ihost+':'+iport;
  end;
  if iurl<>'' then
    ipath:='/'+iurl;
  if Assigned(AHost) then AHost^:=ihost;
  if Assigned(APath) then APath^:=ipath;
end;

function FormatByteSize(const ABytes: Integer; AShowPerSecond: Boolean): String;
const
  B = 1;
  KB = 1024 * B;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if ABytes > GB then
    Result := FormatFloat('#.## GB', ABytes / GB)
  else
  if ABytes > MB then
    Result := FormatFloat('#.## MB', ABytes / MB)
  else
  if ABytes > KB then
    Result := FormatFloat('#.## KB', ABytes / KB)
  else
  if ABytes = 0 then
  begin
    if AShowPerSecond then
      Result := '0 B'
    else
      Result := '0 Bytes';
  end
  else
  begin
    if AShowPerSecond then
      Result := FormatFloat('#.## B', ABytes)
    else
      Result := FormatFloat('#.## Bytes', ABytes);
  end;
  if AShowPerSecond then
    Result := Result + 'ps';
end;

function KeyVal(const AKey, AValue: String): TKeyValuePair;
begin
  Result[0] := AKey;
  Result[1] := AValue;
end;

function QueryString(KeyValuePairs: array of TKeyValuePair): String;
var
  i: Integer;
begin
  Result := '';
  if Length(KeyValuePairs) > 0 then
    for i := Low(KeyValuePairs) to High(KeyValuePairs) do
    begin
      if Result <> '' then
        Result := Result + '&';
      Result := Result + EncodeURL(KeyValuePairs[i, 0]) + '=' + EncodeURL(KeyValuePairs[i, 1]);
    end;
end;

function SetDefaultProxy(const ProxyType, Host, Port, User, Pass: String): Boolean;
begin
  Result := (ProxyType <> DefaultProxyType) or
    (Host <> DefaultProxyHost) or
    (Port <> DefaultProxyPort) or
    (User <> DefaultProxyUser) or
    (Pass <> DefaultProxyPass);
  if not Result then Exit;
  DefaultProxyType := ProxyType;
  DefaultProxyHost := Host;
  DefaultProxyPort := Port;
  DefaultProxyUser := User;
  DefaultProxyPass := Pass;
end;

procedure SetDefaultProxyAndApply(const ProxyType, Host, Port, User, Pass: String);
var
  i: SizeInt;
begin
  if not SetDefaultProxy(ProxyType, Host, Port, User, Pass) then Exit;
  EnterCriticalsection(CS_ALLHTTPSendThread);
  try
    if ALLHTTPSendThread.Count > 0 then
      for i := 0 to ALLHTTPSendThread.Count - 1 do
        THTTPSendThread(ALLHTTPSendThread[i]).SetProxy(ProxyType, Host, Port, User, Pass);
  finally
    LeaveCriticalsection(CS_ALLHTTPSendThread);
  end;
end;

procedure SetDefaultTimeoutAndApply(const ATimeout: Integer);
var
  i: SizeInt;
begin
  if ATimeout = DefaultTimeout then Exit;
  DefaultTimeout := ATimeout;
  EnterCriticalsection(CS_ALLHTTPSendThread);
  try
    if ALLHTTPSendThread.Count > 0 then
      for i := 0 to ALLHTTPSendThread.Count - 1 do
        THTTPSendThread(ALLHTTPSendThread[i]).Timeout := ATimeout;
  finally
    LeaveCriticalsection(CS_ALLHTTPSendThread);
  end;
end;

procedure SetDefaultRetryCountAndApply(const ARetryCount: Integer);
var
  i: SizeInt;
begin
  if ARetryCount = DefaultRetryCount then Exit;
  DefaultRetryCount := ARetryCount;
  EnterCriticalsection(CS_ALLHTTPSendThread);
  try
    if ALLHTTPSendThread.Count > 0 then
      for i := 0 to ALLHTTPSendThread.Count - 1 do
        THTTPSendThread(ALLHTTPSendThread[i]).RetryCount := ARetryCount;
  finally
    LeaveCriticalsection(CS_ALLHTTPSendThread);
  end;
end;

function MaybeEncodeURL(const AValue: String): String;
begin
  Result := Trim(AValue);
  if Result = '' then Exit;
  if Length(DecodeURL(Result)) >= Length(Result) then
    Result := EncodeURL(Result);
end;

{ THTTPQueue }

constructor THTTPQueue.Create;
begin
  InitCriticalSection(FGuardian);
  FActiveConnections := 0;
  MaxConnections := 0;
end;

destructor THTTPQueue.Destroy;
begin
  DoneCriticalSection(FGuardian);
  inherited Destroy;
end;

procedure THTTPQueue.AddConnection;
begin
  if MaxConnections=0 then
  begin
    InterlockedIncrement(FActiveConnections);
    Exit;
  end
  else
  begin
    EnterCriticalSection(FGuardian);
    try
      while (MaxConnections<>0) and (FActiveConnections >= MaxConnections) do
        Sleep(HeartBeatRate);
      InterlockedIncrement(FActiveConnections);
    finally
      LeaveCriticalSection(FGuardian);
    end;
  end;
end;

procedure THTTPQueue.DoneConnection;
begin
  InterlockedDecrement(FActiveConnections);
end;

{ THTTPSendThread }

procedure THTTPSendThread.SetTimeout(AValue: Integer);
begin
  if FTimeout = AValue then Exit;
  FTimeout := AValue;
  Sock.ConnectionTimeout := FTimeout;
  Sock.SocksTimeout := FTimeout;
  Sock.SetTimeout(FTimeout);
end;

procedure THTTPSendThread.OnOwnerTerminate(Sender: TObject);
begin
  Stop;
  if Assigned(FOwnerCustomTerminate) then
    FOwnerCustomTerminate(Sender);
end;

procedure THTTPSendThread.NormalizeHeaders;
var
  i: Integer;
begin
  for i := 0 to Headers.Count - 1 do
    Headers[i] := Trim(Headers.Names[i]) + ': ' + Trim(Headers.ValueFromIndex[i]);
end;

procedure THTTPSendThread.SetHTTPCookies;
begin
  if FClearCookies then
  begin
    FClearCookies := False;
    Exit;
  end;
  if FEnabledCookies and Assigned(CookieManager) then
    CookieManager.SetCookies(FURL, Self);
  if Assigned(OnAfterSetHTTPCookies) then
    OnAfterSetHTTPCookies(Self);
end;

procedure THTTPSendThread.ParseHTTPCookies;
begin
  if Assigned(CookieManager) then
    CookieManager.AddServerCookies(FURL, Self.Headers);
end;

function THTTPSendThread.InternalHTTPRequest(const Method, URL: String;
  const Response: TObject): Boolean;
begin
  if Assigned(OnHTTPRequest) then
    Result := OnHTTPRequest(Self, Method, URL, Response)
  else
    Result := HTTPRequest(Method, URL, Response);
end;

constructor THTTPSendThread.Create(const AOwner: TBaseThread);
begin
  inherited Create;
  FRunning:=False;
  LuaHandler := nil;
  KeepAlive := True;
  if Trim(DefaultUserAgent) <> '' then
    UserAgent := DefaultUserAgent;
  Protocol := '1.1';
  Headers.CaseSensitive := False;
  Headers.NameValueSeparator := ':';
  Headers.CaseSensitive := False;
  Cookies.NameValueSeparator := '=';
  Cookies.Delimiter := ';';
  FEnabledCookies := True;
  FClearCookies := False;
  FCompress := True;
  FFollowRedirection := True;
  FAllowServerErrorResponse := False;
  FRetryCount := DefaultRetryCount;
  FMaxRedirect := 5;
  SetTimeout(DefaultTimeout);
  SetProxy(DefaultProxyType, DefaultProxyHost, DefaultProxyPort, DefaultProxyUser, DefaultProxyPass);
  Reset;
  if Assigned(AOwner) then
  begin
    FOwner := AOwner;
    FOwnerCustomTerminate := FOwner.OnCustomTerminate;
    FOwner.OnCustomTerminate := @OnOwnerTerminate;
  end;
  OnBeforeHTTPMethod := nil;
  OnAfterHTTPMethod := nil;
  OnHTTPRequest := nil;
  EnterCriticalsection(CS_ALLHTTPSendThread);
  try
    ALLHTTPSendThread.Add(Self);
  finally
    LeaveCriticalsection(CS_ALLHTTPSendThread);
  end;
end;

destructor THTTPSendThread.Destroy;
begin
  If FOwner <> nil then
  begin
    if FOwnerCustomTerminate <> nil then
       FOwner.OnCustomTerminate := FOwnerCustomTerminate
    else
      FOwner.OnCustomTerminate := nil;
  end;
  EnterCriticalsection(CS_ALLHTTPSendThread);
  try
    ALLHTTPSendThread.Remove(Self);
  finally
    LeaveCriticalsection(CS_ALLHTTPSendThread);
  end;
  inherited Destroy;
end;

function THTTPSendThread.HTTPMethod(const Method, URL: string): Boolean;
var
  amethod, aurl: String;
begin
  amethod:=Method;
  aurl:=URL;
  try
    FRunning:=True;
    if Assigned(OnBeforeHTTPMethod) then
      OnBeforeHTTPMethod(Self, amethod, aurl);
    SetHTTPCookies;
    Result := inherited HTTPMethod(amethod, aurl);
    if FEnabledCookies then
      ParseHTTPCookies
    else
      Cookies.Clear;
    if Assigned(OnAfterHTTPMethod) then
      OnAfterHTTPMethod(Self, amethod, aurl);
  except
  end;
  FRunning:=False;
end;

function THTTPSendThread.HTTPRequest(const Method, URL: String; const Response: TObject): Boolean;
begin
  Result := False;
  if ThreadTerminated then Exit;
  if ConnectionsQueue<>nil then
    ConnectionsQueue.AddConnection;
  try
    Result := DefaultHTTPRequest(Method, URL, Response);
  except
  end;
  if ConnectionsQueue<>nil then
    ConnectionsQueue.DoneConnection;
end;

function THTTPSendThread.DefaultHTTPRequest(const Method, URL: String; const Response: TObject): Boolean;

  function CheckTerminate: Boolean;
  begin
    Result := Sock.Tag = 1;
    if Result then Sock.Tag := 0;
  end;

type TContentEncoding = (ceNone, ceGzip, ceBrotli);

var
  counter: Integer = 0;
  redirectcounter: Integer = 0;
  s, h, p: String;
  HTTPHeader: TStringList;
  dstream: TMemoryStream;
  content_encoding: TContentEncoding;

begin
  Result := False;
  FURL := TrimRight(TrimLeftSet(URL, [':', '/', #0..' ']));
  if FURL = '' then Exit;
  FURL := MaybeEncodeURL(FURL);
  if Pos('://',FURL)=0 then
    FURL:='https://'+FURL;
  if Pos('HTTP/', Headers.Text) = 1 then Reset;
  NormalizeHeaders;
  HTTPHeader := TStringList.Create;
  HTTPHeader.Assign(Headers);
  try
    // first request
    while (not HTTPMethod(Method, FURL)) or
      ((not FAllowServerErrorResponse) and (ResultCode > 500)) do begin
      if CheckTerminate then Exit;
      if (FRetryCount > -1) and (FRetryCount <= counter) then Exit;
      Inc(Counter);
      Headers.Assign(HTTPHeader);
    end;

    { redirection
      300 Multiple Choices
      301 Moved Permanently
      302 Found (aka Object Moved aka Moved Temporarily)
      303 See Other
      304 Not Modified
      305 Use Proxy
      306 Switch Proxy
      307 Temporary Redirect
      who have location 301, 302, 303, 307?
    }
    if FFollowRedirection then
      while (ResultCode-300) in [1, 2, 3, 7] do begin
        if CheckTerminate then Exit;
        // break too many redirect
        if redirectcounter >= FMaxRedirect then Exit
        else Inc(redirectcounter);
        if HTTPHeader.IndexOfName('Referer') = -1 then
          HTTPHeader.Add('Referer: ' + FURL);
        s := Trim(Headers.Values['Location']);
        if s<>'' then
        begin
          SplitURL(s,@h,@p);
          s:=p;
          if h='' then
            SplitURL(FURL,@h,@p);
          FURL:=h+s;
        end;

        if OnRedirected<>nil then
          OnRedirected(Self, FURL);

        Clear;
        Headers.Assign(HTTPHeader);
        counter := 0;
        while (not HTTPMethod('GET', FURL)) or
          ((not FAllowServerErrorResponse) and (ResultCode > 500)) do begin
          if checkTerminate then Exit;
          if (FRetryCount > -1) and (FRetryCount <= counter) then Exit;
          Inc(counter);
          Clear;
          Headers.Assign(HTTPHeader);
        end;
      end;
  finally
    HTTPHeader.Free;
  end;

  // decompress data
  if Document.Size>0 then
  begin
    content_encoding:=ceNone;
    s := LowerCase(Headers.Values['Content-Encoding']);
    if s.Contains('br') then
      content_encoding:=ceBrotli
    else if s.Contains('gzip') or s.Contains('deflate') then
      content_encoding:=ceGzip;
    if content_encoding<>ceNone then
    begin
      dstream := TMemoryStream.Create;
      try
        case content_encoding of
          ceGzip: unzipStream(Document, dstream);
          ceBrotli: BrotliDecodeStream(Document, dstream);
          else;
        end;
        Document.Clear;
        Document.LoadFromStream(dstream);
      except
      end;
      dstream.Free;
    end;
  end;

  // response
  if Assigned(Response) then
  begin
    if Response is TStringList then
      TStringList(Response).LoadFromStream(Document)
    else
    if Response is TStream then
      Document.SaveToStream(TStream(Response));
  end;
  Result := Document.Size > 0;
end;

function THTTPSendThread.HEAD(const URL: String; const Response: TObject): Boolean;
begin
  Result := InternalHTTPRequest('HEAD', URL, Response);
end;

function THTTPSendThread.GET(const URL: String; const Response: TObject): Boolean;
begin
  Result := InternalHTTPRequest('GET', URL, Response);
end;

function THTTPSendThread.POST(const URL: String; const POSTData: String; const Response: TObject): Boolean;
var
  L: Integer;
begin
  if POSTData <> '' then begin
    Document.Clear;
    WriteStrToStream(Document, POSTData);
  end;
  L := Headers.IndexOfName('Content-Type');
  if L <> -1 then
  begin
    MimeType := Trim(Headers.ValueFromIndex[L]);
    Headers.Delete(L);
  end;
  if (MimeType = 'text/html') then
    MimeType := 'application/x-www-form-urlencoded; charset=UTF-8';
  Result := InternalHTTPRequest('POST', URL, Response);
end;

function THTTPSendThread.XHR(const URL: String; const Response: TObject
  ): Boolean;
begin
  if Pos('HTTP/', Headers.Text) = 1 then Reset;
  Headers.Values['X-Requested-With'] := ' XMLHttpRequest';
  Result := GET(URL, Response);
end;

function THTTPSendThread.GetCookies: String;
var
  i: Integer;
begin
  Result := '';
  if Cookies.Count > 0 then
    for i := 0 to Cookies.Count - 1 do begin
      if Result = '' then Result := Cookies.Strings[i]
      else Result := Result + '; ' + Cookies.Strings[i];
    end;
end;

procedure THTTPSendThread.MergeCookies(const ACookies: String);
var
  v,s: String;
begin
  for v in ACookies.Split(';') do
  begin
    s:=v.Trim;
    if Pos('=', s) > 0 then
      Cookies.Values[Trim(SeparateLeft(s,'='))] := Trim(SeparateRight(s,'='))
    else if Cookies.IndexOf(s)=-1 then
      Cookies.Add(s);
  end;
end;

procedure THTTPSendThread.AddServerCookies(const AURL, ACookies: String;
  const AServerDate: TDateTime);
begin
  if Assigned(CookieManager) then
    CookieManager.AddServerCookies(AURL, ACookies, AServerDate);
end;

function THTTPSendThread.GetLastModified: TDateTime;
var
  s: String;
begin
  Result := Now;
  s := Trim(Headers.Values['last-modified']);
  if s <> '' then
    Result := DecodeRfcDateTime(s);
end;

function THTTPSendThread.GetOriginalFileName: String;
var
  s: String;
begin
  Result := '';
  s := Headers.Values['content-disposition'];
  if s <> '' then
    Result := GetBetween('filename="', '"', s);
end;

function THTTPSendThread.ThreadTerminated: Boolean;
begin
  if Assigned(FOwner) then
    Result := FOwner.IsTerminated
  else
    Result := False;
end;

procedure THTTPSendThread.Stop;
begin
  try
    Sock.Tag := 1;
    Sock.CloseSocket;
  except
  end;
end;

procedure THTTPSendThread.RemoveCookie(const CookieName: String);
var
  i: Integer;
begin
  if CookieName = '' then Exit;
  if Cookies.Count > 0 then begin
    i := Cookies.IndexOfName(CookieName);
    if i > -1 then Cookies.Delete(i);
  end;
end;

procedure THTTPSendThread.SetProxy(const ProxyType, Host, Port, User, Pass: String);
var
  pt: String;
begin
  pt := AnsiUpperCase(ProxyType);
  with Sock do begin
    ProxyHost := '';
    ProxyPort := '';
    ProxyUser := '';
    ProxyPass := '';
    SocksIP := '';
    SocksPort := '1080';
    SocksType := ST_Socks5;
    SocksUsername := '';
    SocksPassword := '';
    if pt = 'HTTP' then
    begin
      ProxyHost := Host;
      ProxyPort := Port;
      ProxyUser := User;
      ProxyPass := Pass;
    end
    else
    if (pt = 'SOCKS4') or (pt = 'SOCKS5') then
    begin
      if pt = 'SOCKS4' then
        SocksType := ST_Socks4
      else
      if pt = 'SOCKS5' then
        SocksType := ST_Socks5;
      SocksIP := Host;
      SocksPort := Port;
      SocksUsername := User;
      SocksPassword := Pass;
    end;
  end;
end;

procedure THTTPSendThread.GetProxy(var ProxyType, Host, Port, User, Pass: String);
begin
  if ProxyHost <> '' then
  begin
    ProxyType := 'HTTP';
    Host := ProxyHost;
    Port := ProxyPort;
    User := ProxyUser;
    Pass := ProxyPass;
  end
  else
  if Sock.SocksIP <> '' then
    with Sock do
    begin
      if SocksType = ST_Socks5 then
        ProxyType := 'SOCKS5'
      else
        ProxyType := 'SOCKS4';
      Host := SocksIP;
      Port := SocksPort;
      User := SocksUsername;
      Pass := SocksPassword;
    end
  else
  begin
    ProxyType := '';
    Host := '';
    Port := '';
    User := '';
    Pass := '';
  end;
end;

procedure THTTPSendThread.SetNoProxy;
begin
  SetProxy('', '', '', '', '');
end;

procedure THTTPSendThread.SetDefaultProxy;
begin
  SetProxy(DefaultProxyType, DefaultProxyHost, DefaultProxyPort, DefaultProxyUser, DefaultProxyPass);
end;

procedure THTTPSendThread.AcceptImage;
begin
  Headers.Values['Accept'] := 'image/webp,*/*';
end;

procedure THTTPSendThread.Reset;
begin
  ResetBasic;
  Headers.Values['DNT'] := '1';
  Headers.Values['Upgrade-Insecure-Requests'] := '1';
  Headers.Values['Accept'] := 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8';
  Headers.Values['Accept-Language'] := 'en-US,en;q=0.5';
  Headers.Values['Accept-Charset'] := 'utf-8';
end;

procedure THTTPSendThread.ResetBasic;
begin
  FRangeStart := 0;
  FRangeEnd := 0;
  FDocument.Clear;
  FHeaders.Clear;
  FCookies.Clear;
  FDownloadSize := 0;
  FUploadSize := 0;
  FMimeType := 'text/html';   // MimeType always replaced by received content-type header after each succesful request
  FSock.CloseSocket;
  if FCompress then Headers.Values['Accept-Encoding'] := 'gzip, deflate, br';
end;

procedure THTTPSendThread.ClearCookies;
begin
  Cookies.Clear;
  FClearCookies:=True;
end;

procedure THTTPSendThread.ClearCookiesStorage;
begin
  Cookies.Clear;
  if CookieManager<>nil then
    CookieManager.Clear;
end;

procedure THTTPSendThread.SaveDocumentToFile(const AFileName: String;
  const ATryOriginalFileName: Boolean; const ALastModified: TDateTime);
var
  f: String;
  d: TDateTime;
begin
  d := ALastModified;
  if d = -1 then
    d := GetLastModified;
  f := '';
  if ATryOriginalFileName then
    f := GetOriginalFileName;
  if f = '' then f := AFileName;
  Document.SaveToFile(f);
  if FileExists(AFileName) then
    FileSetDate(AFileName, DateTimeToFileDate(d));
end;

initialization
  InitCriticalSection(CS_ALLHTTPSendThread);
  ALLHTTPSendThread := TFPList.Create;

finalization
  ALLHTTPSendThread.Free;
  DoneCriticalsection(CS_ALLHTTPSendThread);

end.
