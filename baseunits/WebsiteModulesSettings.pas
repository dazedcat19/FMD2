unit WebsiteModulesSettings;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpsendthread;

type
    TProxyType = (ptDefault, ptDirect, ptHTTP, ptSOCKS4, ptSOCKS5);

  { TProxySettings }

  TProxySettings = class
  private
    FProxyHost: String;
    FProxyPassword: String;
    FProxyPort: String;
    FProxyType: TProxyType;
    FProxyUsername: String;
  published
    property ProxyType: TProxyType read FProxyType write FProxyType default ptDefault;
    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: String read FProxyPort write FProxyPort;
    property ProxyUsername: String read FProxyUsername write FProxyUsername;
    property ProxyPassword: String read FProxyPassword write FProxyPassword;
  end;

  { THTTPSettings }

  THTTPSettings = class
  private
    FCookies: String;
    FProxy: TProxySettings;
    FUserAgent: String;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Cookies: String read FCookies write FCookies;
    property UserAgent: String read FUserAgent write FUserAgent;
    property Proxy: TProxySettings read FProxy write FProxy;
  end;
  
  { TOverrideSettings }
  
  TOverrideSettings = class
  private
    FSaveToPath: String;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SaveToPath: String read FSaveToPath write FSaveToPath;
  end;

  { TWebsiteModuleSettings }

  TWebsiteModuleSettings = class
  private
    FEnabled: Boolean;
    FHTTP: THTTPSettings;
    FOverrideSettings: TOverrideSettings;
    FMaxConnectionLimit: Integer;
    FMaxTaskLimit: Integer;
    FMaxThreadPerTaskLimit: Integer;
    FUpdateListDirectoryPageNumber: Integer;
    FUpdateListNumberOfThread: Integer;
    FDefaultMaxConnectionsLimit: Integer;
    FDefaultStored: Boolean;
    procedure StoreDefault;
    procedure SetEnabled(AValue: Boolean);
    procedure SetMaxConnectionLimit(AValue: Integer);
  public
    ConnectionsQueue: THTTPQueue;
    constructor Create;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property MaxTaskLimit: Integer read FMaxTaskLimit write FMaxTaskLimit default 0;
    property MaxThreadPerTaskLimit: Integer read FMaxThreadPerTaskLimit write FMaxThreadPerTaskLimit default 0;
    property MaxConnectionLimit: Integer read FMaxConnectionLimit write SetMaxConnectionLimit default 0;
    property UpdateListNumberOfThread: Integer read FUpdateListNumberOfThread write FUpdateListNumberOfThread default 0;
    property UpdateListDirectoryPageNumber: Integer read FUpdateListDirectoryPageNumber write FUpdateListDirectoryPageNumber default 0;
    property HTTP: THTTPSettings read FHTTP write FHTTP;
    property OverrideSettings: TOverrideSettings read FOverrideSettings write FOverrideSettings;
  end;

implementation

{ THTTPSettings }

constructor THTTPSettings.Create;
begin
  Proxy:=TProxySettings.Create;
end;

destructor THTTPSettings.Destroy;
begin
  Proxy.Free;
  inherited Destroy;
end;

{ TOverrideSettings }

constructor TOverrideSettings.Create;
begin
  
end;

destructor TOverrideSettings.Destroy;
begin
  inherited Destroy;
end;

{ TWebsiteModuleSettings }

procedure TWebsiteModuleSettings.StoreDefault;
begin
  if FDefaultStored then Exit;
  FDefaultMaxConnectionsLimit := ConnectionsQueue.MaxConnections;
  FDefaultStored := True;
end;

procedure TWebsiteModuleSettings.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if FEnabled then
  begin
    StoreDefault;
    if ConnectionsQueue.MaxConnections <> MaxConnectionLimit then
      ConnectionsQueue.MaxConnections := MaxConnectionLimit;
  end
  else
  begin
    if ConnectionsQueue.MaxConnections <> FDefaultMaxConnectionsLimit then
      ConnectionsQueue.MaxConnections := FDefaultMaxConnectionsLimit;
  end;
end;

procedure TWebsiteModuleSettings.SetMaxConnectionLimit(AValue: Integer);
begin
  if FMaxConnectionLimit = AValue then Exit;
  FMaxConnectionLimit := AValue;
  if FEnabled then
  begin
    StoreDefault;
    if ConnectionsQueue.MaxConnections <> MaxConnectionLimit then
      ConnectionsQueue.MaxConnections := MaxConnectionLimit;
  end;
end;

constructor TWebsiteModuleSettings.Create;
begin
  HTTP:=THTTPSettings.Create;
  OverrideSettings:=TOverrideSettings.Create;
  FEnabled := False;
  FDefaultStored := False;
end;

destructor TWebsiteModuleSettings.Destroy;
begin
  HTTP.Free;
  OverrideSettings.Free;
  inherited Destroy;
end;

end.

