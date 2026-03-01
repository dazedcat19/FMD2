{
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit WebsiteModules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uData, uDownloadsManager, FMDOptions, httpsendthread,
  WebsiteModulesSettings, LuaWebsiteBypass, RegExpr, fpjson, jsonparser,
  jsonscanner, fpjsonrtti, uBaseUnit, httpcookiemanager, BaseThread, syncobjs;

const
  MODULE_NOT_FOUND = -1;
  NO_ERROR = 0;
  NET_PROBLEM = 1;
  INFORMATION_NOT_FOUND = 2;

type

  TModuleContainer = class;
  TUpdateListManagerThread = TObject;

  TOnBeforeUpdateList = function(const AModule: TModuleContainer): Boolean;
  TOnAfterUpdateList = function(const AModule: TModuleContainer): Boolean;
  TOnGetDirectoryPageNumber = function(const AUpdateListManager: TUpdateListManagerThread;
    const AHTTP: THTTPSendThread;
    var APage: Integer;
    const AWorkPtr: Integer;
    const AModule: TModuleContainer): Byte;
  TOnGetNameAndLink = function(const AUpdateListManager: TUpdateListManagerThread;
    const AHTTP: THTTPSendThread;
    const ANames, ALinks: TStringList; const AURL: String;
    const AModule: TModuleContainer): Byte;
  TOnGetInfo = function(const AMangaInfo: TMangaInformation; const AURL: String;
    const AModule: TModuleContainer): Byte;

  TOnTaskStart = function(const ATask: TTaskContainer; const AModule: TModuleContainer): Boolean;
  TOnGetPageNumber = function(const ATaskThread: TTaskThread;
    const AURL: String; const AModule: TModuleContainer): Boolean;
  TOnGetImageURL = function(const ADownloadThread: TDownloadThread;
    const AURL: String; const AModule: TModuleContainer): Boolean;

  TOnBeforeDownloadImage = function(const ADownloadThread: TDownloadThread;
    var AURL: String; const AModule: TModuleContainer): Boolean;

  TOnDownloadImage = function(const ADownloadThread: TDownloadThread;
    const AURL: String; const AModule: TModuleContainer): Boolean;

  TOnSaveImage = function(const ADownloadThread: TDownloadThread;
    const APath, AName: String; const AModule: TModuleContainer): String;

  TOnAfterImageSaved = function(const ADownloadThread: TDownloadThread;
    const AFileName: String; const AModule: TModuleContainer): Boolean;

  TOnLogin = function(const AHTTP: THTTPSendThread; const AModule: TModuleContainer): Boolean;
  TOnAccountState = function(const AModule: TModuleContainer): Boolean;
  TOnCheckSite = function(const AModule: TModuleContainer): Boolean;

  TModuleMethod = (MMGetDirectoryPageNumber, MMGetNameAndLink, MMGetInfo,
    MMTaskStart, MMGetPageNumber, MMGetImageURL, MMBeforeDownloadImage,
    MMDownloadImage, MMSaveImage, MMAfterImageSaved, MMLogin);

  TWebsiteOptionType = (woCheckBox, woEdit, woSpinEdit, woComboBox);

  TWebsiteOptionItem = record
    OptionType: TWebsiteOptionType;
    Name: String;
    Caption: PString;
    BindValue: Pointer;
    Items: PString;
  end;

  TAccountStatus = (asUnknown, asChecking, asValid, asInvalid);

  { TWebsiteModuleAccount }

  TWebsiteModuleAccount = class
  private
    FEnabled: Boolean;
    FStatus: TAccountStatus;
    FUsername,
    FPassword,
    FCookies: String;
  public
    Guardian: TCriticalSection;
    constructor Create;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Username: String read FUsername write FUsername;
    property Password: String read FPassword write FPassword;
    property Status: TAccountStatus read FStatus write FStatus;
    property Cookies: String read FCookies write FCookies;
  end;

  PModuleContainer = ^TModuleContainer;

  { TModuleContainer }

  TModuleContainer = class
  private
    FAccount: TWebsiteModuleAccount;
    FAccountSupport: Boolean;
    FSettings: TWebsiteModuleSettings;
    FTotalDirectory: Integer;
    FWebsiteBypass: TWebsiteBypass;
    FCookieManager: THTTPCookieManager;
    FConnectionsQueue: THTTPQueue;
    procedure SetAccountSupport(AValue: Boolean);
    function WebsiteBypassHTTPRequest(const AHTTP: THTTPSendThread; const Method, URL: String; const Response: TObject = nil): Boolean;
    procedure MergeHTTPCookiesFromSetting(const AHTTP: THTTPSendThread);
    procedure SetTotalDirectory(AValue: Integer);
    procedure AddOption(const AOptionType: TWebsiteOptionType;
      const ABindValue: Pointer; const AName: String; const ACaption: PString; const AItems: PString = nil);
  public
    Guardian: TCriticalSection;
    Tag: Integer;
    TagPtr: Pointer;
    LuaModule: Pointer;
    ID: String;
    Name: String;
    RootURL: String;
    Category: String;
    ActiveTaskCount: Integer;
    SortedList: Boolean;
    InformationAvailable: Boolean;
    FavoriteAvailable: Boolean;
    DynamicPageLink: Boolean;
    TotalDirectoryPage: array of Integer;
    CurrentDirectoryIndex: Integer;
    MaxTaskLimit: Integer;
    MaxThreadPerTaskLimit: Integer;
    OptionList: array of TWebsiteOptionItem;
    OnBeforeUpdateList: TOnBeforeUpdateList;
    OnAfterUpdateList: TOnAfterUpdateList;
    OnGetDirectoryPageNumber: TOnGetDirectoryPageNumber;
    OnGetNameAndLink: TOnGetNameAndLink;
    OnGetInfo: TOnGetInfo;
    OnTaskStart: TOnTaskStart;
    OnGetPageNumber: TOnGetPageNumber;
    OnGetImageURL: TOnGetImageURL;
    OnBeforeDownloadImage: TOnBeforeDownloadImage;
    OnDownloadImage: TOnDownloadImage;
    OnSaveImage: TOnSaveImage;
    OnAfterImageSaved: TOnAfterImageSaved;
    OnLogin: TOnLogin;
    OnAccountState: TOnAccountState;
    OnCheckSite: TOnCheckSite;
    constructor Create;
    destructor Destroy; override;
  public
    property TotalDirectory: Integer read FTotalDirectory write SetTotalDirectory;
    procedure AddOptionCheckBox(const ABindValue: PBoolean; const AName: String;
      const ACaption: PString);
    procedure AddOptionEdit(const ABindValue: PString; const AName: String;
      const ACaption: PString);
    procedure AddOptionSpinEdit(const ABindValue: PInteger; const AName: String;
      const ACaption: PString);
    procedure AddOptionComboBox(const ABindValue: PInteger; const AName: String;
      const ACaption, AItems: PString);
    procedure PrepareHTTP(const AHTTP: THTTPSendThread);
    function CreateHTTP(const AOwner: TBaseThread = nil): THTTPSendThread; inline;

    procedure IncActiveTaskCount; inline;
    procedure DecActiveTaskCount; inline;

    function GetMaxTaskLimit: Integer;
    function GetMaxThreadPerTaskLimit: Integer;

    function CanCreateTask: Boolean;

    property Settings: TWebsiteModuleSettings read FSettings write FSettings;
    property AccountSupport: Boolean read FAccountSupport write SetAccountSupport;
    property Account: TWebsiteModuleAccount read FAccount write FAccount;
    property CookieManager: THTTPCookieManager read FCookieManager;
    property ConnectionsQueue: THTTPQueue read FConnectionsQueue;
  end;

  TModuleContainers = specialize TFPGList<TModuleContainer>;

  { TWebsiteModules }

  TWebsiteModules = class
  private
    FGuardian: TRTLCriticalSection;
    FModuleList: TModuleContainers;
    FLastLocateModule: TModuleContainer;
    function GetModule(const AModuleIndex: Integer): TModuleContainer;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Sort;
    procedure Lock; inline;
    procedure Unlock; inline;
    procedure LoadFromFile;
    procedure SaveToFile;
    function LocateModule(const AModuleID: String): TModuleContainer;
    function LocateModuleByHost(const AHost: String): TModuleContainer;
    property List: TModuleContainers read FModuleList;
    property Count: Integer read GetCount;
    property Module[const AModuleIndex: Integer]: TModuleContainer read GetModule; default;
  end;

var
  Modules: TWebsiteModules;

procedure doInitialize;

function CleanOptionName(const S: String): String;


implementation

var
  CS_Connection: TRTLCriticalSection;

function CleanOptionName(const S: String): String;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  Num = ['0'..'9'];
  AlphaNum = Alpha + Num;
var
  i: Integer;
begin
  Result := Trim(S);
  if Result = '' then Exit;
  while (Length(Result) > 0) and (Result[1] in Num) do
    Delete(Result, 1, 1);
  i := 1;
  while i <= Length(Result) do
    if not (Result[i] in AlphaNum) then
      Delete(Result, i, 1)
    else
      Inc(i);
end;

{ TWebsiteModuleAccount }

constructor TWebsiteModuleAccount.Create;
begin
  Guardian := TCriticalSection.Create;
end;

destructor TWebsiteModuleAccount.Destroy;
begin
  Guardian.Free;
  inherited Destroy;
end;

{ TModuleContainer }

procedure TModuleContainer.SetAccountSupport(AValue: Boolean);
begin
  if FAccountSupport = AValue then Exit;
  FAccountSupport := AValue;
  if FAccountSupport then
  begin
    if FAccount = nil then
      FAccount := TWebsiteModuleAccount.Create;
  end
  else
  if FAccount<>nil then
    FAccount.Free;
end;

function TModuleContainer.WebsiteBypassHTTPRequest(const AHTTP: THTTPSendThread;
  const Method, URL: String; const Response: TObject): Boolean;
begin
  Result := WebsiteBypassRequest(AHTTP, Method, URL, Response, FWebsiteBypass);
end;

procedure TModuleContainer.MergeHTTPCookiesFromSetting(const AHTTP: THTTPSendThread);
begin
  if Settings.Enabled and (Settings.HTTP.Cookies <> '') then
    AHTTP.MergeCookies(Settings.HTTP.Cookies);
end;

procedure TModuleContainer.SetTotalDirectory(AValue: Integer);
var
  i: Integer;
begin
  if FTotalDirectory = AValue then Exit;
  FTotalDirectory := AValue;
  SetLength(TotalDirectoryPage, FTotalDirectory);
  if Length(TotalDirectoryPage) > 0 then
    for i := Low(TotalDirectoryPage) to High(TotalDirectoryPage) do
      TotalDirectoryPage[i] := 1;
end;

constructor TModuleContainer.Create;
begin
  Guardian := TCriticalSection.Create;
  ActiveTaskCount := 0;
  AccountSupport := False;
  SortedList := False;
  InformationAvailable := True;
  FavoriteAvailable := True;
  DynamicPageLink := False;
  TotalDirectory := 1;
  CurrentDirectoryIndex := 0;
  FWebsiteBypass := TWebsiteBypass.Create(self);
  FCookieManager := THTTPCookieManager.Create;
  FConnectionsQueue := THTTPQueue.Create;
  FSettings := TWebsiteModuleSettings.Create;
  FSettings.ConnectionsQueue := FConnectionsQueue;
end;

destructor TModuleContainer.Destroy;
begin
  SetLength(TotalDirectoryPage, 0);
  SetLength(OptionList,0);
  if Assigned(FWebsiteBypass) then
    FWebsiteBypass.Free;
  if Assigned(FAccount) then
    FAccount.Free;
  FSettings.Free;
  Guardian.Free;
  FCookieManager.Free;
  FConnectionsQueue.Free;
  inherited Destroy;
end;

procedure TModuleContainer.AddOptionCheckBox(const ABindValue: PBoolean;
  const AName: String; const ACaption: PString);
begin
  AddOption(woCheckBox, ABindValue, AName, ACaption);
end;

procedure TModuleContainer.AddOptionEdit(const ABindValue: PString; const AName: String;
  const ACaption: PString);
begin
  AddOption(woEdit, ABindValue, AName, ACaption);
end;

procedure TModuleContainer.AddOptionSpinEdit(const ABindValue: PInteger;
  const AName: String; const ACaption: PString);
begin
  AddOption(woSpinEdit, ABindValue, AName, ACaption);
end;

procedure TModuleContainer.AddOptionComboBox(const ABindValue: PInteger;
  const AName: String; const ACaption, AItems: PString);
begin
  AddOption(woComboBox, ABindValue, AName, ACaption, AItems);
end;

procedure TModuleContainer.PrepareHTTP(const AHTTP: THTTPSendThread);
var
  s: String;
begin
  AHTTP.CookieManager := FCookieManager;
  AHTTP.ConnectionsQueue := FConnectionsQueue;
  AHTTP.OnHTTPRequest := @WebsiteBypassHTTPRequest;
  AHTTP.OnAfterSetHTTPCookies := @MergeHTTPCookiesFromSetting;

  if not Settings.Enabled then exit;
  with Settings.HTTP do
  begin
    if UserAgent<>'' then
      AHTTP.UserAgent:=UserAgent;
    with Proxy do
    begin
      case Proxy.ProxyType of
        ptDirect:AHTTP.SetNoProxy;
        ptHTTP:s:='HTTP';
        ptSOCKS4:s:='SOCKS4';
        ptSOCKS5:s:='SOCKS5';
        else s:='';
      end;
      if s<>'' then
        AHTTP.SetProxy(s,ProxyHost,ProxyPort,ProxyUsername,ProxyPassword);
    end;
  end;
end;

function TModuleContainer.CreateHTTP(const AOwner: TBaseThread): THTTPSendThread;
begin
  Result := THTTPSendThread.Create(AOwner);
  PrepareHTTP(Result);
end;

procedure TModuleContainer.IncActiveTaskCount;
begin
  InterLockedIncrement(ActiveTaskCount);
end;

procedure TModuleContainer.DecActiveTaskCount;
begin
  InterLockedDecrement(ActiveTaskCount);
end;

function TModuleContainer.GetMaxTaskLimit: Integer;
begin
  if (Settings.Enabled) and (Settings.MaxTaskLimit <> 0)  then
    Result:=Settings.MaxTaskLimit
  else
    Result:=MaxTaskLimit;
end;

function TModuleContainer.GetMaxThreadPerTaskLimit: Integer;
begin
  if (Settings.Enabled) and (Settings.MaxThreadPerTaskLimit <> 0)  then
    Result:=Settings.MaxThreadPerTaskLimit
  else
    Result:=MaxThreadPerTaskLimit;
end;

function TModuleContainer.CanCreateTask: Boolean;
begin
  if GetMaxTaskLimit > 0 then
    Result := ActiveTaskCount < GetMaxTaskLimit
  else
    Result := True;
end;

procedure TModuleContainer.AddOption(const AOptionType: TWebsiteOptionType;
  const ABindValue: Pointer; const AName: String; const ACaption: PString;
  const AItems: PString);
begin
  if ABindValue = nil then Exit;
  if AName = '' then Exit;
  SetLength(OptionList, Length(OptionList) + 1);
  with OptionList[High(OptionList)] do
  begin
    OptionType := AOptionType;
    BindValue := ABindValue;
    Name := CleanOptionName(AName);
    Caption := ACaption;
    Items := AItems;
  end;
end;

{ TWebsiteModules }

constructor TWebsiteModules.Create;
begin
  InitCriticalSection(FGuardian);
  FModuleList := TModuleContainers.Create;
  FLastLocateModule := nil;
end;

destructor TWebsiteModules.Destroy;
var
  i: Integer;
begin
  if FModuleList.Count > 0 then
    for i := FModuleList.Count - 1 downto 0 do
      FModuleList[i].Free;
  FModuleList.Free;
  DoneCriticalsection(FGuardian);
  inherited Destroy;
end;

function TModuleContainerCompare(const Item1, Item2: TModuleContainer): Integer;
begin
  Result := AnsiCompareStr(Item1.ID, Item2.ID);
end;

procedure TWebsiteModules.Sort;
begin
  FModuleList.Sort(@TModuleContainerCompare);
end;

function TWebsiteModules.LocateModule(const AModuleID: String): TModuleContainer;
var
  L, R, I: Integer;
  CompareRes: PtrInt;
begin
  if Assigned(FLastLocateModule) and (FLastLocateModule.ID = AModuleID) then
    Exit(FLastLocateModule);
  // use binary search, must be sorted
  Result := nil;
  L := 0;
  R := FModuleList.Count - 1;
  while (L<=R) do
  begin
    I := L + (R - L) div 2;
    CompareRes := AnsiCompareStr(AModuleID, FModuleList[I].ID);
    if (CompareRes>0) then
      L := I+1
    else begin
      R := I-1;
      if (CompareRes=0) then
      begin
        Result := FModuleList[I];
        L := I;
      end;
    end;
  end;
  if Assigned(Result) then
    InterlockedExchange(Pointer(FLastLocateModule), Pointer(Result));
end;

function TWebsiteModules.LocateModuleByHost(const AHost: String
  ): TModuleContainer;

  function PosModule(const s: String): TModuleContainer;
  var
    i: Integer;
  begin
    for i := FModuleList.Count - 1 downto 0 do
      if Pos(s, FModuleList[i].RootURL) <> 0 then
        Exit(FModuleList[i]);
    Result := nil;
  end;
var
  h: String;
  expr: TRegExpr;
begin
  h := LowerCase(AHost);
  if Assigned(FLastLocateModule) and (Pos(h, FLastLocateModule.RootURL) <> 0) then
    Exit(FLastLocateModule);
  Result := PosModule(h);
  if Result = nil then
  begin
    SplitURL(h, @h, nil, False, False);
    if h = '' then Exit;
    Result := PosModule(h);
    // if host starts with www. or w**. try without it
    expr := TRegExpr.Create('w+\d*');
    if (Result = nil) and (h.StartsWith('www.') or expr.Exec(h)) then
      Result := PosModule(h.Substring(4));
    expr.Free;
  end;
  if Assigned(Result) then
    InterlockedExchange(Pointer(FLastLocateModule), Pointer(Result));
end;

procedure TWebsiteModules.Lock;
begin
  EnterCriticalsection(FGuardian);
end;

procedure TWebsiteModules.Unlock;
begin
  LeaveCriticalsection(FGuardian);
end;

procedure TWebsiteModules.LoadFromFile;
var
  i, j, k: Integer;
  jd: TJSONDeStreamer;
  ja: TJSONArray;
  fs: TFileStream;
  jp: TJSONParser;
  jo, jo2: TJSONObject;
  j_cookies: TJSONArray;
  c: THTTPCookie;
begin
  if FModuleList.Count=0 then Exit;
  if not FileExists(MODULES_FILE) then Exit;

  ja:=nil;
  try
    fs:=TFileStream.Create(MODULES_FILE,fmOpenRead or fmShareDenyWrite);
    try
      jp:=TJSONParser.Create(fs,[joUTF8]);
      ja:=TJSONArray(jp.Parse);
    finally
      jp.Free;
    end;
  finally
    fs.Free;
  end;

  if (ja<>nil) and (ja.Count<>0) then
    try
      jd:=TJSONDeStreamer.Create(nil);
      jd.Options:=jd.Options+[jdoIgnorePropertyErrors];
      for i:=FModuleList.Count-1 downto 0 do
        with FModuleList[i] do
        begin
          jo:=nil;
          for j:=ja.Count-1 downto 0 do
            if ja.Objects[j].Get('ID','')=ID then
            begin
              jo:=ja.Objects[j];
              Break;
            end;
          if jo<>nil then
          begin
            jo2:=jo.Get('Settings',TJSONObject(nil));
            if jo2<>nil then
              jd.JSONToObject(jo2,Settings);
            if Length(OptionList)<>0 then
            begin
              jo2:=jo.Get('Options',TJSONObject(nil));
              if jo2<>nil then
                for k:=Low(OptionList) to High(OptionList) do
                  with OptionList[k],jo2 do
                    case OptionType of
                      woCheckBox:PBoolean(BindValue)^:=Get(Name,PBoolean(BindValue)^);
                      woEdit:PString(BindValue)^:=Get(Name,PString(BindValue)^);
                      woSpinEdit,woComboBox:PInteger(BindValue)^:=Get(Name,PInteger(BindValue)^);
                    end;
            end;
            if Account<>nil then
            begin
              jo2:=jo.Get('Account',TJSONObject(nil));
              if jo2<>nil then
              begin
                jd.JSONToObject(jo2,Account);
                if Account.Username<>'' then Account.Username := DecryptString(Account.Username);
                if Account.Password<>'' then Account.Password := DecryptString(Account.Password);
                if Account.Cookies<>'' then Account.Cookies := DecryptString(Account.Cookies);
                if Account.Status=asChecking then
                  Account.Status:=asUnknown;
              end;
            end;
            j_cookies:=jo.Get('Cookies',TJSONArray(nil));
            if Assigned(j_cookies) then
            begin
              for k:=0 to j_cookies.Count-1 do
              begin
                c:=THTTPCookie.Create;
                jd.JSONToObject(TJSONObject(j_cookies.Items[k]), c);
                CookieManager.AddCookie(c, True);
              end;
            end;
            ja.Delete(j);
          end;
        end;
    finally
      jd.Free;
      ja.Free;
    end;
end;

procedure TWebsiteModules.SaveToFile;
var
  i, j: Integer;
  js: TJSONStreamer;
  ja, j_cookies: TJSONArray;
  fs: TMemoryStream;
  jo: TJSONObject;
  jo2: TJSONObject;
begin
  if FModuleList.Count=0 then Exit;
  ja:=TJSONArray.Create;
  js:=TJSONStreamer.Create(nil);
  js.Options:=js.Options+[jsoDateTimeAsString];
  try
    for i:=0 to FModuleList.Count-1 do
      with FModuleList[i] do
      begin
        jo:=TJSONObject.Create;
        ja.Add(jo);
        jo.Add('ID',ID);
        jo.Add('Settings',js.ObjectToJSON(Settings));
        if Length(OptionList) <> 0 then
        begin
          jo2:=TJSONObject.Create;
          jo.Add('Options',jo2);
          for j:=Low(OptionList) to High(OptionList) do
            with OptionList[j],jo2 do
              case OptionType of
                woCheckBox:Add(Name,PBoolean(BindValue)^);
                woEdit:Add(Name,PString(BindValue)^);
                woSpinEdit,woComboBox:Add(Name,PInteger(BindValue)^);
              end;
        end;
        if Account<>nil then
        begin
          jo2:=js.ObjectToJSON(Account);
          jo2.Strings['Username']:=EncryptString(Account.Username);
          jo2.Strings['Password']:=EncryptString(Account.Password);
          jo2.Strings['Cookies']:=EncryptString(Account.Cookies);
          jo.Add('Account',jo2);
        end;
        j_cookies:=TJSONArray.Create;
        for j:=0 to CookieManager.Cookies.Count-1 do
        begin
          if CookieManager.Cookies[j].Persistent then
            j_cookies.Add(js.ObjectToJSON(CookieManager.Cookies[j]));
        end;
        jo.Add('Cookies', j_cookies);
      end;
    fs:=TMemoryStream.Create;
    try
      ja.DumpJSON(fs);
      fs.SaveToFile(MODULES_FILE);
    finally
      fs.Free;
    end;
  finally
    ja.Free;
    js.Free;
  end;
end;

function TWebsiteModules.GetModule(const AModuleIndex: Integer
  ): TModuleContainer;
begin
  if (AModuleIndex < 0) or (AModuleIndex >= FModuleList.Count) then Exit(nil);
  Result := FModuleList[AModuleIndex];
end;

function TWebsiteModules.GetCount: Integer;
begin
  Result := FModuleList.Count;
end;

procedure doInitialize;
begin
  if Modules = nil then
    Modules := TWebsiteModules.Create;
end;

initialization
  InitCriticalSection(CS_Connection);
  doInitialize;

finalization
  if Assigned(Modules) then
    FreeAndNil(Modules);
  DoneCriticalsection(CS_Connection);

end.
