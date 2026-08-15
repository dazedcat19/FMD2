unit GitHubRepo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpsendthread, BaseThread, fpjson;

type
{ TGitHubRepo }

  TGitHubRepo = class
  private
    ConfigFile: String;
    HTTP: THTTPSendThread;
    Tree: TJSONArray;
    Props: TJSONObject;
  protected
    procedure SetAuth;
    function QueryLastCommit: String;
    function QueryTree: String;
    function QueryProps(const ANames: TStrings): String;
  public
    api_url,
    download_url,
    owner,
    name,
    token,
    ref,
    path,
    last_commit: String;
    max_deep: Integer;
    constructor Create(const AConfigFile: String; const AThread: TBaseThread = nil);
    destructor Destroy; override;
    function GetLastCommit: String;
    function GetTree: TJSONArray;
    function GetDownloadURL(const AName: String): String;
    function GetProps(const ANames: TStrings): TJSONObject;
  end;

implementation

uses
  jsonparser, jsonscanner, jsonini, uBaseUnit;

{ TGitHubRepo }

procedure TGitHubRepo.SetAuth;
begin
  HTTP.Headers.Values['Authorization']:=' bearer '+token;
end;

function TGitHubRepo.QueryLastCommit: String;
var
  lpath: String;
begin
  lpath:=path; if lpath<>'' then lpath := ', path: "'+lpath+'"';
  Result := '{"query": "'+StringToJSONString(
               '{'+
                 'repository(owner: "'+owner+'", name: "'+name+'") {'+
                   'ref(qualifiedName: "'+ref+'") {'+
                     'target {'+
                       '... on Commit {'+
                         'history(first: 1'+lpath+'){ '+
                           'nodes {'+
                             'oid'+
                           '}'+
                         '}'+
                       '}'+
                     '}'+
                   '}'+
                 '}'+
               '}'
         )+'"}';
end;

function TGitHubRepo.QueryTree: String;

  function onTree(const x: Integer):string;
  begin
    Result:= '... on Tree {'+
                'entries {'+
                  'oid '+
                  'name ';
    if x>1 then
      Result+=    'object {'+
                    onTree(x-1)+
                  '}';
    Result+=    '}'+
             '}';
  end;
begin
  Result := '{"query": "'+StringToJSONString(
              '{'+
                'repository(owner: "'+owner+'", name: "'+name+'") {'+
                  'object(expression: "'+last_commit+':'+path+'") {'+
                    onTree(max_deep)+
                  '}'+
                '}'+
              '}'
         )+'"}';
end;

function TGitHubRepo.QueryProps(const ANames: TStrings): String;
  function onProps:String;
  var
    i: Integer;
    lpath: String;
  begin
    Result:='';
    lpath :=path; if lpath<>'' then lpath+='/';
    for i:=0 to ANames.Count-1 do
    begin
      Result += 'p'+IntToStr(i)+': history(path: "'+lpath+ANames[i]+'", first: 1) {'+
                  'nodes {'+
                    'message '+
                    'committedDate'+
                  '}'+
                '} ';
    end;
  end;
begin
  Result := '{"query": "'+StringToJSONString(
              '{'+
                'repository(owner: "'+owner+'", name: "'+name+'") {'+
                  'object(oid: "'+last_commit+'") {'+
                    '... on Commit {'+
                      onProps+
                    '}'+
                  '}'+
                '}'+
              '}'
         )+'"}';
end;

constructor TGitHubRepo.Create(const AConfigFile: String;
  const AThread: TBaseThread);
begin
  ConfigFile := AConfigFile;
  HTTP := THTTPSendThread.Create(AThread);
  if FileExists(ConfigFile) then
    with TJSONIniFile.Create(ConfigFile) do
      try
        api_url      := ReadString               ('GitHub', 'api_url'     , '');
        download_url := ReadString               ('GitHub', 'download_url', '');
        owner        := ReadString               ('GitHub', 'owner'       , '');
        name         := ReadString               ('GitHub', 'name'        , '');
        token        := DecryptString(ReadString ('GitHub', 'token'       , ''));
        ref          := ReadString               ('GitHub', 'ref'         , '');
        path         := ReadString               ('GitHub', 'path'        , '');
        max_deep     := ReadInteger              ('GitHub', 'max_deep'    , 2);
      finally
        Free;
      end;
  if api_url  = '' then api_url  := 'https://api.github.com/graphql';
  if ref      = '' then ref      := 'master';
  if max_deep < 1  then max_deep := 1;
  last_commit:= ref;
end;

destructor TGitHubRepo.Destroy;
begin
  if Assigned(Tree) then Tree.Free;
  if Assigned(Props) then Props.Free;
  HTTP.Free;
  inherited Destroy;
end;

function TGitHubRepo.GetLastCommit: String;
var
  d: TJSONData;
  a: TJSONArray;
begin
  last_commit:='';
  HTTP.Reset;
  SetAuth;
  d:=nil;
  if HTTP.POST(api_url, QueryLastCommit) then
    with TJSONParser.Create(HTTP.Document, [joUTF8]) do
      try
        d:=Parse;
      finally
        free;
      end;
  if Assigned(d) then
    try
      a:=TJSONArray(d.GetPath('data.repository.ref.target.history.nodes'));
      if Assigned(a) then
        last_commit:=TJSONObject(a.Items[0]).Get('oid','');
    except
    end;
    d.free;
  Result := last_commit;
end;

function TGitHubRepo.GetTree: TJSONArray;
var
  d: TJSONData;
  a: TJSONArray;
begin
  result:=nil;
  HTTP.Reset;
  SetAuth;
  if last_commit='' then
    last_commit := 'master';
  d:=nil;
  if HTTP.POST(api_url, QueryTree) then
    with TJSONParser.Create(HTTP.Document, [joUTF8]) do
      try
        d:=Parse;
      finally
        free;
      end;
  if Assigned(Tree) then
    FreeAndNil(Tree);
  if Assigned(d) then
  begin
    try
      a:=TJSONArray(d.GetPath('data.repository.object.entries'));
      if Assigned(a) then
        Tree:=TJSONArray(a.Clone);
    except
    end;
    d.free;
  end;
  Result := Tree;
end;

function TGitHubRepo.GetDownloadURL(const AName: String): String;
var
  lpath: String;
begin
  lpath:=path; if lpath<>'' then lpath:=lpath+'/';
  Result:=AppendURLDelim(download_url)+owner+'/'+name+'/'+ref+'/'+lpath+AName;
end;

function TGitHubRepo.GetProps(const ANames: TStrings): TJSONObject;
var
  d: TJSONData;
  o: TJSONObject;
begin
  result:=nil;
  HTTP.Reset;
  SetAuth;
  if last_commit='' then
    last_commit := 'master';
  d:=nil;
  if HTTP.POST(api_url, QueryProps(ANames)) then
    with TJSONParser.Create(HTTP.Document, [joUTF8]) do
      try
        d:=Parse;
      finally
        free;
      end;
  if Assigned(Props) then
    FreeAndNil(Props);
  if Assigned(d) then
  begin
    try
      o:=TJSONObject(d.GetPath('data.repository.object'));
      if Assigned(o) then
        Props:=TJSONObject(o.Clone);
    except
    end;
    d.free;
  end;
  Result := Props;
end;

end.

