unit GitHubRepoV3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, httpsendthread, BaseThread, fpjson;

type
{ TGitHubRepo }

  TTreeItem = class
    path,
    sha: String;
  end;

  TTreeItems = specialize TFPGObjectList<TTreeItem>;

  TGitHubRepo = class
  private
    FDirty: boolean;
    ConfigFile: String;
    WorkFile: String;
    HTTP: THTTPSendThread;
  protected
    procedure ReadWorkFile;
    procedure WriteWorkFile;
  public
    api_url,
    download_url,
    owner,
    name,
    ref,
    path: String;

    last_commit_sha,
    last_commit_etag,
    tree_sha,
    tree_etag: String;

    Tree: TTreeItems;
  public
    constructor Create(const AConfigFile, AWorkFile: String; const AThread: TBaseThread = nil);
    destructor Destroy; override;
    function GetLastCommit: String;
    function GetLastCommitMessage(const FRepoPath: String): String;
    function GetTree: Boolean;
    function GetUpdate: Boolean;
    function GetDownloadURL(const AName: String): String;
  end;

implementation

uses jsonConf, jsonini, uBaseUnit;

function AppendURLDelim(const URL: String): String;
begin
  Result := URL;
  if (URL <> '') and (URL[Length(URL)] <> '/') then
  begin
    Result := URL + '/';
  end;
end;

{ TGitHubRepo }

procedure TGitHubRepo.ReadWorkFile;
begin
  if FileExists(WorkFile) then
  begin
    with TJSONConfig.Create(nil) do
    begin
      try
        Filename := WorkFile;
        last_commit_sha := GetValue('last_commit_sha', '');
        last_commit_etag := GetValue('last_commit_etag', '');
        FDirty := False;
      except
      end;
      Free;
    end;
  end;
end;

procedure TGitHubRepo.WriteWorkFile;
begin
  if not FDirty then
  begin
    Exit;
  end;

  if FileExists(WorkFile) then
  begin
    DeleteFile(WorkFile);
  end;

  with TJSONConfig.Create(nil) do
  begin
    try
      Filename := WorkFile;
      FormatOptions := AsCompressedJSON;
      Formatted := True;
      SetValue('last_commit_sha' , last_commit_sha);
      SetValue('last_commit_etag', last_commit_etag);
      FDirty := false;
    except
    end;
    Free;
  end;
end;

constructor TGitHubRepo.Create(const AConfigFile, AWorkFile: String;
  const AThread: TBaseThread);
begin
  ConfigFile := AConfigFile;
  WorkFile := AWorkFile;

  HTTP := THTTPSendThread.Create(AThread);
  HTTP.FollowRedirection := False;
  HTTP.UserAgent := UserAgentCURL;
  HTTP.ResetBasic;

  if FileExists(ConfigFile) then
  begin
    with TJSONIniFile.Create(ConfigFile) do
    begin
      try
        api_url := ReadString('GitHub', 'api_url', '');
        download_url := ReadString('GitHub', 'download_url', '');
        owner := ReadString('GitHub', 'owner', '');
        name := ReadString('GitHub', 'name' , '');
        ref := ReadString('GitHub', 'ref', '');
        path := ReadString('GitHub', 'path', '');
      finally
        Free;
      end;
    end;
  end;

  if api_url = '' then
  begin
    api_url := 'https://api.github.com/';
  end;

  if ref = '' then
  begin
    ref := 'master';
  end;

  ReadWorkFile;
  Tree := TTreeItems.Create;
end;

destructor TGitHubRepo.Destroy;
begin
  Tree.Free;
  HTTP.Free;
  WriteWorkFile;
  inherited Destroy;
end;

function TGitHubRepo.GetLastCommit: String;
var
  s: String;
  d: TJSONData;
begin
  Result := '';
  HTTP.ResetBasic;
  // use conditional etag, ignore if return 304 not modified
  // https://developer.github.com/v3/#conditional-requests
  if last_commit_etag <> '' then
  begin
    HTTP.Headers.Values['If-None-Match'] := ' ' + last_commit_etag;
  end;

  s := AppendURLDelim(api_url) + 'repos/' + owner + '/' + name + '/commits?sha=' + ref + '&per_page=1';
  if path <> '' then
  begin
    s += '&path=' + path;
  end;

  if HTTP.GET(s) then
  begin
    s := Trim(HTTP.Headers.Values['ETag']);
    if s <> '' then
    begin
      last_commit_etag := s;
    end;

    d := GetJSON(HTTP.Document);
    if Assigned(d) then
    begin
      try
        if d.JSONType = jtArray then
        begin
          last_commit_sha := TJSONObject(TJSONArray(d).Items[0]).Get('sha');
        end;
      except
      end;
      d.Free;
    end;
  end;
  Result := last_commit_sha;
end;

function TGitHubRepo.GetLastCommitMessage(const FRepoPath: String): String;
var
  s, message: String;
  d: TJSONData;
begin
  Result := '';
  message := '';
  HTTP.ResetBasic;
  s := AppendURLDelim(api_url) + 'repos/' + owner + '/' + name + '/commits?per_page=1';
  if path <> '' then
  begin
    s += '&path=' + path + '/' + FRepoPath;
  end;

  if HTTP.GET(s) then
  begin
    d := GetJSON(HTTP.Document);
    if Assigned(d) then
    begin
      try
        if d.JSONType = jtArray then
        begin
          message := TJSONObject(TJSONArray(d).Items[0]).FindPath('commit.message').AsString;
        end;
      except
      end;
      d.Free;
    end;
  end;

  Result := message;
end;

function TGitHubRepo.GetTree: Boolean;
var
  d: TJSONData;
  a: TJSONArray;
  s: String;
  i: Integer;
  item: TTreeItem;
begin
  Result := False;
  HTTP.ResetBasic;

  s := last_commit_sha;
  if s = '' then
  begin
    s := ref;
  end;

  s := AppendURLDelim(api_url) + 'repos/' + owner + '/' + name + '/git/trees/' + s + ':' + path + '?recursive=1';
  if HTTP.GET(s) then
  begin
    d := GetJSON(HTTP.Document);
    if Assigned(d) then
    begin
      try
        a := TJSONArray(d.GetPath('tree'));
        Tree.Clear;
        for i := 0 to a.Count-1 do
        begin
          with TJSONObject(a.Items[i]) do
          begin
            s := Get('type');

            if s <> 'tree' then
            begin
              item := TTreeItem.Create;
              item.path := Get('path');
              item.sha := Get('sha');
              Tree.Add(item);
            end;
          end;
        end;
      except
      end;
      d.Free;
    end;
    Result := Tree.Count <> 0;
  end
end;

function TGitHubRepo.GetUpdate: Boolean;
var
  old_commit_sha, new_commit_sha: String;
begin
  Result := False;
  old_commit_sha := last_commit_sha;
  new_commit_sha := GetLastCommit;
  if (new_commit_sha <> '') and (new_commit_sha <> old_commit_sha) then
  begin
    Result := GetTree;
  end;
  FDirty := Result;
end;

function TGitHubRepo.GetDownloadURL(const AName: String): String;
var
  lpath: String;
begin
  lpath := path;
  if lpath <> '' then
  begin
    lpath := lpath + '/';
  end;
  Result := AppendURLDelim(download_url) + owner + '/' + name + '/' + ref + '/' + lpath + AName;
end;

end.

