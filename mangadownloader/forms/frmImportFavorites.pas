{
        File: frmImportFavorites.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit frmImportFavorites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StdCtrls, Buttons, EditBtn,
  LazFileUtils, uBaseUnit, WebsiteModules, FMDOptions, RegExpr,
  frmNewChapter, DBDataProcess, Controls, uCustomControls;

type

  { TImportFavorites }

  TImportFavorites = class(TForm)
    btImport: TBitBtn;
    btCancel: TBitBtn;
    cbSoftware: TComboBox;
    edPath: TCustomDirectoryEdit;
    lbSelectSoftware: TLabel;
    procedure btImportClick(Sender: TObject);
  private
    { private declarations }
    FImportFailed: Boolean;
    FUnimportedMangas: TStringList;

    procedure Run;
    procedure FMDHandle;
    procedure DMDHandle;
  public
    { public declarations }
  end;

resourcestring
  RS_ImportThreadStatus = 'Importing...';
  RS_ImportFailed = 'Import could not find a suitable file.';
  RS_ImportCompleted = 'Import completed.';
  RS_ListUnimportedCaption = 'List of unimported manga';

implementation

uses
  frmMain, frmCustomMessageDlg, uSilentThread, FMDVars;

{$R *.lfm}

{ TImportFavorites }

{ ----- private methods ----- }

procedure TImportFavorites.FMDHandle;
var
  db: TDBDataProcess;
  m: TModuleContainer;
  dbfile, fmd1path, fmd2path, dbfilepath, website, moduleid, link, title,
  saveto, status, currentchapter, downloadedchapterlist, favoriteenabled: String;
  hasModuleID, hasWebsite, hasStatus :Boolean;
  columnList: TStringList;
begin
  fmd1path := CleanAndExpandDirectory(edPath.Text) + 'works/favorites.db';
  fmd2path := CleanAndExpandDirectory(edPath.Text) + 'userdata/favorites.db';
  dbfilepath := CleanAndExpandDirectory(edPath.Text) + 'favorites.db';
  if FileExistsUTF8(fmd1path) then
  begin
    dbfile := fmd1path;
  end
  else if FileExistsUTF8(fmd2path) then
  begin
    dbfile := fmd2path;
  end
  else if FileExistsUTF8(dbfilepath) then
  begin
    dbfile := dbfilepath;
  end
  else
  begin
    FImportFailed := True;
    Exit;
  end;

  db := TDBDataProcess.Create;
  columnList := TStringList.Create;
  try
    if db.ConnectFile(dbfile) then
    begin
      hasModuleID := False;
      hasWebsite := False;
      db.Table.ReadOnly := True;
      db.Table.SQL.Text := 'PRAGMA table_info(favorites)'; // Get column metadata
      db.Table.Open;

      while not db.Table.EOF do
      begin
        columnList.Add(db.Table.FieldByName('name').AsString);
        db.Table.Next;
      end;

      hasModuleID := columnList.IndexOf('moduleid') <> -1;
      hasWebsite := columnList.IndexOf('website') <> -1;
      hasStatus := columnList.IndexOf('status') <> -1;
      db.Table.Close;

      db.Table.SQL.Text := 'SELECT * FROM favorites';
      db.Table.Open;

      while not db.Table.EOF do
      begin
        m := nil;
        if hasModuleID then
        begin
          moduleid := db.Table.FieldByName('moduleid').AsString;
        end;
        if hasWebsite then
        begin
          website := db.Table.FieldByName('website').AsString;
        end;
        if hasStatus then
        begin
          status := db.Table.FieldByName('status').AsString;
        end;
        link := db.Table.FieldByName('link').AsString;
        title := db.Table.FieldByName('title').AsString;
        saveto := db.Table.FieldByName('saveto').AsString;
        currentchapter := db.Table.FieldByName('currentchapter').AsString;
        downloadedchapterlist := db.Table.FieldByName('downloadedchapterlist').AsString;
        favoriteenabled := db.Table.FieldByName('enabled').AsString;

        if (hasModuleID) and (moduleid <> '') then
        begin
          m := Modules.LocateModule(moduleid);
        end
        else if (hasWebsite) and (website <> '') then
        begin
          m := Modules.LocateModuleByHost(website);
        end;
        if (hasStatus) and (status = '') then
        begin
          status := RS_InfoStatus_Unknown;
        end;

        if Assigned(m) then
        begin
          SilentThreadManager.Add(
            MD_ImportToFavorites,
            m,
            title,
            link,
            saveto,
            status,
            currentchapter,
            downloadedchapterlist,
            StrToBoolDef(favoriteenabled, True)
          );
        end
        else
        begin
          FUnimportedMangas.Add(title + ' <' + website + link + '>');
        end;

        db.Table.Next;
      end;
    end;
  finally
    db.Table.Close;
  end;

  db.Free;
end;

procedure TImportFavorites.DMDHandle;
var
  fstream  : TFileStream;
  list,
  urlList,
  mangaList: TStringList;
  host,
  bookmarksPath, path: String;
  i: Integer;
  regx: TRegExpr;
  m: TModuleContainer;
begin
  bookmarksPath := CleanAndExpandDirectory(edPath.Text) + 'Config/Bookmarks';
  if NOT FileExistsUTF8(bookmarksPath) then
  begin
    FImportFailed := True;
    Exit;
  end;

  list:= TStringList.Create;
  urlList:= TStringList.Create;
  mangaList:= TStringList.Create;
  fstream:= TFileStream.Create(bookmarksPath, fmOpenRead);

  list.LoadFromStream(fstream);
  if list.Count > 0 then
  begin
    for i:= 0 to list.Count-1 do
    begin
      if Pos('<MangaLink>', list.Strings[i]) > 0 then
      begin
        urlList.Add(GetString(list.Strings[i], '<MangaLink>', '</MangaLink>'));
      end;
      if Pos('<MangaName>', list.Strings[i]) > 0 then
      begin
        mangaList.Add(StringFilter(GetString(list.Strings[i], '<MangaName>', '</MangaName>')));
      end;
    end;
  end;

  if urlList.Count > 0 then
  begin
    path:= CleanAndExpandDirectory(settingsfile.ReadString('saveto', 'SaveTo', ''));
    regx := TRegExpr.Create;
    try
      regx.Expression := REGEX_HOST;
      for i:= 0 to urlList.Count-1 do
      begin
        host := '';
        m := nil;
        host := LowerCase(regx.Replace(urlList[i], '$2', True));
        if host <> '' then
        begin
          m := Modules.LocateModuleByHost(host);
        end;

        if Assigned(m) then
        begin
          SilentThreadManager.Add(
            MD_AddToFavorites,
            m,
            mangaList[i],
            RemoveHostFromURL(urlList[i]),
            path);
        end
        else
          FUnimportedMangas.Add(mangaList.Strings[i] + ' <' + urlList.Strings[i] + '>');
      end;
    finally
      regx.Free;
    end;
  end;

  fstream.Free;
  list.Free;
  urlList.Free;
  mangaList.Free;
end;

procedure TImportFavorites.Run;
begin
  MainForm.sbMain.Panels[1].Text := RS_ImportThreadStatus;
  FImportFailed := False;
  FUnimportedMangas := TStringList.Create;

  case cbSoftware.ItemIndex of
    0: FMDHandle;
    1: DMDHandle;
  end;

  MainForm.sbMain.Panels[1].Text := '';

  if FUnimportedMangas.Count > 0 then
  begin
    with TNewChapter.Create(MainForm) do
    try
      Caption := RS_ListUnimportedCaption;
      lbNotification.Caption := '';
      btCancel.Visible := False;
      btQueue.Visible := False;
      btDownload.Visible := True;
      btDownload.Caption := RS_BtnOK;
      mmMemo.Lines.Text := FUnimportedMangas.Text;
      ShowModal;
    finally
      Free;
    end;
  end;

  FUnimportedMangas.Free;
  if FImportFailed then
  begin
    CenteredMessageDlg(Self, RS_ImportFailed, mtError, [mbOK], 0);
  end
  else
  begin
    CenteredMessageDlg(Self, RS_ImportCompleted, mtConfirmation, [mbOk], 0);
  end;
end;

{ ----- public methods ----- }

procedure TImportFavorites.btImportClick(Sender: TObject);
begin
  Run;
end;

end.
