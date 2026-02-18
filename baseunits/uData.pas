{
        File: uData.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uData;

{$mode objfpc}{$H+}

// This unit contains all necessary functions for data processing

interface

uses
  Classes, SysUtils, uBaseUnit, DBDataProcess, FMDOptions, httpsendthread,
  BaseThread, LazFileUtils, strutils, httpsend;

type

  { TMangaInformation }

  TMangaInformation = class(TObject)
  private
    FOwner: TBaseThread;
    FModule: Pointer;
    procedure SetModule(const AValue: Pointer);
  public
    HTTP: THTTPSendThread;
    MangaInfo: TMangaInfo;
    isGetByUpdater: Boolean;
    isGenerateFolderChapterName: Boolean;
    isRemoveUnicode: Boolean;
    isRemoveHostFromChapterLinks: Boolean;

    constructor Create(const AOwnerThread: TBaseThread = nil);
    destructor Destroy; override;
    function GetInfoFromURL(const AURL: String): Byte;
    procedure SyncInfoToData(const ADataProcess: TDBDataProcess); overload;
    procedure AddInfoToData(const ATitle, ALink: String; const ADataProcess: TDBDataProcess); overload;
    property Thread: TBaseThread read FOwner;
    property Module: Pointer read FModule write SetModule;
  end;

var
  options: TStringList;

implementation

uses
  Dialogs, WebsiteModules, uUpdateThread;

{ TMangaInformation }

constructor TMangaInformation.Create(const AOwnerThread: TBaseThread);
begin
  inherited Create;

  FOwner := AOwnerThread;
  HTTP := THTTPSendThread.Create(AOwnerThread);
  MangaInfo := TMangaInfo.Create;
  isGetByUpdater := False;
  isRemoveHostFromChapterLinks := True;
end;

destructor TMangaInformation.Destroy;
begin
  if Assigned(MangaInfo) then
  begin
    MangaInfo.Free;
  end;

  HTTP.Free;
  inherited Destroy;
end;

procedure TMangaInformation.SetModule(const AValue: Pointer);
begin
  if FModule = AValue then
  begin
    Exit;
  end;

  FModule := AValue;
  if Assigned(FModule) and Assigned(HTTP) then
  begin
    TModuleContainer(FModule).PrepareHTTP(HTTP);
  end;
end;

function TMangaInformation.GetInfoFromURL(const AURL: String): Byte;
var
  s, s2: String;
  j, k: Integer;
  del: Boolean;
  bmangaInfo: TBaseMangaInfo;
begin
  if Trim(AURL) = '' then
  begin
    Exit(INFORMATION_NOT_FOUND);
  end;

  GetBaseMangaInfo(MangaInfo, bmangaInfo);

  MangaInfo.Module := FModule;
  MangaInfo.CoverLink := '';
  MangaInfo.NumChapter := 0;
  MangaInfo.ChapterNames.Clear;
  MangaInfo.ChapterLinks.Clear;

  if Assigned(TModuleContainer(FModule).OnGetInfo) then
  begin
    MangaInfo.URL := FillHost(TModuleContainer(FModule).RootURL, AURL);
    Result := TModuleContainer(FModule).OnGetInfo(Self, AURL, TModuleContainer(FModule));
  end
  else
  begin
    Exit(INFORMATION_NOT_FOUND);
  end;

  with MangaInfo do
  begin
    if Link = '' then
    begin
      Link := RemoveHostFromURL(MangaInfo.URL);
    end;

    // cleanup info
    CoverLink := CleanURL(CoverLink);
    Title := Trim(FixWhiteSpace(RemoveStringBreaks(CommonStringFilter(Title))));
    AltTitles := Trim(FixWhiteSpace(RemoveStringBreaks(CommonStringFilter(AltTitles))));
    Authors := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(Authors))));
    Artists := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(Artists))));
    Genres := Trim(FixWhiteSpace(RemoveStringBreaks(Trim(Genres))));

    Authors := TrimRightChar(Trim(FixWhiteSpace(Authors)), [',']);
    Artists := TrimRightChar(Trim(FixWhiteSpace(Artists)), [',']);
    Genres := TrimRightChar(Trim(FixWhiteSpace(Genres)), [',']);

    Summary := CleanMultilinedString(FixWhiteSpace(Summary));

    // fix info
    if (LeftStr(Authors, 1) = '<') or (Authors = '-') or (Authors = ':') then
    begin
      Authors := '';
    end;

    if (LeftStr(Artists, 1) = '<') or (Artists = '-') or (Artists = ':') then
    begin
      Artists := '';
    end;

    if (Summary = '-') or (Summary = ':') then
    begin
      Summary := '';
    end;

    if Title = '' then
    begin
      Title := 'N/A';
    end;

    if (LeftStr(AltTitles, 1) = '<') or (AltTitles = '-') or (AltTitles = ':') then
    begin
      AltTitles := '';
    end;

    FillBaseMangaInfo(MangaInfo, bmangaInfo);

    // cleanup chapters
    if ChapterLinks.Count > 0 then
    begin
      while ChapterNames.Count < ChapterLinks.Count do
      begin
        ChapterNames.Add('');
      end;

      while ChapterLinks.Count < ChapterNames.Count do
      begin
        ChapterNames.Delete(ChapterNames.Count - 1);
      end;

      for j := 0 to ChapterLinks.Count - 1 do
      begin
        ChapterLinks[j] := Trim(ChapterLinks[j]);
        ChapterNames[j] := Trim(ChapterNames[j]);
      end;
    end;

    // remove duplicate chapter
    if ChapterLinks.Count > 0 then
    begin
      j := 0;
      while j < (ChapterLinks.Count - 1) do
      begin
        del := False;
        if (j + 1) < ChapterLinks.Count then
        begin
          for k := j + 1 to ChapterLinks.Count - 1 do
          begin
            if SameText(ChapterLinks[j], ChapterLinks[k]) then
            begin
              ChapterLinks.Delete(j);
              ChapterNames.Delete(j);
              del := True;
              Break;
            end;
          end;
        end;

        if not del then
        begin
          Inc(j);
        end;
      end;
    end;

    if ChapterLinks.Count > 0 then
    begin
      // remove host from chapter links
      if isRemoveHostFromChapterLinks then
      begin
        RemoveHostFromURLsPair(ChapterLinks, ChapterNames);
      end;

      // fixing chapter name
      for j := 0 to ChapterNames.Count - 1 do
      begin
        ChapterNames[j] := Trim(CleanString(RemoveStringBreaks(
          CommonStringFilter(ChapterNames[j]))));
      end;

      //remove manga name from chapter
      if OptionRemoveMangaNameFromChapter and (Title <> '') then
      begin
        s := LowerCase(Title);
        j := Length(s);
        for k := 0 to ChapterNames.Count - 1 do
        begin
          s2 := LowerCase(ChapterNames[k]);
          if Length(s2) > j then
          begin
            if Pos(s, s2) = 1 then
            begin
              s2 := ChapterNames[k];
              Delete(s2, 1, j);
              s2 := Trim(s2);

              if LeftStr(s2, 2) = '- ' then
              begin
                Delete(s2, 1, 2);
              end;

              ChapterNames[k] := s2;
            end;
          end;
        end;
      end;
    end;

    NumChapter := ChapterLinks.Count;
  end;
end;

procedure TMangaInformation.SyncInfoToData(const ADataProcess: TDBDataProcess);
begin
  if not Assigned(ADataProcess) then
  begin
    Exit;
  end;

  with MangaInfo do
  begin
    ADataProcess.UpdateData(Title, AltTitles, Link, Authors, Artists, Genres, Status, Summary,
      NumChapter, ModuleID);
  end;
end;

procedure TMangaInformation.AddInfoToData(const ATitle, ALink: String; const ADataProcess: TDBDataProcess);
begin
  if not Assigned(ADataProcess) then
  begin
    Exit;
  end;

  if (MangaInfo.Title = '') and (ATitle <> '') then
  begin
    MangaInfo.Title := ATitle;
  end;

  if (MangaInfo.Link = '') and (ALink <> '') then
  begin
    MangaInfo.Link := ALink;
  end;

  with MangaInfo do
  begin
    ADataProcess.AddData(Title, AltTitles, Link, Authors, Artists, Genres, Status,
      StringBreaks(Summary), NumChapter, Now);
  end;
end;

end.
