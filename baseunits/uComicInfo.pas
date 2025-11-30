{
        File: uComicInfo.pas
        License: GPLv2
        This unit is a part of Free Manga Downloader
}

unit uComicInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseUnit;

type

  { TComicInfo }

  TComicInfo = class
  private
    class function EscapeXML(const AText: String): String; static;
    class function FormatDate(ADate: TDateTime): String; static;
  public
    class function GenerateComicInfoXML(
      const AFilePath: String; 
      const ATitle: String;
      const ANumber: Integer;
      const AMangaInfo: TMangaInfo;
      const APageCount: Integer
    ): Boolean; static;

    class function GenerateExtraEPUBMetadata(
      const AFilePath: String; 
      const ATitle: String;
      const ANumber: Integer;
      const AMangaInfo: TMangaInfo
    ): String; static;
  end;

implementation

{ TComicInfo }

class function TComicInfo.EscapeXML(const AText: String): String;
begin
  Result := AText;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&apos;', [rfReplaceAll]);
  Result := Trim(Result)
end;

class function TComicInfo.FormatDate(ADate: TDateTime): String;
begin
  if ADate = 0 then
    Result := ''
  else
    Result := FormatDateTime('yyyy-MM-dd', ADate);
end;

class function TComicInfo.GenerateComicInfoXML(
  const AFilePath: String; 
  const ATitle: String;
  const ANumber: Integer;
  const AMangaInfo: TMangaInfo;
  const APageCount: Integer
  ): Boolean;
var
  XML: TStringList;
  L: TStringList;
begin
  Result := False;
  if not Assigned(AMangaInfo) then Exit;

  L := nil;
  XML := TStringList.Create;
  try
    XML.Add('<?xml version="1.0" encoding="UTF-8"?>');
    XML.Add('<ComicInfo xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">');

    if AMangaInfo.Title <> '' then
      XML.Add('  <Series>' + EscapeXML(AMangaInfo.Title) + '</Series>');

    if ATitle <> '' then
      XML.Add('  <Title>' + EscapeXML(ATitle) + '</Title>')
    else if AMangaInfo.Title <> '' then
      XML.Add('  <Title>' + EscapeXML(AMangaInfo.Title) + '</Title>');

    XML.Add('  <Number>' + IntToStr(ANumber + 1) + '</Number>');

    // If the Count matches the Volume count or Chapter count, then Kavita will assume the Series is Completed (you own all items of the series)
    if (AMangaInfo.Status = '0') then // completed
      XML.Add('  <Count>' + EscapeXML(IntToStr(AMangaInfo.ChapterLinks.Count)) + '</Count>');
    
    if AMangaInfo.Summary <> '' then
      XML.Add('  <Summary>' + EscapeXML(AMangaInfo.Summary) + '</Summary>');
    
    if AMangaInfo.Authors <> '' then
      XML.Add('  <Writer>' + EscapeXML(AMangaInfo.Authors) + '</Writer>');
    
    if AMangaInfo.Artists <> '' then
    XML.Add('  <Penciller>' + EscapeXML(AMangaInfo.Artists) + '</Penciller>');
    
    if AMangaInfo.Genres <> '' then
      XML.Add('  <Genre>' + EscapeXML(AMangaInfo.Genres) + '</Genre>');
    
    case AMangaInfo.Status of
      '0': XML.Add('  <Notes>Series completed</Notes>');
      '1': XML.Add('  <Notes>Series ongoing</Notes>');
      '2': XML.Add('  <Notes>Series on hold</Notes>');
      '3': XML.Add('  <Notes>Series cancelled</Notes>');
    end;

    XML.Add('  <Web>' + EscapeXML(AMangaInfo.FullLink) + '</Web>');
    
    if APageCount > 0 then
      XML.Add('  <PageCount>' + IntToStr(APageCount) + '</PageCount>');
    
    XML.Add('</ComicInfo>');
    
    if Trim(XML.Text) <> '' then
    begin
      L := TStringList.Create;
      L.Text := XML.Text;
      L.SaveToFile(AFilePath);
      Result := FileExists(AFilePath);
    end;
  finally
    L.Free;
    XML.Free;
  end;
end;

class function TComicInfo.GenerateExtraEPUBMetadata(
  const AFilePath: String;
  const ATitle: String;
  const ANumber: Integer;
  const AMangaInfo: TMangaInfo
): String;
var
  XML: TStringList;
  StatusText: String;
begin
  Result := '';
  if not Assigned(AMangaInfo) then Exit;

  XML := TStringList.Create;
  try
    if AMangaInfo.Authors <> '' then
      XML.Add('  <dc:creator opf:role="aut">' + EscapeXML(AMangaInfo.Authors) + '</dc:creator>');

    if AMangaInfo.Artists <> '' then
      XML.Add('  <dc:creator opf:role="art">' + EscapeXML(AMangaInfo.Artists) + '</dc:creator>');

    if AMangaInfo.Summary <> '' then
      XML.Add('  <dc:description>' + EscapeXML(AMangaInfo.Summary) + '</dc:description>');

    if AMangaInfo.Title <> '' then
      XML.Add('  <meta name="series" content="' + EscapeXML(AMangaInfo.Title) + '"/>');

    if ANumber >= 0 then
      XML.Add('  <meta name="chapter" content="' + IntToStr(ANumber + 1) + '"/>');

    if AMangaInfo.Genres <> '' then
      XML.Add('  <meta name="genre" content="' + EscapeXML(AMangaInfo.Genres) + '"/>');

    case AMangaInfo.Status of
      '0': StatusText := 'completed';
      '1': StatusText := 'ongoing';
      '2': StatusText := 'on_hold';
      '3': StatusText := 'cancelled';
    else
      StatusText := '';
    end;

    if StatusText <> '' then
      XML.Add('  <meta name="status" content="' + StatusText + '"/>');

    if AMangaInfo.Website <> '' then
      XML.Add('  <meta name="publisher" content="' + EscapeXML(AMangaInfo.Website) + '"/>');

    XML.Add('  <meta property="dcterms:modified">' +
            FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', Now) +
            '</meta>');

    if Trim(XML.Text) <> '' then
      Result := XML.Text;
  finally
    XML.Free;
  end;
end;

end.
