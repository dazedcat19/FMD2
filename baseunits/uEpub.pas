unit uEpub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TPage = class;
  TPages = specialize TFPGList<TPage>;
  TStreams = specialize TFPGList<TStream>;

  { TEpubBuilder }

  TEpubBuilder = class
  private
    FTitle, FUuid: String;
    FMimeType, FContainer, FStyle, FContent, FToc: TStringStream;
    FPages: TPages;
    function CreateContent: String;
    function CreateToc: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddImage(const path: String);
    procedure SaveToStream(const stream: TStream);
    property Title: String read FTitle write FTitle;
  end;

  { TPage }

  TPage = class
  private
    FIndex: Integer;
    FImagePath, FPageTitle: String;
    FPage: TStringStream;
    function GetContentItem: String;
    function GetPageId: String;
    function GetImageId: String;
    function GetImageName: String;
    function GetPageName: String;
    function GetPageRef: String;
    function GetPage: String;
    function GetPageStream: TStringStream;
    function GetNavPoint: String;
  public
    constructor Create(const index: Integer; const imagePath, pageTitle: String);
    destructor Destroy; override;
    property ImagePath: String read FImagePath;
    property Index: Integer read FIndex;
    property ContentItem: String read GetContentItem;
    property PageId: String read GetPageId;
    property ImageId: String read GetImageId;
    property ImageName: String read GetImageName;
    property PageName: String read GetPageName;
    property PageRef: String read GetPageRef;
    property PageStream: TStringStream read GetPageStream;
    property NavPoint: String read GetNavPoint;
  end;

implementation

uses Zipper, htmlelements, uBaseUnit;

const
  CONTAINER: String =
    '<?xml version="1.0" encoding="utf-8"?>' + LineEnding +
    '<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">' + LineEnding +
    '  <rootfiles>' + LineEnding +
    '    <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>' + LineEnding +
    '  </rootfiles>' + LineEnding +
    '</container>' + LineEnding;

  CONTENT: String =
    '<?xml version="1.0" encoding="utf-8"?>' + LineEnding +
    '<package xmlns="http://www.idpf.org/2007/opf" xmlns:dc="http://purl.org/dc/elements/1.1/" ' +
      'xmlns:opf="http://www.idpf.org/2007/opf" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
      'xmlns:dcterms="http://purl.org/dc/terms/" unique-identifier="bookid" version="2.0">' + LineEnding +
    '  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">' + LineEnding +
    '    <dc:title>%s</dc:title>' + LineEnding +
    '    <dc:language>und</dc:language>' + LineEnding +
    '    <dc:identifier id="bookid" opf:scheme="UUID">urn:uuid:%s</dc:identifier>' + LineEnding +
    '  </metadata>' + LineEnding +
    '  <manifest>' + LineEnding +
    '    <item id="_toc" href="toc.ncx" media-type="application/x-dtbncx+xml"/>' + LineEnding +
    '    <item id="_style" href="style.css" media-type="text/css"/>' + LineEnding +
    '%s' + LineEnding +
    '  </manifest>' + LineEnding +
    '  <spine toc="_toc">'  + LineEnding +
    '%s' + LineEnding +
    '  </spine>' + LineEnding +
    '</package>';

  CONTENT_ITEM: String =
    '    <item id="%s" href="%s" media-type="%s"/>' + LineEnding;

  CONTENT_ITEMREF: String =
    '    <itemref idref="%s"/>' + LineEnding;

  TOC: String =
    '<?xml version="1.0" encoding="utf-8"?>' + LineEnding +
    '<ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">' + LineEnding +
    '  <head>' + LineEnding +
    '    <meta name="dtb:uid" content="urn:uuid:%s"/>' + LineEnding +
    '  </head>' + LineEnding +
    '  <docTitle>' + LineEnding +
    '    <text>%s</text>' + LineEnding +
    '  </docTitle>' + LineEnding +
    '  <navMap>' + LineEnding +
    '%s' + LineEnding +
    '  </navMap>' + LineEnding +
    '</ncx>' + LineEnding;

  NAV_POINT: String =
    '    <navPoint id="toc_%d" playOrder="%d">' + LineEnding +
    '      <navLabel>' + LineEnding +
    '        <text>Page %d</text>' + LineEnding +
    '      </navLabel>' + LineEnding +
    '      <content src="%s"/>' + LineEnding +
    '    </navPoint>' + LineEnding;

  PAGE: String =
    '<?xml version="1.0" encoding="utf-8"?>' + LineEnding +
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">' + LineEnding +
    '<html xmlns="http://www.w3.org/1999/xhtml">' + LineEnding +
    '<head>' + LineEnding +
    '  <link href="style.css" rel="stylesheet" type="text/css"/>' + LineEnding +
    '  <title>%s</title>' + LineEnding +
    '</head>' + LineEnding +
    '<body>' + LineEnding +
    '  <div><img src="%s"/></div>' + LineEnding +
    '</body>' + LineEnding +
    '</html>' + LineEnding;

  STYLE: String =
    'img {' + LineEnding +
    '  max-width: 100%;' + LineEnding +
    '  max-height: 100%;' + LineEnding +
    '}' + LineEnding;

function NewUUID: String;
var
  guid: TGuid;
begin
  CreateGUID(guid);
  SetLength(Result, 36);
  StrLFmt(PChar(Result), 36, '%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',
    [
     Longint(GUID.D1), GUID.D2, GUID.D3,
     GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
     GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]
    ]);
  Result := LowerCase(Result);
end;

{ TEpubBuilder }

function TEpubBuilder.CreateContent: String;
var
  items: String = '';
  refs: String = '';
  i: Integer;
begin
  for i := 0 to FPages.Count-1 do begin
    items += FPages[i].ContentItem;
    refs += FPages[i].PageRef;
  end;
  Result := Format(CONTENT, [EscapeHTML(Title), FUuid, items, refs]);
end;

function TEpubBuilder.CreateToc: String;
var
  navPoints: String = '';
  i: Integer;
begin
  for i := 0 to FPages.Count-1 do
    navPoints += FPages[i].NavPoint;
  Result := Format(TOC, [FUuid, Title, navPoints]);
end;

procedure TEpubBuilder.AddImage(const path: String);
var
  index: Integer;
  pageTitle: String;
begin
  index := FPages.Count + 1;
  pageTitle := Format('%s - %.4d', [Title, index]);
  FPages.Add(TPage.Create(index, path, pageTitle));
end;

procedure TEpubBuilder.SaveToStream(const stream: TStream);
var
  zip: TZipper;
  i: Integer;
  page: TPage;
begin
  zip := TZipper.Create;
  try
    // add mimetype
    FMimeType := TStringStream.Create('application/epub+zip');
    zip.Entries.AddFileEntry(FMimeType, 'mimetype');

    // add META-INF/container.xml
    FContainer := TStringStream.Create(CONTAINER);
    zip.Entries.AddFileEntry(FContainer, 'META-INF/container.xml');

    // add css
    FStyle := TStringStream.Create(STYLE);
    zip.Entries.AddFileEntry(FStyle, 'OEBPS/style.css');

    for i := 0 to FPages.Count-1 do begin
      page := FPages[i];
      // add image file
      zip.Entries.AddFileEntry(page.ImagePath, 'OEBPS/images/' + page.ImageName);
      // add xhtml file
      zip.Entries.AddFileEntry(page.PageStream, 'OEBPS/' + page.PageName);
    end;

    // add content.opf
    FContent := TStringStream.Create(CreateContent);
    zip.Entries.AddFileEntry(FContent, 'OEBPS/content.opf');

    // add toc.ncx
    FToc := TStringStream.Create(CreateToc);
    zip.Entries.AddFileEntry(FToc, 'OEBPS/toc.ncx');

    zip.SaveToStream(stream);
  finally
    zip.Free;
  end;
end;

constructor TEpubBuilder.Create;
begin
  FMimeType := nil;
  FContainer := nil;
  FStyle := nil;
  FContent := nil;
  FToc := nil;
  FPages := TPages.Create;
  FUuid := NewUUID;
end;

destructor TEpubBuilder.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPages.Count-1 do
    FPages[i].Free;
  FreeAndNil(FPages);
  FreeAndNil(FMimeType);
  FreeAndNil(FContainer);
  FreeAndNil(FStyle);
  FreeAndNil(FContent);
  FreeAndNil(FToc);
  inherited;
end;

{ TPage }

function TPage.GetContentItem: String;
begin
  Result := Format(CONTENT_ITEM, [ImageId, 'images/' + ImageName, GetMimeType(FImagePath)]);
  Result += Format(CONTENT_ITEM, [PageId, PageName, 'application/xhtml+xml']);
end;

function TPage.GetPageId: String;
begin
  Result := Format('page%.4d', [FIndex]);
end;

function TPage.GetImageId: String;
begin
  Result := Format('image%.4d', [FIndex]);
end;

function TPage.GetImageName: String;
begin
  Result := Format('%.4d%s', [FIndex, ExtractFileExt(FImagePath)]);
end;

function TPage.GetPageName: String;
begin
  Result := Format('%.4d.xhtml', [FIndex])
end;

function TPage.GetPageRef: String;
begin
  Result := Format(CONTENT_ITEMREF, [PageId]);
end;

function TPage.GetPage: String;
begin
  Result := Format(PAGE, [EscapeHTML(FPageTitle), 'images/' + ImageName]);
end;

function TPage.GetPageStream: TStringStream;
begin
  if not Assigned(FPage) then
    FPage := TStringStream.Create(GetPage);
  Result := FPage;
end;

function TPage.GetNavPoint: String;
begin
  Result := Format(NAV_POINT, [FIndex, FIndex, FIndex, PageName]);
end;

constructor TPage.Create(const index: Integer; const imagePath, pageTitle: String);
begin
  FPage := nil;
  FIndex := index;
  FImagePath := imagePath;
  FPageTitle := pageTitle;
end;

destructor TPage.Destroy;
begin
  FreeAndNil(FPage);
  inherited Destroy;
end;

end.

