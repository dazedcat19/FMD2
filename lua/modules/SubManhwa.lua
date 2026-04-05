----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID               = '00001673c7b1c4f4ba22be78fd599a2d0'
	m.Name             = 'SubManhwa'
	m.RootURL          = 'https://submanhwa.com'
	m.Category         = 'Spanish'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetPageNumber  = 'GetPageNumber'
    m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaReaderOnline'
-- DirectoryParameters = '/'            --> Override template variable by uncommenting this line.
XPathTokenStatus    = 'Estado'
XPathTokenAuthors   = 'Autor(es)'
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
XPathTokenGenres    = 'Categorías'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
    local v, x = nil
    local u = MaybeFillHost(MODULE.RootURL, URL)

    if not HTTP.GET(u) then return net_problem end

    x = CreateTXQuery(HTTP.Document)

    MANGAINFO.Title = x.XPathString('//h1[contains(@class,"manga-title-centered")]')
    MANGAINFO.CoverLink = x.XPathString('//img[contains(@class,"img-responsive")]/@src')
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[div[text()="Estado"]]/div[contains(@class,"detail-value")]'), 'Ongoing', 'Completed')
    MANGAINFO.Authors = x.XPathStringAll('//div[div[text()="Autor(es)"]]/div[contains(@class,"detail-value")]/a')
    MANGAINFO.Artists = x.XPathStringAll('//div[div[text()="Artist(s)"]]/div[contains(@class,"detail-value")]/a')
    MANGAINFO.Genres = x.XPathStringAll('//div[div[text()="Tags"]]/div[contains(@class,"detail-value")]/a')
    MANGAINFO.Summary = x.XPathString('//h5[contains(text(),"Resumen")]/following-sibling::p[1]')

    for v in x.XPath('//div[contains(@class,"chapter-preview-info") or contains(@class,"chapter-card-item")]//a').Get() do
        MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
        MANGAINFO.ChapterNames.Add(x.XPathString('normalize-space(text())', v))
    end

    MANGAINFO.ChapterLinks.Reverse()
    MANGAINFO.ChapterNames.Reverse()

    return no_error
end


-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	return true
end

