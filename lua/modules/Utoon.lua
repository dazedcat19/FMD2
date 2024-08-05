----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2b8e4004d8bd434ca5d8b75da95499f9'
	m.Name                     = 'Utoon'
	m.RootURL                  = 'https://utoon.net'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'
-- XPathTokenAuthors = 'Author(s)'
-- XPathTokenArtists = 'Artist(s)'
-- XPathTokenGenres  = 'Genre(s)'
-- XPathTokenStatus  = 'Status'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="post-title"]/h1')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="summary_image"]//img/@data-src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="author-content"]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@class="artist-content"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres-content"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="summary-heading" and contains(., "Status")]/following-sibling::div'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="description-summary"]/p')

	HTTP.Reset()
	HTTP.Headers.Values['Cache-Control'] = 'no-cache'
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
	HTTP.MimeType = 'application/x-www-form-urlencoded'
	if HTTP.POST(MANGAINFO.URL .. 'ajax/chapters') then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="li__text"]/a[not(@href="#")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end