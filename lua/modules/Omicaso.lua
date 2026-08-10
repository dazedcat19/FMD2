----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd36c58efbd4b414b91a220922bab96fb'
	m.Name                     = 'Omicaso'
	m.RootURL                  = 'https://omicaso.org'
	m.Category                 = 'Indonesian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/manga.php?sort=created&limit=40&page='
local DirectoryPageLimit = 40

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(math.ceil(CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*).total') / DirectoryPageLimit)) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).items()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//p[@class="detail-alt-title"]/text()')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="detail-poster"]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//div[span="Author"]/strong')
	MANGAINFO.Artists   = x.XPathString('//div[span="Artist"]/strong')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="detail-rating-line"]/a, //div[span="Type"]/strong)')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[span="Status"]/strong'))
	MANGAINFO.Summary   = x.XPathString('//section[@class="synopsis-box"]/p')

	for v in x.XPath('//div[@data-chapter-list]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span/strong', v))
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@data-reader-image]/@src', TASK.PageLinks)

	return true
end