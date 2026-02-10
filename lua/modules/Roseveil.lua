----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '1f580436aef64e93b00ef95ae09e5e12'
	m.Name                     = 'Roseveil'
	m.RootURL                  = 'https://roseveil.org'
	m.Category                 = 'Indonesian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/comic/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="nav-links"]/(a[@class="page-numbers"])[last()]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//h3/a[contains(@href, "/manga/")]', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="text-gray-400 text-sm"]')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "wp-post-image")]/@src')
	MANGAINFO.Genres    = x.XPathStringAll('(//a[contains(@href, "manga-genre")], //div[./span="Type"]/following-sibling::div/span)')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[./span="Status"]/following-sibling::div/span[2]'), 'on-going', 'end', 'on-hold', 'canceled')
	MANGAINFO.Summary   = x.XPathString('//div[@id="panel-synopsis"]/div[1]/p')

	HTTP.Reset()
	HTTP.Headers.Values['Content-Length'] = 0
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
	if not HTTP.POST(MANGAINFO.URL .. 'ajax/chapters/?t=1') then return net_problem end
	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//li[contains(@class, "wp-manga-chapter")]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('.//h3', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	require 'templates.Madara'.GetPageNumber()

	return true
end