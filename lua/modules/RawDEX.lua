----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/manga/page/'
local DirectoryParameters = '/?m_orderby=new-manga'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1 .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="page-numbers"]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//h2/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('replace(//p[@class="rdx-manga-alternative"], " [|]", ",")')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="rdx-manga-cover"]/@src')
	MANGAINFO.Authors   = x.XPathString('//div[dt="Author"]/dd')
	MANGAINFO.Artists   = x.XPathString('//div[dt="Artist"]/dd')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="rdx-manga-tags"]/a, //div[dt="Type"]/dd)')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="rdx-manga-status"]'), 'on-going', 'end')
	MANGAINFO.Summary   = x.XPathString('//div[@class="rdx-manga-summary"]/p')

	for v in x.XPath('//div[@class="rdx-chapter-list"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[@class="rdx-chapter-row__label"]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//figure[@class="rdx-reader-page"]/img/@src', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'f164e6052faf4c8a896a8be3cef7cb61'
	m.Name                     = 'RawDEX'
	m.RootURL                  = 'https://rawdex.net'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end