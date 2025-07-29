----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'deb0eed0b1eb4971bcec0eaac5cdf94b'
	m.Name                     = 'ManhwaRead'
	m.RootURL                  = 'https://manhwaread.com'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/manhwa/page/'
local DirectoryParameters = '/?sortby=new'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[contains(@class, "wp-pagenavi")]/a[@class="last"]/@href'):match('/(%d+)/')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//a[contains(@class, "manga-item__link")]', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="manga-titles grow"]/h1')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="manga-titles grow"]/h2')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "items-center gap-7")]//img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[./div="Author:"]//a/span[1]')
	MANGAINFO.Artists   = x.XPathStringAll('//div[./div="Artist:"]//a/span[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[contains(@class, "manga-genres")]/a|//div[./div="Tags:"]//a/span[1]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('(//span[contains(@class, "manga-status")])[1]/@data-status'))
	MANGAINFO.Summary   = x.XPathString('string-join(//div[@id="mangaDesc"]//p, "\r\n")')

	local publisher = x.XPathString('//div[./div="Publisher:"]//a/span[1]')
	if publisher ~= '' then MANGAINFO.Summary = 'Publisher: ' .. publisher .. '\r\n \r\n' .. MANGAINFO.Summary end

	for v in x.XPath('//div[@id="chaptersList"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[1]', v))
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('//script[contains(., "chapterData")]/substring-before(substring-after(., "chapterData = "), ";")'))
	local base = x.XPathString('json(*).base')
	x.ParseHTML(require 'fmd.crypto'.DecodeBase64(x.XPathString('json(*).data')))
	for src in x.XPath('json(*)().src').Get() do
		TASK.PageLinks.Add(base .. '/' .. src.ToString())
	end
	
	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end