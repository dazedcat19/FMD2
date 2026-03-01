----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '4b5f8afd9a174af7b386a6de8ed83a2f'
	m.Name                     = 'MangaTown'
	m.RootURL                  = 'https://www.mangatown.com'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetImageURL            = 'GetImageURL'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/directory/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. '?name.az'

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="next-page"]/a[last()-1]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. '.htm?name.az'

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//ul[@class="manga_pic_list"]/li/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//li[b="Alternative Name:"]/text()')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="detail_info clearfix"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//li[b="Author(s):"]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//li[b="Artist(s):"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('(//li[b="Genre(s):"]/a, //li[b="Demographic:"]/a, //li[b="Type:"]/a)')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//li[b="Status(s):"]/text()'))
	MANGAINFO.Summary   = x.XPathString('//li[b="Summary:"]/span[@id="show"]/text()')

	for v in x.XPath('//ul[@class="chapter_list"]/li').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('string-join((a/text(), span[not(@class)]), " - ")', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u MaybeFillHost(MODULE.RootURL,URL)

	if not HTTP.GET(u) then return false end

	TASK.PageNumber = CreateTXQuery(HTTP.Document).XPathCount('(//select[not(@id)])[1]/option[not(contains(@value, "featured.html"))]')

	return true
end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
	local s = MaybeFillHost(MODULE.RootURL, URL)
	if WORKID > 0 then s = s:gsub('/+$', '') .. '/'.. (WORKID + 1) .. '.html' end

	if not HTTP.GET(s) then return false end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPathCount('//div[@id="viewer"]//img[@alt]/@src') > 1 then
		x.XPathStringAll('//div[@id="viewer"]//img[@alt]/@src', TASK.PageLinks)
		for i = 0, TASK.PageLinks.Count - 1 do
			TASK.PageLinks[i] = TASK.PageLinks[i]:gsub('^//', 'https://')
		end
	else
		TASK.PageLinks[WORKID] = x.XPathString('//div[@id="viewer"]//img[@alt]/@src'):gsub('^//', 'https://')
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end