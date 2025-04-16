----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/newest?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="paginator"]/a[last()-1]/@href'):match('page=(%d+)$')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="title"]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//h2')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="img-cover"]/img/@data-src')
	MANGAINFO.Authors   = x.XPathStringAll('//p[./strong[contains(., "Authors")]]/a/span')
	MANGAINFO.Genres    = x.XPathStringAll('//p[./strong[contains(., "Genres")]]/a/normalize-space(.)'):gsub(' ,', '')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[./strong[contains(., "Status")]]/a'))
	MANGAINFO.Summary   = x.XPathString('//p[@class="content"]')

	for v in x.XPath('//ul[@class="chapter-list"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div/strong', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local body, i, pages = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	body = HTTP.Document.ToString()
	pages = body:match('var chapImages = (.-);')
	for i in pages:gmatch('([^",]+)') do
		TASK.PageLinks.Add(i)
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M