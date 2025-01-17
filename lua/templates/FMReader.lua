----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga-list.html?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[contains(@class, "pagination")]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[contains(@class, "series-title")]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('(//span[@itemprop="name"])[3]')
	if MANGAINFO.Title == '' then
		MANGAINFO.Title = x.XPathString('//nav[@aria-label="breadcrumb"]//li[3]')
	end
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "thumbnail")]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//ul[contains(@class, "manga-info")]/li[contains(., "Author")]//a|//span[@itemprop="author"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//ul[contains(@class, "manga-info")]/li[contains(., "Genre")]//a|//span[@itemprop="genre"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[contains(@class, "manga-info")]/li[contains(., "Status")]//a'), 'Incomplete|On going')
	MANGAINFO.Summary   = x.XPathString('string-join(//div[./h3="Description"]/p/text()|//div[@class="summary-content"]/p/text(), "\r\n")')

	x.XPathHREFTitleAll('//ul[contains(@class, "list-chapters")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathStringAll('//div[@class="chapter-content"]/img/@data-src', TASK.PageLinks)
	if TASK.PageLinks.Count == 0 then
		x.XPathStringAll('//div[@class="chapter-content"]/img/@src', TASK.PageLinks)
	end
	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M