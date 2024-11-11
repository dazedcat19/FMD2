----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga-list?sort=new&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="pagination_wrap"]/a[last()]/@href'):match('&page=(%d+)')) or 1

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
	MANGAINFO.Title     = x.XPathString('//span[@class="series-name"]/a'):gsub('Engsub', ''):gsub('engsub', ''):gsub('Manhwa', ''):gsub('manhwa', '')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="series-cover"]//@style'):match("background%-image: url%('(.-)'%)")
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="series-information"]/div[./span="Author:"]/span/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="series-information"]/div[./span="Genre:"]/span/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="series-information"]/div[./span="Status:"]/span/a'), 'On going', 'Completed')
	MANGAINFO.Summary   = x.XPathString('//div[@class="summary-content"]')

	x.XPathHREFTitleAll('//ul[contains(@class, "list-chapters")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="chapter-content"]/img/@data-src', TASK.PageLinks)

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M