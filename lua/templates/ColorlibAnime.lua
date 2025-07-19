----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga?type=all&sort=published&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="product__pagination"]/a[last()]/@href'):match('page=(%d+)')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@style="display: flex; flex-wrap:wrap;"]//h5/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="anime__details__title"]/h3')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="anime__details__pic set-bg det"]/@data-setbg')
	MANGAINFO.Authors   = x.XPathString('//div[@class="anime__details__title"]/span')
	MANGAINFO.Genres    = x.XPathStringAll('//li[./span="Categorie:"]/a'):gsub(',,', ',') .. ', ' .. x.XPathString('//li[./span="Type:"]/a'):gsub('^%l', string.upper)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//li[./span="Status:"]/text()'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="anime__details__text mb-5"]//p')

	x.XPathHREFAll('//div[@id="chapterList"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="col-lg-10 read-img"]/img/@src', TASK.PageLinks)

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M