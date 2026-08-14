----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = 'HachiRaw'
		m.RootURL                  = url
		m.Category                 = 'Raw'
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.SortedList               = true
	end
	AddWebsiteModule('1d09f3bea8f148fa9e9215fc578fedcd', 'https://manga1001.win')
	AddWebsiteModule('1d09f3bea8f148fa9e9215fc578fedcd', 'https://hachiraw.win')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/category/order/addtime/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@id="pagination"]/div/a[@class="end"]/@href'):match('/(%d+)$')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="cate-comic-list clearfix"]//p[@class="comic__title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//p[@class="comic-title j-comic-title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="de-info__cover"]/img/@src')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="comic-status" and ./span[contains(., "テーマ")]]//a')

	x.XPathHREFAll('//div[@class="de-chapter"]//a[@class="j-chapter-link"]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="rd-article-wr clearfix"]//img/@data-original', TASK.PageLinks)

	return true
end