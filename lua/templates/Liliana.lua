----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/all-manga/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@id="blog-pager"]/span[last()]/a/@href'):match('/(%d+)/?')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="text-center"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="full r2 md-w120"]/@src'))
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="y6x11p" and ./i[@class="fas fa-user"]]/span/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="mt-15"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="y6x11p" and ./i[@class="fas fa-rss"]]/span'),
		'OnGoing|On-Hold|進行中|保留|Đang tiến hành|Tạm ngưng', 'Canceled|Completed|完了|キャンセル|Hoàn thành|Đã hủy')
	MANGAINFO.Summary   = x.XPathString('//div[@id="syn-target"]')

	x.XPathHREFAll('//li[@class="chapter"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local cid, i, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	cid = x.XPathString('//script[contains(., "const CHAPTER_ID")]/substring-before(substring-after(., "const CHAPTER_ID = "), ";")')
	u = MODULE.RootURL .. '/ajax/image/list/chap/' .. cid

	if not HTTP.GET(u) then return net_problem end

	x.ParseHTML(require 'utils.json'.decode(HTTP.Document.ToString()).html)
	for i = 0, x.XPathCount('//a/@href') do
		TASK.PageLinks.Add(x.XPathString('//div[@data-index="' .. i .. '"]/a/@href'))
	end
	TASK.PageLinks.Clear()
	if TASK.PageLinks.Count == 0 then
		x.XPathStringAll('//a/@href', TASK.PageLinks)
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M