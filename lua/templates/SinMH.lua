----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/list_'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[@class="last"]/a/@data-page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//p[@class="ell"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//p[@class="cover"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//span[(./strong="漫画作者：")]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//span[(./strong="漫画类型：")]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[(./strong="漫画状态：")]/a'), '连载中', '已完结')
	MANGAINFO.Summary   = x.XPathString('//div[@id="intro-all"]'):gsub('漫画简介：', '')

	for v in x.XPath('//ul[@id="chapter-list-1"]/li/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span', v))
	end

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local body, images, pages, path = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if HTTP.GET(MODULE.RootURL .. '/js/config.js') then
		server = HTTP.Document.ToString():match('"domain":%["(.-)"%]},') .. '/'
	end

	if not HTTP.GET(u) then return net_problem end

	body = HTTP.Document.ToString()
	images = body:match('var chapterImages = (%[.-%]);')
	path = body:match('var chapterPath = "(.-)";')
	if images then
		pages = require "utils.json".decode(images)
		for i, v in ipairs(pages) do
			TASK.PageLinks.Add(server .. path .. v)
		end
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M