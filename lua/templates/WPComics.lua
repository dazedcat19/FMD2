----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/tim-truyen?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[contains(@class, "item")]//h3/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title-detail"]')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "col-image")]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//li[contains(@class, "author")]/p[2]/normalize-space(.)')
	MANGAINFO.Genres    = x.XPathStringAll('//li[contains(@class, "kind")]/p[2]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//li[contains(@class, "status")]/p[2]'),
		'Ongoing|Đang tiến hành|Đang cập nhật|進行中', 'Completed|Hoàn thành|完了')
	MANGAINFO.Summary   = x.XPathString('//div[@class="detail-content"]/*[not(@class="list-title")]')

	x.XPathHREFAll('//div[@class="list-chapter"]//li[not(@style)]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathStringAll('//div[@class="page-chapter"]/img/@data-src', TASK.PageLinks)
	if TASK.PageLinks.Count == 0 then
		x.XPathStringAll('//div[@class="page-chapter"]/img/@src', TASK.PageLinks)
	end

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M