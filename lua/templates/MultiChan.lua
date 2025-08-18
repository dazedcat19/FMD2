----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local MangaPerPage = 20

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@id="pagination"]/span/a[last()]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. '?offset=' .. MangaPerPage * URL

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//a[@class="title_link"]', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="name_row"]/h1')
	MANGAINFO.AltTitles = x.XPathString('//table[@class="mangatitle"]//tr[./td="Другие названия"]//h2')
	MANGAINFO.CoverLink = x.XPathString('//img[@id="cover"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//table[@class="mangatitle"]//tr[./td="Автор"]/td//a|//div[@id="info_wrap"]/div[./div="Автор"]//a')
	MANGAINFO.Genres    = x.XPathStringAll('//li[@class="sidetag"]/a[last()]') .. ', ' .. x.XPathString('//table[@class="mangatitle"]//tr[./td="Тип"]/td//a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathStringAll('//table[@class="mangatitle"]//tr[./td="Загружено"]//h2/text()'), 'продолжается', 'завершен')
	MANGAINFO.Summary   = x.XPathString('//div[@id="description"]/string-join(text(), "\r\n")')

	x.XPathHREFAll('//table[@class="table_cha"]//div[contains(@class, "manga")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('//script[contains(., "var data")]/substring-before(substring-after(., "var data = "), ";")'))
	x.XPathStringAll('json(*).fullimg()', TASK.PageLinks)

	return true
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