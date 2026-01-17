----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga-list/new-manga?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="panel_page_number"]//a[contains(@class, "page_last")]/@href'):match('page=(%d+)')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="list-truyen-item-wrap"]/h3/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//ul[@class="manga-info-text"]//h1')
	MANGAINFO.AltTitles = x.XPathString('//ul[@class="manga-info-text"]//h2/substring-after(., "Alternative :")')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="manga-info-pic"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//ul[@class="manga-info-text"]/li[contains(., "Author")]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="manga-info-text"]/li[contains(., "Genre")]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="manga-info-text"]/li[contains(., "Status")]'))
	MANGAINFO.Summary   = x.XPathStringAll('//div[@id="contentBox"]/substring-after(., "your bookmark.")')

	local slug = URL:match('/([^/]+)$')

	if not HTTP.GET(MODULE.RootURL .. '/api/manga/' .. slug .. '/chapters?limit=10000&offset=0') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.chapters()').Get() do
		MANGAINFO.ChapterLinks.Add('manga/' .. slug .. '/' .. v.GetProperty('chapter_slug').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('chapter_name').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="container-chapter-reader"]/img[@title]/@src', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M