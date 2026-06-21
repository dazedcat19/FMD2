----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/titles/search?sort=newest&limit=50&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*).data.pagination.total_pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).data.items()').Get() do
		LINKS.Add(v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(//script[@id="__NEXT_DATA__"]).props.pageProps.initialManga')
	MANGAINFO.Title     = x.XPathString('name', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(altNames?*?name, ", ")', info)
	MANGAINFO.CoverLink = x.XPathString('cover', info)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*?name, ", ")', info)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, manga?demography?name, manga?bookType?name), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info))
	MANGAINFO.Summary   = x.XPathString('summary', info)

	local mid = x.XPathString('id', info)

	if not HTTP.GET(API_URL .. '/titles/' .. mid .. '/chapters') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.chapters()').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetProperty('url').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('name').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(//script[@id="__NEXT_DATA__"]).props.pageProps.initialChapter.images()', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M