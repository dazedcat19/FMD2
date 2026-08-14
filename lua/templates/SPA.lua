----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/manga-list?sort=new&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(//div[@id="app"]/@data-page).props.paginate.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(//div[@id="app"]/@data-page).props.paginate.data()').Get() do
		LINKS.Add('manga/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(//div[@id="app"]/@data-page).props')
	MANGAINFO.Title     = x.XPathString('manga?name', json)
	MANGAINFO.AltTitles = x.XPathString('manga?other_name', json)
	MANGAINFO.CoverLink = x.XPathString('manga?cover_url', json)
	MANGAINFO.Authors   = x.XPathString('string-join(manga?artists?*?name, ", ")', json)
	MANGAINFO.Genres    = x.XPathString('string-join(manga?genres?*?name, ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('manga?status_id', json), '0', '1', '', '2')
	MANGAINFO.Summary   = x.XPathString('manga?pilot', json)

	local slug = URL:match('/([^/]+)$')
	for v in x.XPath('chapters?*', json).Get() do
		MANGAINFO.ChapterLinks.Add('manga/' .. slug .. '/' .. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('name').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('json(//div[@id="app"]/@data-page).props.chapterContent'))
	x.XPathStringAll('//img/@src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M