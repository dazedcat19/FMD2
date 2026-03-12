----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/wp-json/neoglass/v1/mangas?per_page=100&paged='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).total_pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).items()').Get() do
		LINKS.Add(v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local slug = URL:gsub('/$', ''):match('([^/]+)$')
	local u = MODULE.RootURL .. '/wp-content/static/manga/' .. slug .. '.json'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*)')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('alternative', json)
	MANGAINFO.CoverLink = x.XPathString('thumbnail', json)
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*, concat(upper-case(substring(type, 1, 1)), lower-case(substring(type, 2)))), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json), 'on-going', 'end')
	MANGAINFO.Summary   = x.XPathString('synopsis', json)

	local chapters = {}
	for v in x.XPath('chapters?*', json).Get() do
		table.insert(chapters, {
			slug = v.GetProperty('slug').ToString(),
			title = v.GetProperty('title').ToString()
		})
	end

	table.sort(chapters, function(a, b) return (tonumber(a.slug:match('(%d+)')) or 0) < (tonumber(b.slug:match('(%d+)')) or 0) end)

	for _, chapter in ipairs(chapters) do
		MANGAINFO.ChapterLinks.Add('komik/' .. slug .. '/' .. chapter.slug)
		MANGAINFO.ChapterNames.Add(chapter.title)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="mjv2-page-image"]/@src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M