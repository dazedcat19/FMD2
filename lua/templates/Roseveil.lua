----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/search?sort=new&limit=100&type=COMIC&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).total_pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('comic/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local slug = URL:gsub('/$', ''):match('([^/]+)$')
	local u = API_URL .. '/series/comic/' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('parse-json(.)')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('string-join(alternative_titles, ", ")', json)
	MANGAINFO.CoverLink = x.XPathString('poster_image_url', json)
	MANGAINFO.Authors   = x.XPathString('author_name', json)
	MANGAINFO.Artists   = x.XPathString('artist_name', json)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, concat(upper-case(substring(comic_subtype, 1, 1)), lower-case(substring(comic_subtype, 2)))), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('comic_status', json))
	MANGAINFO.Summary   = x.XPathString('synopsis', json)

	for v in x.XPath('units?*', json).Get() do
		local title = v.GetProperty('title').ToString()
		if title == '' then title = 'Chapter ' .. math.floor(v.GetProperty('number').ToString()) end

		MANGAINFO.ChapterLinks.Add(slug .. '/chapter/' .. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add(title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = API_URL .. '/series/comic' .. URL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPathStringAll('json(*).chapter.pages().image_url', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M