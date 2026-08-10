----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/filtrar?limit=100000'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = API_URL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).paginated.data()').Get() do
		LINKS.Add(path .. '/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local slug = URL:match('/([^/]+)$')
	local u = API_URL .. '/serie/' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('parse-json(.)?serie')
	MANGAINFO.Title     = x.XPathString('name', info)
	MANGAINFO.AltTitles = x.XPathString('alternativeName', info)
	MANGAINFO.CoverLink = x.XPathString('urlImg', info)
	MANGAINFO.Genres    = x.XPathString('string-join(genders?*?name, ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('stateId', info), '1', '4', '2', '3|5')
	MANGAINFO.Summary   = x.XPathString('sinopsis', info)

	for v in x.XPath('chapters?*', info).Get() do
		MANGAINFO.ChapterLinks.Add(slug .. '/'.. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Cap. ' .. v.GetProperty('num').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = API_URL .. '/serie' .. URL

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local pages = x.XPathString('json(*).pageches.urlImg')
	if pages == '' then pages = x.XPathString('json(*).pageches().urlImg') end
	x.ParseHTML(pages)
	x.XPathStringAll('json(*)()', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M