----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/trpc/search.query?batch=1&input={"0":{"json":{"q":"","sort":"newest","limit":100,"offset":%s,"maxRating":"pornographic"}}}'
local DirectoryPageLimit = 100

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http headers for making a request.
local function SetRequestHeaders()
	HTTP.Cookies.Values['__st'] = __st
	if Key then
		HTTP.Headers.Values[Key] = Value
	end
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination:format(0)
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(math.ceil(CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*)().result.data.json.totalHits') / DirectoryPageLimit)) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination:format(URL * 100)
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*)().result.data.json.hits()').Get() do
		LINKS.Add('manga/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local json = require 'utils.json'
	local slug = URL:match('/([^/]+)$')
	local u = MODULE.RootURL .. '/api/trpc/series.bySlugWithGenres?batch=1&input={"0":{"json":{"slug":"' .. slug .. '"}}}'
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	local data = json.decode(HTTP.Document.ToString())[1]
	if not data.result then MANGAINFO.Title = data.error.json.message return no_error end
	MANGAINFO.Title     = data.result.data.json.title
	MANGAINFO.AltTitles = table.concat(json.decode(data.result.data.json.alternativeTitles), ', ')
	MANGAINFO.CoverLink = data.result.data.json.coverUrl
	MANGAINFO.Authors   = table.concat(data.result.data.json.authors, ', ')
	MANGAINFO.Artists   = table.concat(data.result.data.json.artists, ', ')
	MANGAINFO.Genres    = table.concat(data.result.data.json.genres, ', ')
	MANGAINFO.Status    = MangaInfoStatusIfPos(data.result.data.json.status, 'ongoing|releasing')
	MANGAINFO.Summary   = data.result.data.json.synopsis

	local type = data.result.data.json.type
	if type then
		MANGAINFO.Genres = (MANGAINFO.Genres ~= '' and MANGAINFO.Genres .. ', ' or '') .. type:gsub('^%l', string.upper)
	end

	local s = '/api/trpc/series.chapters?batch=1&input={"0":{"json":{"seriesId":' .. data.result.data.json.id .. '}}}'

	HTTP.Reset()
	SetRequestHeaders()

	if not HTTP.GET(MODULE.RootURL .. s) then return net_problem end

	local chapters = json.decode(HTTP.Document.ToString())[1].result.data.json
	for _, chapter in ipairs(chapters) do
		local title = chapter.title
		local number = chapter.number

		title = title and ChapterName .. number ~= title:gsub('0(%d)', '%1') and ' - ' .. title or ''

		MANGAINFO.ChapterLinks.Add(slug .. '/' .. number)
		MANGAINFO.ChapterNames.Add(ChapterName .. number .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local slug, number = URL:match('^/([^/]+)/([^/]+)$')
	local u = MODULE.RootURL .. '/api/trpc/reader.chapterPages?batch=1&input={"0":{"json":{"seriesSlug":"' .. slug .. '","chapterNumber":' .. number .. '}}}'
	HTTP.Reset()
	SetRequestHeaders()

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*)().result.data.json().webpUrl', TASK.PageLinks)

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