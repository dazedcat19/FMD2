----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/comics'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('"data":', ']}]', x.XPathString('//script[contains(., "trending")]'):gsub('\\"', '\"'):gsub('\\\\', '\\')) .. ']')
	for v in x.XPath('json(*)()').Get() do
		LINKS.Add('ver/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local title, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('"data":', ',"numFollow"', x.XPathString('//script[contains(., "lastChapters")]'):gsub('\\"', '\"'):gsub('\\\\', '\\')))
	json = x.XPath('json(*)')
	MANGAINFO.Title     = x.XPathString('name', json)
	MANGAINFO.CoverLink = x.XPathString('urlImg', json)
	MANGAINFO.Authors   = x.XPathString('json(*).autors().autor.name')
	MANGAINFO.Artists   = x.XPathString('json(*).artists().artist.name')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).genders().gender.name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('seriesStatus', json))
	MANGAINFO.Summary   = x.XPathString('sinopsis', json)

	for v in x.XPath('json(*).lastChapters()').Get() do
		title = v.GetProperty('name').ToString()
		title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

		MANGAINFO.ChapterLinks.Add('ver/' .. x.XPathString('slug', json) .. '/' .. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. v.GetProperty('num').ToString() .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local name1, name2, name3, name4, s, value1, value2, value3, value4, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathStringAll('//main//@src', TASK.PageLinks)

	if TASK.PageLinks.Count == 0 then
		name1, value1 = x.XPathString('//input[2]/@name'), x.XPathString('//input[2]/@value')
		name2, value2 = x.XPathString('//input[3]/@name'), x.XPathString('//input[3]/@value')
		name3, value3 = x.XPathString('//input[4]/@name'), x.XPathString('//input[4]/@value')
		name4, value4 = x.XPathString('//input[5]/@name'), x.XPathString('//input[5]/@value')
		s = name1 .. '=' .. value1 .. '&' .. name2 .. '=' .. value2 .. '&' .. name3 .. '=' .. value3 .. '&' .. name4 .. '=' .. value4
		u = x.XPathString('//form/@action')

		HTTP.Reset()
		if HTTP.POST(u, s) then
			CreateTXQuery(HTTP.Document).XPathStringAll('//main/img/@src', TASK.PageLinks)
		end
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M