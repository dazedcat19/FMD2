----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/browse/newest/page-'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(//script[@data-page="app"]).props.books.lastPage'))

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(//script[@data-page="app"]).props.books.data()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('parse-json(//script[@data-page="app"]).props')
	MANGAINFO.Title     = x.XPathString('book?title', info)
	MANGAINFO.AltTitles = x.XPathString('book?alt', info)
	MANGAINFO.CoverLink = x.XPathString('book?cover', info)
	MANGAINFO.Artists   = x.XPathString('string-join(book?groups?*[type="artist"]?tags?*?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((book?tags?*?name, book?type?label), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('book?status?label', info))
	MANGAINFO.Summary   = x.XPathString('book?description', info)

	local desc = {}
	local parodies = {}
	local characters = {}

	for parody in x.XPath('book?groups?*[type="parody"]?tags?*?name', info).Get() do
		parodies[#parodies + 1] = parody.ToString()
	end

	for character in x.XPath('book?groups?*[type="character"]?tags?*?name', info).Get() do
		characters[#characters + 1] = character.ToString()
	end

	if #parodies > 0 then table.insert(desc, 'Parodies: ' .. table.concat(parodies, ', ')) end
	if #characters > 0 then table.insert(desc, 'Characters: ' .. table.concat(characters, ', ')) end
	MANGAINFO.Summary = MANGAINFO.Summary .. '\r\n \r\n' .. table.concat(desc, '\r\n')

	for v in x.XPath('chapters?*', info).Get() do
		local number = v.GetProperty('number').ToString()
		local title = v.GetProperty('title').ToString()

		MANGAINFO.ChapterLinks.Add(v.GetProperty('url').ToString())
		MANGAINFO.ChapterNames.Add(number .. ' - ' .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(//script[@data-page="app"]).props.images()', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '053da85062184626a0a1ff6d57c3c955'
	m.Name                     = 'Hentai2Read'
	m.RootURL                  = 'https://hentai2read.com'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end