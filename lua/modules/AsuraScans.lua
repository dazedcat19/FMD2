----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.asurascans.com/api/series'
local DirectoryPageLimit = 20
local DirectoryPagination = '?sort=newest&order=desc&limit=' .. DirectoryPageLimit .. '&offset='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 0

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = math.ceil(CreateTXQuery(HTTP.Document).XPathString('json(*).meta.total') / DirectoryPageLimit) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (DirectoryPageLimit * URL)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('comics/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('/([^/]+)$')
	local u = API_URL .. '/' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(*).series')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(alt_titles?*, ", ")', info)
	MANGAINFO.CoverLink = x.XPathString('cover', info)
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Artists   = x.XPathString('artist', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, upper-case(substring(type, 1, 1)) || lower-case(substring(type, 2))), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info), 'ongoing', 'completed', 'hiatus', 'axed|dropped')

	local summary = x.XPathString('description', info)
	if summary ~= '' then
		MANGAINFO.Summary = CreateTXQuery(summary).XPathString('string-join(//text(), "\r\n")')
	end

	if not HTTP.GET(u .. '/chapters') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()[not(is_premium)]').Get() do
		local number = v.GetProperty('number').ToString()
		local title = v.GetProperty('title').ToString()

		title = (title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(slug .. '/chapters/' .. number)
		MANGAINFO.ChapterNames.Add('Chapter ' .. number .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. URL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.chapter.pages().url', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '7103ae6839ea46ec80cdfc2c4b37c803'
	m.Name                     = 'Asura Scans'
	m.RootURL                  = 'https://asurascans.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end