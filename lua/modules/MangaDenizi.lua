----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '04f12fc7c4284fd987406f050711d1d7'
	m.Name                     = 'MangaDenizi'
	m.RootURL                  = 'https://www.mangadenizi.net'
	m.Category                 = 'Turkish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/manga?section_only=1&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).manga.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).manga.data()').Get() do
		LINKS.Add('manga/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(//div[@id="app"]/@data-page).props.manga')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('string-join(alternative_titles?*, ", ")', json)
	MANGAINFO.CoverLink = x.XPathString('cover_url', json)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*?name, ", ")', json)
	MANGAINFO.Genres    = x.XPathString('string-join((categories?*?name, themes?*?name, type?name), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json))
	MANGAINFO.Summary   = x.XPathString('description', json)

	local slug = URL:match('/([^/]+)$')
	for v in x.XPath('chapters?*', json).Get() do
		local title = v.GetProperty('title').ToString()
		title = (title ~= 'null') and (' - ' .. title) or ''
		MANGAINFO.ChapterLinks.Add('read/' .. slug .. '/' .. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Bölüm ' .. v.GetProperty('number').ToString() .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(//div[@id="app"]/@data-page).props.pages().image_url', TASK.PageLinks)

	return true
end