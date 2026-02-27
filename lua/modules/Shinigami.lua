----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c0b91b10b0bf4bb98a90a9995457c0f9'
	m.Name                     = 'Shinigami ID'
	m.RootURL                  = 'https://09.shinigami.asia'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.shngm.io/v1'
local DirectoryPagination = '/manga/list?page_size=100000'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. v.GetProperty('manga_id').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('/([^/]+)$')
	local u = API_URL .. '/manga/detail/' .. mid

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*).data')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('alternative_title', json)
	MANGAINFO.CoverLink = x.XPathString('cover_image_url', json)
	MANGAINFO.Authors   = x.XPathString('string-join(taxonomy?Author?*?name, ", ")', json)
	MANGAINFO.Artists   = x.XPathString('string-join(taxonomy?Artist?*?name, ", ")', json)
	MANGAINFO.Genres    = x.XPathString('string-join((taxonomy?Genre?*?name, taxonomy?Format?*?name), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json), '1', '2', '3')
	MANGAINFO.Summary   = x.XPathString('description', json)

	if not HTTP.GET(API_URL .. '/chapter/' .. mid .. '/list?page_size=10000&sort_by=chapter_number&sort_order=asc') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		local chapter = v.GetProperty('chapter_number').ToString()
		local title = v.GetProperty('chapter_title').ToString()

		title = (title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(v.GetProperty('chapter_id').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. chapter .. title)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/chapter/detail' .. URL

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*).data')
	local base = x.XPathString('base_url', json)
	local path = x.XPathString('chapter?path', json)
	for v in x.XPath('chapter?data?*', json).Get() do
		TASK.PageLinks.Add(base .. path .. v.ToString())
	end

	return true
end