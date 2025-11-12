----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '0e45db2650604f74a0caeb7c1d69a749'
	m.Name                     = 'Swat Manga'
	m.RootURL                  = 'https://meshmanga.com'
	m.Category                 = 'Arabic'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://appswat.com/v2/api/v2'
local DirectoryPagination = '/series/?order_by=-created_at&page_size=200&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1
	HTTP.Headers.Values['Accept'] = '*/*'

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = math.ceil(CreateTXQuery(HTTP.Document).XPathString('json(*).count') / 200) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)
	HTTP.Headers.Values['Accept'] = '*/*'

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).results()').Get() do
		LINKS.Add('series/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local mid = URL:match('/series/(%d+)$')
	local u = API_URL .. '/series/' .. mid
	HTTP.Headers.Values['Accept'] = '*/*'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*)')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('alternative', json)
	MANGAINFO.CoverLink = x.XPathString('poster/thumbnail', json)
	MANGAINFO.Authors   = x.XPathString('author/name', json)
	MANGAINFO.Genres    = x.XPathString('string-join(genres?*/name, ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status/name', json))
	MANGAINFO.Summary   = x.XPathString('story', json)

	HTTP.Reset()
	HTTP.Headers.Values['Accept'] = '*/*'

	if not HTTP.GET(API_URL .. '/chapters/?order_by=order&page_size=10000&serie=' .. mid) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).results()').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('chapter').ToString())
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/chapters' .. URL
	HTTP.Reset()
	HTTP.Headers.Values['Accept'] = '*/*'

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).images().image', TASK.PageLinks)

	return true
end
