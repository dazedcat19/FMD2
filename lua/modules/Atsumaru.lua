----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2da79eaeb73d4539ad47d4eeb9549415'
	m.Name                     = 'Atsumaru'
	m.RootURL                  = 'https://atsu.moe'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://atsu.moe/api'
local DirectoryPagination = '/explore/filteredView'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination
	local s = '{"filter":{"tags":[],"status":[],"types":[]},"page":' .. URL .. '}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	local series = CreateTXQuery(HTTP.Document).XPath('json(*).items()')
	if series.Count == 0 then return no_error end

	for v in series.Get() do
		LINKS.Add('manga/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('/([^/]+)$')
	local u = API_URL .. '/manga/page?id=' .. mid

	if not HTTP.GET(u) then return net_problem end
	
	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).mangaPage.title')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('json(*).mangaPage.poster.image'))
	MANGAINFO.Authors   = x.XPathStringAll('json(*).mangaPage.authors().name')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).mangaPage.tags().name') .. ', ' .. x.XPathString('json(*).mangaPage.type')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).mangaPage.status'))
	MANGAINFO.Summary   = x.XPathString('json(*).mangaPage.synopsis')

	local page = 0
	while true do
		if not HTTP.GET(API_URL .. '/manga/chapters?id=' .. mid .. '&sort=asc&page=' .. tostring(page)) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).chapters()').Get() do
			MANGAINFO.ChapterLinks.Add(mid .. '/' .. v.GetProperty('id').ToString())
			MANGAINFO.ChapterNames.Add(v.GetProperty('title').ToString())
		end
		page = page + 1
		local pages = tonumber(x.XPathString('json(*).pages')) or 1
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local mid, cid = URL:match('^/([^/]+)/([^/]+)$')
	local u = API_URL .. '/read/chapter?mangaId=' .. mid .. '&chapterId=' .. cid

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).readChapter.pages()').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.GetProperty('image').ToString()))
	end

	return true
end