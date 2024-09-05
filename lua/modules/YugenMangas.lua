----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ac42a85566244b7e836679491ce679e6'
	m.Name                     = 'YugenMangás'
	m.RootURL                  = 'https://yugenmangasbr.voblog.xyz'
	m.Category                 = 'Portuguese'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://api.yugenweb.com/api'
CDN_URL = 'https://media.yugenweb.com'
DirectoryPagination = '/widgets/sort_and_filter/?order=desc&sort=date&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).total_pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).results()').Get() do
		LINKS.Add('series/' .. v.GetProperty('code').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local id, pages, title, slug, v, x = nil
	local page = 1
	local u = API_URL .. '/series/detail/series/'
	local s = '{"code":"' .. URL:match('/series/(%d+)$') .. '"}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.CoverLink = x.XPathString('json(*).path_cover')
	MANGAINFO.Authors   = x.XPathString('json(*).author')
	MANGAINFO.Artists   = x.XPathString('json(*).artist')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).genres()')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).status'), 'Em Lançamento', 'Finalizado')
	MANGAINFO.Summary   = x.XPathString('json(*).synopsis')

	while true do
		HTTP.Reset()
		HTTP.MimeType = 'application/json'
		if HTTP.POST(API_URL .. '/series/chapters/get-series-chapters/?page=' .. tostring(page) .. '', s) then
			x = CreateTXQuery(HTTP.Document)
			pages = tonumber(math.ceil(x.XPathString('json(*).count') / 21)) or 1
			for v in x.XPath('json(*).results.chapters()').Get() do
				MANGAINFO.ChapterLinks.Add(v.GetProperty('code').ToString())
				MANGAINFO.ChapterNames.Add(v.GetProperty('name').ToString())
			end
		else
			break
		end
		page = page + 1
		if page > pages then
			break
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local v = nil
	local u = API_URL .. '/chapters/chapter-info/'
	local s = '{"code":"' .. URL:match('(%d+)') .. '"}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).images()').Get() do
		TASK.PageLinks.Add(CDN_URL .. '/' .. v.ToString())
	end

	return no_error
end