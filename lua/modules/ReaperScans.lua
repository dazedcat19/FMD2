----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fb042c961d06479582edb2fa582e3a41'
	m.Name                     = 'ReaperScans'
	m.RootURL                  = 'https://reaperscans.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://api.reaperscans.com'
DirectoryPagination = '/query?page='
DirectoryParameters = '&perPage=20&series_type=Comic&query_string=&order=desc&orderBy=created_at&adult=true'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	if not HTTP.GET(API_URL .. DirectoryPagination .. 1 .. DirectoryParameters) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).meta.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1) .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. x.XPathString('series_slug', v))
		NAMES.Add(x.XPathString('title', v))
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local id, name, pages, slug, title, v, x = nil
	local page = 1
	local u = API_URL .. '/series/' .. URL:match('/series/(.-)$')

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.CoverLink = 'https://media.reaperscans.com/file/4SRBHm/' .. x.XPathString('json(*).thumbnail')
	MANGAINFO.Authors   = x.XPathString('json(*).author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).status'))
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	id = x.XPathString('json(*).id')
	slug = x.XPathString('json(*).series_slug')
	while true do
		if HTTP.GET(API_URL .. '/chapter/query?page=' .. tostring(page) .. '&perPage=30&series_id=' .. id) then
			x = CreateTXQuery(HTTP.Document)
			pages = tonumber(x.XPathString('json(*).meta.last_page')) or 1
			for v in x.XPath('json(*).data()').Get() do
				name  = x.XPathString('chapter_name', v)
				title = x.XPathString('chapter_title', v)

				title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

				MANGAINFO.ChapterLinks.Add('series/' .. slug .. '/' .. x.XPathString('chapter_slug', v))
				MANGAINFO.ChapterNames.Add(name .. title)
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
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="container"]//img/@*[contains(., "https")]', TASK.PageLinks)

	return no_error
end
