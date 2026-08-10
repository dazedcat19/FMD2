----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api/manga/'
local DirectoryPagination = '?order_by=id&page='
local USER_AGENT = 'FreeMangaDownloader'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. 1
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).pagination.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. (URL + 1)
	sleep(1000)
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).mangas()').Get() do
		LINKS.Add(v.GetProperty('view_url').ToString())
		NAMES.Add(v.GetProperty('russian').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('.(%d+)/$')
	local u = MODULE.RootURL .. API_URL .. mid
	sleep(1000)
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(*).manga')
	MANGAINFO.Title     = x.XPathString('russian', info)
	MANGAINFO.AltTitles = x.XPathString('string-join((name, synonyms?*), ", ")', info)
	MANGAINFO.CoverLink = x.XPathString('cover?preview', info)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join(genres?*?name, ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('translation_status', info), 'continued', 'completed')
	MANGAINFO.Summary   = x.XPathString('description', info)

	if not HTTP.GET(u .. '/chapters') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).chapters()').Get() do
		local volume = v.GetProperty('volume').ToString()
		local chapter = v.GetProperty('number').ToString()
		local title = v.GetProperty('title').ToString()

		volume = volume ~= 'null' and string.format('Том %s. ', volume) or ''
		chapter = chapter ~= 'null' and string.format('Глава %s', chapter) or ''
		title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

		MANGAINFO.ChapterLinks.Add(mid .. '/' .. v.GetProperty('chapter_id').ToString())
		MANGAINFO.ChapterNames.Add(volume .. chapter .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local mid, cid = URL:match('^/([^/]+)/([^/]+)$')
	local u = MODULE.RootURL .. API_URL .. mid .. '/chapters/' .. cid
	sleep(1000)
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).chapter.pages().url', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.UserAgent = USER_AGENT

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '41e78386ff3447e7a283b6ce55950f0f'
	m.Name                     = 'Desu'
	m.RootURL                  = 'https://desu.uno'
	m.Category                 = 'Russian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.MaxTaskLimit             = 2
	m.MaxConnectionLimit       = 3
	m.SortedList               = true
end