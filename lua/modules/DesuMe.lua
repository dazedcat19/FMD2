----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '41e78386ff3447e7a283b6ce55950f0f'
	m.Name                     = 'DesuMe'
	m.RootURL                  = 'https://desu.me'
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

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = '/manga/api/'
DirectoryPagination = '?limit=50&order=id&page='
USER_AGENT = 'FreeMangaDownloader'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local x = nil
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. 1
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	PAGENUMBER = tonumber(math.ceil(x.XPathString('json(*).pageNavParams.count') / x.XPathString('json(*).pageNavParams.limit'))) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. (URL + 1)
	sleep(1000)
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).response()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('russian').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local chapter, json, title, v, volume, x = nil
	local u = MODULE.RootURL .. API_URL .. URL:match('.(%d+)/$')
	sleep(1000)
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	json = x.XPath('json(*).response')
	MANGAINFO.Title     = x.XPathString('russian', json)
	MANGAINFO.CoverLink = x.XPathString('image/preview', json)
	MANGAINFO.Genres    = x.XPathStringAll('json(*).response.genres().russian')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('trans_status', json), 'continued', 'completed')
	MANGAINFO.Summary   = x.XPathString('description', json)

	for v in x.XPath('json(*).response.chapters.list()').Get() do
		volume = v.GetProperty('vol').ToString()
		chapter = v.GetProperty('ch').ToString()
		title = v.GetProperty('title').ToString()

		volume = volume ~= 'null' and string.format('Том %s. ', volume) or ''
		chapter = chapter ~= 'null' and string.format('Глава %s', chapter) or ''
		title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

		MANGAINFO.ChapterLinks.Add(x.XPathString('id', json) .. '/chapter/' .. v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(volume .. chapter .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local v = nil
	local u = MODULE.RootURL .. API_URL .. URL:gsub('^/', '')
	sleep(1000)
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).response.pages.list()').Get() do
		TASK.PageLinks.Add(v.GetProperty('img').ToString())
	end

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.UserAgent = USER_AGENT

	return true
end