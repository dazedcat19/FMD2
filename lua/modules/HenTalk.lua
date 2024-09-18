----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'b8ad8d26ff8842ca8c6dc8044131eb5f'
	m.Name                     = 'HenTalk'
	m.RootURL                  = 'https://hentalk.pw'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.MaxTaskLimit             = 1
	m.MaxConnectionLimit       = 1
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://hentalk.pw/api'
CDN_URL = 'https://cdn.fakku.cc/image/'
USER_AGENT = 'FreeMangaDownloader'
DirectoryPagination = '/library?sort=created_at&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local x = nil
	local u = API_URL .. DirectoryPagination .. 1
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	PAGENUMBER = tonumber(math.ceil(x.XPathString('json(*).total') / x.XPathString('json(*).limit'))) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1)
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).archives()').Get() do
		LINKS.Add('g/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local magazines, parodies, publishers, summary, x = nil
	local u = API_URL .. '/g/' .. URL:match('g/(%d+)')
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.CoverLink = x.XPathString('json(*).thumbnail_url')
	MANGAINFO.Authors   = x.XPathStringAll('json(*).artists().name') .. ', ' .. x.XPathStringAll('json(*).circles().name')

	magazines  = x.XPathString('json(*).magazines().name')
	publishers = x.XPathString('json(*).publishers().name')
	parodies   = x.XPathString('json(*).parodies().name')
	if magazines  ~= '' then magazines  = ', ' .. magazines end
	if publishers ~= '' then publishers = ', ' .. publishers end
	if parodies   ~= '' then parodies   = ', ' .. parodies end
	MANGAINFO.Genres = x.XPathStringAll('json(*).tags().name') .. magazines .. publishers .. parodies

	summary = x.XPathString('json(*).description')
	if summary ~= 'null' then MANGAINFO.Summary = summary end

	MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local hash, v, x = nil
	local u = API_URL .. '/g/' .. URL:match('g/(%d+)')
	HTTP.UserAgent = USER_AGENT

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	hash = x.XPathString('json(*).hash') .. '/'
	for v in x.XPath('json(*).images().filename').Get() do
		TASK.PageLinks.Add(CDN_URL .. hash .. v.ToString())
	end

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end