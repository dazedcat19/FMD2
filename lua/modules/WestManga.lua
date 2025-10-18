----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '35e1b3ff5dbf428889d0f316c3d881e6'
	m.Name                     = 'WestManga'
	m.RootURL                  = 'https://westmanga.me'
	m.Category                 = 'Indonesian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://data.westmanga.me/api'
local DirectoryPagination = '/contents?orderBy=Added&type=Comic&page='

----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

-- Set the required http headers for making a request.
local function SetRequestHeaders(RequestPath)
	local access_key = 'WM_WEB_FRONT_END'
	local secret_key = 'xxxoidj'
	local timestamp = os.time()
	local key = timestamp .. 'GET' .. RequestPath:match('([^?]+)') .. access_key .. secret_key
	HTTP.Headers.Values['x-wm-accses-key'] = access_key
	HTTP.Headers.Values['x-wm-request-time'] = timestamp
	HTTP.Headers.Values['x-wm-request-signature'] = require('utils.sha256').hmac_sha256(key, 'wm-api-request')
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1
	SetRequestHeaders('/api' .. DirectoryPagination)

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).paginator.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)
	SetRequestHeaders('/api' .. DirectoryPagination)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('comic/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. URL
	SetRequestHeaders('/api' .. URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('json(*).data')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('alternative_name', json)
	MANGAINFO.CoverLink = x.XPathString('cover', json)
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Genres    = x.XPathStringAll('json(*).data.genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json))
	MANGAINFO.Summary   = x.XPathString('sinopsis', json)

	for v in x.XPath('json(*).data.chapters()').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. v.GetProperty('number').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/v' .. URL
	SetRequestHeaders('/api/v' .. URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.images()', TASK.PageLinks)

	return true
end