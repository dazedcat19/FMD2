----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'df01551e1739407a98669e37318842b0'
	m.Name                     = 'SoftKomik'
	m.RootURL                  = 'https://softkomik.com'
	m.Category                 = 'Indonesian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://v2.softdevices.my.id'
local CDN_URL = 'https://image.softkomik.com/softkomik/'
local DirectoryPagination = '?limit=24&sortBy=newKomik&page='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http headers for making a request.
local function SetRequestHeaders()
	local now = os.time()

	local sign  = MODULE.Storage['sign']
	local token = MODULE.Storage['token']
	local saved = MODULE.Storage['session_time'] or 0

	if sign ~= '' and token ~= '' and (now - saved) < 21300 then
		HTTP.Reset()
		HTTP.Headers.Values['X-Sign']  = sign
		HTTP.Headers.Values['X-Token'] = token
		return no_error
	end

	if not HTTP.GET(API_URL .. '/api/session') then return net_problem end

	local body = HTTP.Document.ToString()
	local new_sign  = body:match('"sign":"(.-)"')
	local new_token = body:match('"token":"(.-)"')

	if new_sign and new_token then
		MODULE.Storage['sign']         = new_sign
		MODULE.Storage['token']        = new_token
		MODULE.Storage['session_time'] = now

		HTTP.Reset()
		HTTP.Headers.Values['X-Sign']  = new_sign
		HTTP.Headers.Values['X-Token'] = new_token
	end
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. '/komik' .. DirectoryPagination .. 1
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end
	
	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).maxPage')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. '/komik' .. DirectoryPagination .. (URL + 1)
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add(v.GetProperty('title_slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. '/komik' .. URL
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('parse-json(.)')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('title_alt', json)
	MANGAINFO.CoverLink = 'https://softkomik.com/_next/image?url=https://cover.softdevices.my.id/softkomik-cover/' .. x.XPathString('gambar', json) .. '&w=256&q=100'
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Genres    = x.XPathString('string-join((Genre?*, concat(upper-case(substring(type, 1, 1)), lower-case(substring(type, 2)))), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json))
	MANGAINFO.Summary   = x.XPathString('sinopsis', json)

	SetRequestHeaders()

	if not HTTP.GET(u .. '/chapter?limit=9999999') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).chapter().chapter').Get() do
		local ch = v.ToString()
		MANGAINFO.ChapterLinks.Add(URL .. '/chapter/' .. ch)
		MANGAINFO.ChapterNames.Add('Chapter ' .. ch)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local id = HTTP.Document.ToString():match('"_id":".-".-"_id":"(.-)"')
	SetRequestHeaders()

	if not HTTP.GET(API_URL .. '/komik' .. URL .. '/img/' .. id) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).imageSrc()').Get() do
		TASK.PageLinks.Add(CDN_URL .. v.ToString())
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end