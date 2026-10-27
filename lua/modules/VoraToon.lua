----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api/proxy'
local DirectoryPage = '/series?take=100000'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http headers for making a request.
local function SetRequestHeaders()
	local expiry = tonumber(MODULE.Storage['expiry']) or 0
	local now = tonumber(MODULE.Storage['now']) or 0
	local vt_proxy = MODULE.Storage['vt_proxy']

	if vt_proxy == '' or now > expiry then
		if not HTTP.GET(MODULE.RootURL) then return net_problem end
		vt_proxy = HTTP.Cookies.Values['vt_proxy']
		local now, expiry = vt_proxy:match('v1%.(%d+)%.(%d+)%.')
		MODULE.Storage['expiry'] = expiry
		MODULE.Storage['now'] = now
		MODULE.Storage['vt_proxy'] = vt_proxy
	end

	HTTP.Reset()
	HTTP.Cookies.Values['vt_proxy'] = MODULE.Storage['vt_proxy']
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. DirectoryPage
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data().data').Get() do
		LINKS.Add('series/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('/([^/]+)$')
	local u = MODULE.RootURL .. API_URL .. '/series/' .. slug
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('parse-json(.)?data?data')
	MANGAINFO.Title     = x.XPathString('?title', info)
	MANGAINFO.AltTitles = x.XPathString('?nativeTitle', info)
	MANGAINFO.CoverLink = x.XPathString('?coverImage', info)
	MANGAINFO.Authors   = x.XPathString('?author', info)
	MANGAINFO.Genres    = x.XPathString('string-join((?genres?*?data?name, concat(upper-case(substring(?format, 1, 1)), lower-case(substring(?format, 2)))), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('?status', info))
	MANGAINFO.Summary   = x.XPathString('?synopsis', info)

	SetRequestHeaders()

	if not HTTP.GET(u .. '/chapters') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('parse-json(.)?data?*?data').Get() do
		local idx = v.GetProperty('index').ToString()
		local title = v.GetProperty('title').ToString()
		title = (title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add('series/' .. slug .. '/chapters/' .. idx)
		MANGAINFO.ChapterNames.Add('Chapter ' .. idx .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. API_URL .. URL
	SetRequestHeaders()

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.data.images()', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	SetRequestHeaders()

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'b8206e754d4541689c1d367f7e19fd64'
	m.Name                     = 'VoraToon'
	m.RootURL                  = 'https://voratoon.co'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end