----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/contents?orderBy=Added&type=Comic&page='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http headers for making a request.
local function SetRequestHeaders()
	local now = os.time()

	local token  = MODULE.Storage['token']
	local expiry = tonumber(MODULE.Storage['exp']) or 0

	if token == '' or now > expiry then
		if not HTTP.GET(MODULE.RootURL .. '/api/visit-token') then return false end

		local body = HTTP.Document.ToString()
		MODULE.Storage['token'] = body:match('"token":"(.-)"')
		MODULE.Storage['exp'] = body:match('"expires_at":(%d+)')
	end

	HTTP.Reset()
	HTTP.Headers.Values['X-WM-Visit'] = MODULE.Storage['token']
	HTTP.Headers.Values['Sec-Fetch-Site'] = 'same-origin'
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. '/api' .. DirectoryPagination .. 1
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).paginator.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/api' .. DirectoryPagination .. (URL + 1)
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('comic/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MODULE.RootURL .. '/api' .. URL
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('json(*).data')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('alternative_name', info)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('cover', info))
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Genres    = x.XPathString('string-join(genres?*?name, ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info))
	MANGAINFO.Summary   = x.XPathString('sinopsis', info)

	for v in x.XPath('chapters?*', info).Get() do
		MANGAINFO.ChapterLinks.Add(v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. v.GetProperty('number').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. '/api/v' .. URL
	SetRequestHeaders()

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.images()').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '35e1b3ff5dbf428889d0f316c3d881e6'
	m.Name                     = 'WestManga'
	m.RootURL                  = 'https://v1.westmanga.my'
	m.Category                 = 'Indonesian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end