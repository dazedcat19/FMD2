----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'b8206e754d4541689c1d367f7e19fd64'
	m.Name                     = 'KomikCast'
	m.RootURL                  = 'https://v1.komikcast.fit'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://be.komikcast.fit'
local DirectoryPagination = '/series?take=100000'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http header for making a request.
local function SetRequestHeaders()
	HTTP.Headers.Values['Origin']  = MODULE.RootURL
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('parse-json(.)?data?data?*').Get() do
		LINKS.Add('series/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('/([^/]+)$')
	local u = API_URL .. '/series/' .. slug
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('parse-json(.)?data?data')
	MANGAINFO.Title     = x.XPathString('?title', json)
	MANGAINFO.AltTitles = x.XPathString('?nativeTitle', json)
	MANGAINFO.CoverLink = x.XPathString('?coverImage', json)
	MANGAINFO.Authors   = x.XPathString('string(?author)', json)
	MANGAINFO.Genres    = x.XPathString('string-join((?genres?*/data/name, concat(upper-case(substring(?format, 1, 1)), lower-case(substring(?format, 2)))), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('?status', json))
	MANGAINFO.Summary   = x.XPathString('?synopsis', json)

	HTTP.Reset()
	SetRequestHeaders()

	if not HTTP.GET(u .. '/chapters') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('parse-json(.)?data?data?*').Get()do
		local idx = v.GetProperty('index').ToString()
		local title = v.GetProperty('title').ToString()
		title = (title ~= 'null' and title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add('series/' .. slug .. '/chapters/' .. idx)
		MANGAINFO.ChapterNames.Add('Chapter ' .. idx .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. URL
	HTTP.Reset()
	SetRequestHeaders()

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('parse-json(.)?data?data?images?*', TASK.PageLinks)

	return true
end