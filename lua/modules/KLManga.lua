----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '010777f53bf2414fad039b9567c8a9ce'
	m.Name                     = 'KLManga'
	m.RootURL                  = 'https://klz9.com'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api'
local DirectoryPagination = '/manga/list?limit=1000&sort=Title&order=asc&page='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http headers for making a request.
local function SetRequestHeaders()
	local key = 'KL9K40zaSyC9K40vOMLLbEcepIFBhUKXwELqxlwTEF'
	local timestamp = os.time()
	local payload = timestamp .. '.' .. key
	local signature = require('utils.sha256').sha256(payload)
	HTTP.Headers.Values['x-client-ts']  = timestamp
	HTTP.Headers.Values['x-client-sig'] = signature
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. 1
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).totalPages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. (URL + 1)
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).items()').Get() do
		LINKS.Add(v.GetProperty('slug').ToString() .. '.html')
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MODULE.RootURL .. API_URL .. '/manga/slug' .. URL:gsub('.html$', '')
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*)')
	MANGAINFO.Title     = x.XPathString('name', json)
	MANGAINFO.AltTitles = x.XPathString('other_name', json)
	MANGAINFO.CoverLink = x.XPathString('cover', json)
	MANGAINFO.Authors   = x.XPathString('authors', json)
	MANGAINFO.Artists   = x.XPathString('artists', json)
	MANGAINFO.Genres    = x.XPathString('genres', json):gsub(',', ', ')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('m_status', json), '2', '1')
	MANGAINFO.Summary   = x.XPathString('description', json)
	
	local chapters = {}

	for v in x.XPath('json(*).chapters()').Get() do
		local chapter = tonumber(v.GetProperty('chapter').ToString())
		local title = v.GetProperty('name').ToString()
		title = (title ~= 'null' and title ~= '') and (' - ' .. title) or ''

		chapters[#chapters + 1] = {
			id      = v.GetProperty('id').ToString(),
			chapter = chapter,
			name    = 'Chapter ' .. chapter .. title
		}
	end

	table.sort(chapters, function(a, b) return a.chapter < b.chapter end)

	for _, c in ipairs(chapters) do
		MANGAINFO.ChapterLinks.Add(c.id)
		MANGAINFO.ChapterNames.Add(c.name)
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. API_URL .. '/chapter' .. URL
	HTTP.Reset()
	SetRequestHeaders()

	if not HTTP.GET(u) then return false end

	local images = CreateTXQuery(HTTP.Document).XPathString('json(*).content')
	for image in images:gmatch('[^\r\n]+') do
		TASK.PageLinks.Add(image)
	end

	return true
end