----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '760d177b1f6d4763a08971c0c1b5572b'
	m.Name                     = 'Olympus Scanlation'
	m.RootURL                  = 'https://olympusbiblioteca.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://dashboard.olympusbiblioteca.com/api'
local DirectoryPagination = '/api/series?type=comic&direction=asc&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).data.series.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.series.data()').Get() do
		LINKS.Add('series/comic-' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('/series/comic%-(.-)$')
	local u = MODULE.RootURL .. '/api/series/' .. slug

	if not HTTP.GET(u) then return net_problem end
	
	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('json(*).data')
	MANGAINFO.Title     = x.XPathString('name', info)
	MANGAINFO.CoverLink = x.XPathString('cover', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, concat(upper-case(substring(type, 1, 1)), lower-case(substring(type, 2)))), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status?id', info), '1', '4', '3', '5|7')
	MANGAINFO.Summary   = x.XPathString('summary', info)

	local page = 1
	local pages = tonumber(math.ceil(x.XPathString('chapter_count', info) / 40)) or 1
	while true do
		if not HTTP.GET(API_URL .. '/series/' .. slug .. '/chapters?direction=asc&type=comic&page=' .. page) then return net_problem end
		for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
			MANGAINFO.ChapterLinks.Add(slug .. '/chapters/' .. v.GetProperty('id').ToString())
			MANGAINFO.ChapterNames.Add('Capítulo ' .. v.GetProperty('name').ToString())
		end
		page = page + 1
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local mid, cid = URL:match('^/(.-)/chapters/(.-)$')
	local u = MODULE.RootURL .. '/api/capitulo/comic-' .. mid .. '/' .. cid

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).chapter.pages()', TASK.PageLinks)

	return true
end