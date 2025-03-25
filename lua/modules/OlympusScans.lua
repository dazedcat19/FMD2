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

API_URL = 'https://dashboard.olympusbiblioteca.com/api'
DirectoryPagination = '/series?type=comic&direction=desc&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).data.series.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.series.data()').Get() do
		LINKS.Add('series/comic-' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local pages, v, x = nil
	local page = 1
	local slug = URL:match('/series/comic%-(.-)$')
	local u = API_URL .. '/series/' .. slug

	if not HTTP.GET(u) then return net_problem end
	
	x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	MANGAINFO.Title     = x.XPathString('json(*).data.name')
	MANGAINFO.CoverLink = x.XPathString('json(*).data.cover')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).data.genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).data.status.id'), '1', '4', '3', '5|7')
	MANGAINFO.Summary   = x.XPathString('json(*).data.summary')

	pages = tonumber(math.ceil(x.XPathString('json(*).data.chapter_count') / 40)) or 1
	while true do
		if not HTTP.GET(u .. '/chapters?direction=asc&type=comic&page=' .. tostring(page)) then return net_problem end
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

-- Get the page count for the current chapter.
function GetPageNumber()
	local v = nil
	local u = API_URL .. '/series' .. URL .. '?type=comic'

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).chapter.pages()').Get() do
		TASK.PageLinks.Add(v.ToString())
	end

	return no_error
end