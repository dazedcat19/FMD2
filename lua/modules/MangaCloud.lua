----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local domain = 'mangacloud.org'

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c256a1dbccc5461491a96dd926ec7538'
	m.Name                     = 'MangaCloud'
	m.RootURL                  = 'https://' .. domain
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.' .. domain
local CDN_URL = 'https://pika.' .. domain
local DirectoryPagination = '/comic/browse'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local s = '{"sort":"created_date-DESC","page":' .. (URL + 1) .. '}'
	local u = API_URL .. DirectoryPagination
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	local series = CreateTXQuery(HTTP.Document).XPath('json(*).data()')
	if series.Count == 0 then return no_error end

	for v in series.Get() do
		LINKS.Add('comic/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('(%d+)')
	local u = API_URL .. '/comic/' .. mid

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('json(*).data')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('string-join((alt_titles, nat_titles), " • ")', json)
	MANGAINFO.CoverLink = CDN_URL .. '/' .. mid .. '/' .. x.XPathString('cover/id', json) .. '.' .. x.XPathString('cover/f', json)
	MANGAINFO.Authors   = x.XPathString('authors', json)
	MANGAINFO.Artists   = x.XPathString('artists', json)
	MANGAINFO.Genres    = x.XPathString('string-join((tags?*/name, type), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json))
	MANGAINFO.Summary   = x.XPathString('description', json)

	for v in x.XPath('chapters?*', json).Get() do
		local id = v.GetProperty('id').ToString()
		local number = v.GetProperty('number').ToString()
		local title = v.GetProperty('name').ToString()
		title = (title ~= 'null') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add('chapter/' .. id)
		MANGAINFO.ChapterNames.Add('Chapter ' .. number .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. URL

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*).data')
	local cid = x.XPathString('id', json)
	local mid = x.XPathString('comic_id', json)
	for v in x.XPath('images?*', json).Get() do
		TASK.PageLinks.Add(CDN_URL .. '/' .. mid .. '/' .. cid .. '/' .. v.GetProperty('id').ToString() .. '.' .. v.GetProperty('f').ToString())
	end

	return true
end