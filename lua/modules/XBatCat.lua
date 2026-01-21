----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, url, cat)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = 'XBatCat'
		m.RootURL                  = url
		m.Category                 = cat
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.SortedList               = true
	end
	AddWebsiteModule('5257a0c426b94accb6dcee3101308314', 'https://xbat.app', 'English')
	AddWebsiteModule('41e43d6fa1434937afad3bc04a1e8603', 'https://xbat.si')
	AddWebsiteModule('53347251db9d4d5eb92ef8bc6101e5f7', 'https://xbat.io')
	AddWebsiteModule('cf8702f7f5d24bd2a1b9b9904beb246b', 'https://xbat.me')
	AddWebsiteModule('ac808ac813a3499baa65bf640519ed59', 'https://xbat.tv')
	AddWebsiteModule('52c9306a3b93482ea3145c9e619b67fa', 'https://xbat.la')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/ap2/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_comic_browse_pager($select: Comic_Browse_Select) { get_comic_browse_pager( select: $select ) { pages } }","variables":{"select":{"where":"browse","size":120,"sortby":"field_create"}}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).data.get_comic_browse_pager.pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_comic_browse($select: Comic_Browse_Select) { get_comic_browse( select: $select ) { items { data { id name } } } }","variables":{"select":{"where":"browse","page":' .. (URL + 1) .. ',"size":120,"sortby":"field_create"}}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.get_comic_browse.items().data').Get() do
		LINKS.Add('title/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('/(%d+)')
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_comicNode($id: ID!) { get_comicNode(id: $id) { data { name altNames urlCoverOri authors artists genres uploadStatus originalStatus summary } } }","variables":{"id":"' .. mid .. '"}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('json(*).data.get_comicNode.data')
	MANGAINFO.Title     = x.XPathString('name', json)
	MANGAINFO.AltTitles = x.XPathString('string-join(altNames?*, ", ")', json)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('urlCoverOri', json))
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*, ", ")', json)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*, ", ")', json)
	MANGAINFO.Genres    = x.XPathString('string-join(genres?*, ", ")', json):gsub('_', ' '):gsub('(%l)(%w*)', function(first, rest) return first:upper() .. rest end)
	MANGAINFO.Summary   = x.XPathString('summary', json)

	local status = x.XPathString('uploadStatus', json)
	if status == 'null' then status = x.XPathString('originalStatus', json) end
	MANGAINFO.Status = MangaInfoStatusIfPos(status)

	local s = '{"query":"query get_comic_chapterList($comicId: ID!, $start: Int) { get_comic_chapterList(comicId: $comicId, start: $start) { data { id dname title } } }","variables":{"comicId":"' .. mid .. '","start":-1}}'
	HTTP.Reset()
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.get_comic_chapterList().data').Get() do
		local chapter = v.GetProperty('dname').ToString()
		local title = v.GetProperty('title').ToString()
		title = (title ~= 'null' and title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(chapter .. title)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_chapterNode($id: ID!) { get_chapterNode(id: $id) { data { imageFile { urlList } } } }","variables":{"id":"' .. URL:match('(%d+)') .. '"}}'
	HTTP.Reset()
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.get_chapterNode.data.imageFile.urlList()', TASK.PageLinks)

	return true
end