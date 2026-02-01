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
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination

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
	local u = API_URL .. '/series/' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('json(*).data.data')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('nativeTitle', json)
	MANGAINFO.CoverLink = x.XPathString('coverImage', json)
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*/data/name, concat(upper-case(substring(format, 1, 1)), lower-case(substring(format, 2)))), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json))
	MANGAINFO.Summary   = x.XPathString('synopsis', json)

	if not HTTP.GET(u .. '/chapters') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data().data').Get() do
		local title = v.GetProperty('title').ToString()
		title = (title ~= 'null' and title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add('series/' .. slug .. '/chapters/' .. v.GetProperty('index').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. v.GetProperty('index').ToString() .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. URL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.data.images()', TASK.PageLinks)

	return true
end