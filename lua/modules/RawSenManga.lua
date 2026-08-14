----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '8461acb310df4019a6154ad7b3623a42'
	m.Name                     = 'Raw Sen Manga'
	m.RootURL                  = 'https://raw.senmanga.com'
	m.Category                 = 'Raw'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api/'
local DirectoryPagination = 'directory?order=Added&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local series = CreateTXQuery(HTTP.Document).XPath('json(*).series()')
	if series.Count == 0 then return no_error end

	for v in series.Get() do
		LINKS.Add(v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MODULE.RootURL .. API_URL .. 'manga' .. URL

	if not HTTP.GET(u) then return net_problem end

	local x = require 'utils.json'.decode(HTTP.Document.ToString())
	MANGAINFO.Title     = x.title
	MANGAINFO.AltTitles = x.alt_name
	MANGAINFO.CoverLink = x.cover
	MANGAINFO.Authors   = x.author
	MANGAINFO.Artists   = x.artist
	MANGAINFO.Genres    = x.genre
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.status)
	MANGAINFO.Summary   = x.summary

	for _, v in ipairs(x.chapterList) do
		MANGAINFO.ChapterLinks.Add(v.full_url)
		MANGAINFO.ChapterNames.Add(v.title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. API_URL .. 'read' .. URL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).pages()', TASK.PageLinks)

	return true
end