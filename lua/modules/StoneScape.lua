----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'b78cfc8821a445719548a7636faf82cc'
	m.Name                     = 'StoneScape'
	m.RootURL                  = 'https://stonescape.xyz'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api'
local DirectoryPagination = '/series?limit=50&contentType=manhwa&page='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Convert API genre keys to readable genre names.
local function GetGenre(genre)
	local genres = {
		['genderbender'] = 'Gender Bender',
		['martialarts'] = 'Martial Arts',
		['post-apocalyptic'] = 'Post-Apocalyptic',
		['schoollife'] = 'School Life',
		['sci-fi'] = 'Sci-Fi',
		['shoujoai'] = 'Shoujo Ai',
		['shounenai'] = 'Shounen Ai',
		['sliceoflife'] = 'Slice of Life',
		['video-games'] = 'Video Games'
	}
	if genres[genre] then
		return genres[genre]
	end

	return (genre:gsub('^%l', string.upper))
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).pagination.totalPages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MODULE.RootURL .. API_URL .. '/series/by-slug/' .. URL:match('/([^/]+)$')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('parse-json(.)')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('originalTitle', json)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('coverUrl', json))
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('publicationStatus', json))
	MANGAINFO.Summary   = x.XPathString('description', json)

	local genres = {}
	for genre in x.XPath('(genres?*, contentType)', json).Get() do
		table.insert(genres, GetGenre(genre.ToString()))
	end
	MANGAINFO.Genres = table.concat(genres, ', ')

	if not HTTP.GET(u .. '/chapters') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('parse-json(.).chapters?*').Get() do
		local chapter = v.GetProperty('chapterNumber').ToString():gsub('%.00', '')
		local title = v.GetProperty('title').ToString()
		title = (title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(v.GetProperty('chapterId').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. chapter .. title)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. API_URL .. '/chapters' .. URL .. '/pages'

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).pages().url').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
	end

	return true
end