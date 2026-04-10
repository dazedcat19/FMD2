----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2da79eaeb73d4539ad47d4eeb9549415'
	m.Name                     = 'Atsumaru'
	m.RootURL                  = 'https://atsu.moe'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnCheckSite              = 'CheckSite'

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local translations = {
		['en'] = {
			['showscangroup'] = 'Show scanlation group'
		},
		['id_ID'] = {
			['showscangroup'] = 'Tampilkan grup scanlation'
		}
	}
	local lang = translations[slang] or translations['en']
	m.AddOptionCheckBox('showscangroup', lang.showscangroup, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://atsu.moe/api'
local DirectoryPagination = '/explore/filteredView'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination
	local s = '{"filter":{"tags":[],"status":[],"types":[],"showAdult":true},"page":' .. URL .. '}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	local series = CreateTXQuery(HTTP.Document).XPath('json(*).items()')
	if series.Count == 0 then return no_error end

	for v in series.Get() do
		LINKS.Add('manga/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('/([^/]+)$')
	local u = API_URL .. '/manga/page?id=' .. mid

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(*).mangaPage')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(otherNames?*, ", ")', info)
	MANGAINFO.CoverLink = MODULE.RootURL .. '/static/' .. x.XPathString('poster?image', info)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, type), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info))
	MANGAINFO.Summary   = x.XPathString('synopsis', info)

	local optgroup = MODULE.GetOption('showscangroup')
	local scanlator_map = {}

	if optgroup then
		for v in x.XPath('scanlators?*', info).Get() do
			local id   = v.GetProperty('id').ToString()
			local name = v.GetProperty('name').ToString()
			scanlator_map[id] = name
		end
	end

	if not HTTP.GET(API_URL .. '/manga/allChapters?mangaId=' .. mid) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).chapters()').Get() do
		local scanlator = ''

		if optgroup then
			local scanlator_id = v.GetProperty('scanlationMangaId').ToString()
			local name = scanlator_map[scanlator_id]

			if name then
				scanlator = ' [' .. name .. ']'
			end
		end

		MANGAINFO.ChapterLinks.Add(mid .. '/' .. v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('title').ToString() .. scanlator)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local mid, cid = URL:match('^/([^/]+)/([^/]+)$')
	local u = API_URL .. '/read/chapter?mangaId=' .. mid .. '&chapterId=' .. cid

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).readChapter.pages()').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.GetProperty('image').ToString()))
	end

	return true
end

-- Verify the module's functionality by checking a specific manga and chapter.
function CheckSite()
	MANGACHECK.MangaURL     = '/manga/6rUzU'
	MANGACHECK.MangaTitle   = 'The Academy’s Sashimi Sword Master'
	MANGACHECK.ChapterURL   = '/6rUzU/pIEtz'
	MANGACHECK.ChapterTitle = 'Episode 74'
	MANGACHECK.ChapterURLAddRootHost = false
end