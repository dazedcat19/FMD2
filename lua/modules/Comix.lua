----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '88a001d7619244ef98d13ecd869b8e64'
	m.Name                     = 'Comix'
	m.RootURL                  = 'https://comix.to'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
	m.MaxTaskLimit             = 2
	m.MaxConnectionLimit       = 4

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

local API_URL = 'https://comix.to/api/v2'
local DirectoryPagination = '/manga?order[created_at]=desc&limit=100&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).result.pagination.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).result.items()').Get() do
		LINKS.Add('title/' .. v.GetProperty('hash_id').ToString() .. '-' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. '/manga/' .. URL:match('/title/([^%-]+)%-')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*).result')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('string-join(alt_titles?*, ", ")', json)
	MANGAINFO.CoverLink = x.XPathString('poster/medium', json)
	MANGAINFO.Genres    = x.XPathString('type', json):gsub('^%l', string.upper)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json), 'releasing', 'finished', 'on_hiatus', 'discontinued')
	MANGAINFO.Summary   = x.XPathString('synopsis', json)

	local page = 1
	while true do
		if not HTTP.GET(u .. '/chapters?order[number]=asc&limit=100&page=' .. tostring(page)) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).result.items()').Get() do
			local volume = v.GetProperty('volume').ToString()
			local chapter = v.GetProperty('number').ToString()
			local title = v.GetProperty('name').ToString()
			local scanlators = v.GetProperty('scanlation_group').GetProperty('name').ToString()

			volume = (volume ~= '0') and ('Vol. ' .. volume .. ' ') or ''
			chapter = (chapter ~= '') and ('Ch. ' .. chapter) or ''
			title = (title ~= '') and (' - ' .. title) or ''
			scanlators = (scanlators ~= '') and MODULE.GetOption('showscangroup') and (' [' .. scanlators .. ']') or ''

			MANGAINFO.ChapterLinks.Add(v.GetProperty('chapter_id').ToString())
			MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. scanlators)
		end
		page = page + 1
		local pages = tonumber(x.XPathString('json(*).result.pagination.last_page')) or 1
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/chapters' .. URL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).result.images()', TASK.PageLinks)

	return true
end