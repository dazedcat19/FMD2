----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '214e30f0afec420cafddefe22b4d973c'
	m.Name                     = 'ComicK'
	m.RootURL                  = 'https://comick.io'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.TotalDirectory           = #DirectoryPages

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['showscangroup'] = 'Show scanlation group',
			['lang'] = 'Language:'
		},
		['id_ID'] = {
			['showscangroup'] = 'Tampilkan grup scanlation',
			['lang'] = 'Bahasa:'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	local items = 'All'
	local t = GetLangList()
	for k, v in ipairs(t) do items = items .. '\r\n' .. v end
	m.AddOptionComboBox('lang', lang:get('lang'), items, 11)
	m.AddOptionCheckBox('showscangroup', lang:get('showscangroup'), false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.comick.io'

DirectoryPages = {'&status=1&demographic=1', '&status=1&demographic=2', '&status=1&demographic=3', '&status=1&demographic=4', '&status=1&demographic=5',
	'&status=2&demographic=1', '&status=2&demographic=2', '&status=2&demographic=3', '&status=2&demographic=4', '&status=2&demographic=5',
	'&status=3&demographic=1', '&status=3&demographic=2', '&status=3&demographic=3', '&status=3&demographic=4', '&status=3&demographic=5',
	'&status=4&demographic=1', '&status=4&demographic=2', '&status=4&demographic=3', '&status=4&demographic=4', '&status=4&demographic=5'}

local Langs = {
	['ar'] = 'Arabic',
	['bn'] = 'Bengali',
	['bg'] = 'Bulgarian',
	['my'] = 'Burmese',
	['ca'] = 'Catalan',
	['zh'] = 'Chinese (Simp)',
	['zh-hk'] = 'Chinese (Trad)',
	['cs'] = 'Czech',
	['da'] = 'Danish',
	['nl'] = 'Dutch',
	['en'] = 'English',
	['fr'] = 'French',
	['de'] = 'German',
	['el'] = 'Greek',
	['he'] = 'Hebrew',
	['hi'] = 'Hindi',
	['hu'] = 'Hungarian',
	['id'] = 'Indonesian',
	['it'] = 'Italian',
	['ja'] = 'Japanese',
	['ko'] = 'Korean',
	['lt'] = 'Lithuanian',
	['ms'] = 'Malay',
	['mn'] = 'Mongolian',
	['no'] = 'Norwegian',
	['fa'] = 'Persian',
	['pl'] = 'Polish',
	['pt-br'] = 'Portuguese (Br)',
	['pt'] = 'Portuguese (Pt)',
	['ro'] = 'Romanian',
	['ru'] = 'Russian',
	['es'] = 'Spanish (Es)',
	['es-419'] = 'Spanish (LATAM)',
	['sv'] = 'Swedish',
	['tl'] = 'Tagalog',
	['th'] = 'Thai',
	['tr'] = 'Turkish',
	['uk'] = 'Ukrainian',
	['vi'] = 'Vietnamese'
}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Parse ISO 8601 formatted date string and return Unix timestamp.
local function parse_iso8601(date_str)
    local pattern = '(%d+)%-(%d+)%-(%d+)T(%d+):(%d+):(%d+)'
    local year, month, day, hour, min, sec = date_str:match(pattern)
    return os.time({
        year = year,
        month = month,
        day = day,
        hour = hour,
        min = min,
        sec = sec
    })
end

-- Return a sorted list of all available language names.
function GetLangList()
	local t = {}
	for k, v in pairs(Langs) do table.insert(t, v) end
	table.sort(t)
	return t
end

-- Find and return the language key for a given language name or index.
local function FindLanguage(lang)
	local t = GetLangList()
	for i, v in ipairs(t) do
		if i == lang then
			lang = v
			break
		end
	end
	for k, v in pairs(Langs) do
		if v == lang then return k end
	end
	return nil
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. '/v1.0/search?limit=300' .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. '&page=' .. (URL + 1) .. '&sort=user_follow_count'
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	if not HTTP.GET(u) then return net_problem end

	local series = CreateTXQuery(HTTP.Document).XPath('json(*)()')
	if series.Count == 0 then return no_error end

	for v in series.Get() do
		LINKS.Add('comic/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local json = require 'utils.json'
	local u = API_URL .. URL:match('(/comic/.-)$')
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	if not HTTP.GET(u) then return net_problem end

	local x = json.decode(HTTP.Document.ToString())
	MANGAINFO.Title      = x.comic.title
	MANGAINFO.CoverLink  = 'https://meo3.comick.pictures/' .. x.comic.md_covers[1].b2key
	MANGAINFO.Status     = MangaInfoStatusIfPos(x.comic.status, '1', '2', '4', '3')
	MANGAINFO.Summary    = x.comic.desc

	local alttitles = {}
	for _, alttitle in ipairs(x.comic.md_titles) do
		table.insert(alttitles, alttitle.title)
	end
	MANGAINFO.AltTitles = table.concat(alttitles, ', ')

	local authors = {}
	for _, author in ipairs(x.authors) do
		table.insert(authors, author.name)
	end
	MANGAINFO.Authors = table.concat(authors, ', ')

	local artists = {}
	for _, artist in ipairs(x.artists) do
		table.insert(artists, artist.name)
	end
	MANGAINFO.Artists = table.concat(artists, ', ')

	local genres = {}
	for _, genre in ipairs(x.comic.md_comic_md_genres) do
		table.insert(genres, genre.md_genres.name)
	end
	MANGAINFO.Genres  = table.concat(genres, ', ')

	local demographic = x.demographic
	if demographic ~= nil then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. demographic end

	local manga_id   = x.comic.hid
	local optgroup  = MODULE.GetOption('showscangroup')
	local optlang   = MODULE.GetOption('lang')
	local optlangid = FindLanguage(optlang)
	local page = 1
	local langparam

	if optlangid == nil then langparam = '' else langparam = '&lang=' .. optlangid end

	while true do
		HTTP.Reset()
		HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'
		if not HTTP.GET(API_URL .. '/comic/' .. manga_id .. '/chapters?chap-order=1&page=' .. tostring(page) .. langparam) then return net_problem end
		local chapter_data = json.decode(HTTP.Document.ToString())
		local chapters = chapter_data.chapters

		for _, chapter in ipairs(chapters) do
			local ignore = false

			-- Ignore chapters that have a delayed release time
			if chapter.publish_at ~= nil then
				if parse_iso8601(chapter.publish_at) >= os.time(os.date('!*t')) then
					ignore = true
				end
			end

			local groups = {}
			if chapter.md_chapters_groups ~= nil then
				for _, group in ipairs(chapter.md_chapters_groups) do
					if group.md_groups ~= nil then
						table.insert(groups, Trim(group.md_groups.title))
					end
				end
			end

			if #groups == 0 and chapter.group_name ~= nil then
				for _, group_name in ipairs(chapter.group_name) do
					table.insert(groups, group_name)
				end
			end

			if not ignore then
				local volume = chapter.vol ~= nil and chapter.vol ~= '' and string.format('Vol. %s ', chapter.vol) or ''
				local chapter_number = chapter.chap ~= nil and chapter.chap ~= '' and string.format('Ch. %s', chapter.chap) or ''
				local title = chapter.title ~= nil and chapter.title ~= '' and string.format(' - %s', chapter.title) or ''

				local language = optlang == 0 and string.format(' [%s]', chapter.lang) or ''

				local scanlators = ''
				if optgroup then
					if #groups == 0 then
						scanlators = ' [no group]'
					else
						scanlators = ' [' .. table.concat(groups, ', ') .. ']'
					end
				end

				MANGAINFO.ChapterLinks.Add(chapter.hid)
				MANGAINFO.ChapterNames.Add(volume .. chapter_number .. title .. language .. scanlators)
			end
		end

		page = page + 1
		local pages = math.ceil(chapter_data.total / chapter_data.limit)
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/chapter' .. URL .. '?tachiyomi=true'
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).chapter.images().url', TASK.PageLinks)

	return true
end