----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '214e30f0afec420cafddefe22b4d973c'
	m.Name                     = 'ComicK'
	m.RootURL                  = 'https://comick.live'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'

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

local API_URL = 'https://comick.live/api'
local DirectoryPagination = '/search?order_by=user_follow_count&order_direction=desc&type=comic'

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
	local u = API_URL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local p = 1
	while true do
		sleep(7000)
		for v in x.XPath('json(*).data()').Get() do
			LINKS.Add('comic/' .. v.GetProperty('slug').ToString())
			NAMES.Add(v.GetProperty('title').ToString())
		end
		local next_url = x.XPathString('json(*).next_page_url')
		if next_url == 'null' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. p)
		p = p + 1
		if not HTTP.GET(next_url) then break end
		x.ParseHTML(HTTP.Document)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(//script[@id="comic-data"])')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('string-join(md_titles?*/title, ", ")', json)
	MANGAINFO.CoverLink = x.XPathString('default_thumbnail', json)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*/name, ", ")', json)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*/name, ", ")', json)
	MANGAINFO.Genres    = x.XPathString('string-join(md_comic_md_genres?*/md_genres/name, ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json), '1', '2', '4', '3')
	MANGAINFO.Summary   = x.XPathString('desc', json)

	local demographic = x.XPathString('demographic_name', json):gsub('^%l', string.upper)
	if demographic ~= '' then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. demographic end
	local origin = x.XPathString('origination', json)
	if origin ~= '' then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. origin end

	local slug      = x.XPathString('slug', json)
	local optgroup  = MODULE.GetOption('showscangroup')
	local optlang   = MODULE.GetOption('lang')
	local optlangid = FindLanguage(optlang)
	local page = 1
	local langparam

	if optlangid == nil then langparam = '' else langparam = '&lang=' .. optlangid end

	while true do
		if not HTTP.GET(API_URL .. '/comics/' .. slug .. '/chapter-list?chapOrder=asc&page=' .. tostring(page) .. langparam) then return net_problem end
		local chapters = require 'utils.json'.decode(HTTP.Document.ToString())

		for _, chapter in ipairs(chapters.data) do
			local ignore = false

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
				local volume = (chapter.vol ~= nil and chapter.vol ~= '') and ('Vol. ' .. chapter.vol .. ' ') or ''
				local chapter_number = (chapter.chap ~= nil and chapter.chap ~= '') and ('Ch. ' .. chapter.chap) or ''
				local title = (chapter.title ~= nil and chapter.title ~= '') and (' - ' .. chapter.title) or ''

				local language = (optlang == 0) and (' ' .. chapter.lang) or ''

				local scanlators = ''
				if optgroup then
					if #groups == 0 then
						scanlators = ' [no group]'
					else
						scanlators = ' [' .. table.concat(groups, ', ') .. ']'
					end
				end

				MANGAINFO.ChapterLinks.Add('comic/' .. slug .. '/' .. chapter.hid .. '-chapter-' .. chapter.chap .. '-' .. optlangid)
				MANGAINFO.ChapterNames.Add(volume .. chapter_number .. title .. language .. scanlators)
			end
		end

		page = page + 1
		local pages = chapters.pagination.last_page
		if page > pages then
			break
		end
	end

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(//script[@id="sv-data"]).chapter.images().url', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end