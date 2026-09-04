----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/search.data?sortBy=alphabetical&_routes=pages/SearchPage&page='
local json = require 'utils.json'

local Langs = {
    {  nil, 'All' },
    { 'ar', 'Arabic' },
    { 'bn', 'Bengali' },
    { 'bg', 'Bulgarian' },
    { 'my', 'Burmese' },
    { 'zh', 'Chinese' },
    { 'zh-hk', 'Chinese (Hong Kong)' },
    { 'cs', 'Czech' },
    { 'da', 'Danish' },
    { 'nl', 'Dutch' },
    { 'en', 'English' },
    { 'fi', 'Finnish' },
    { 'fr', 'French' },
    { 'ka', 'Georgian' },
    { 'de', 'German' },
    { 'el', 'Greek' },
    { 'he', 'Hebrew' },
    { 'hi', 'Hindi' },
    { 'hu', 'Hungarian' },
    { 'id', 'Indonesian' },
    { 'it', 'Italian' },
    { 'ja', 'Japanese' },
    { 'ko', 'Korean' },
    { 'la', 'Latin' },
    { 'lt', 'Lithuanian' },
    { 'ms', 'Malay' },
    { 'mn', 'Mongolian' },
    { 'no', 'Norwegian' },
    { 'fa', 'Persian' },
    { 'pl', 'Polish' },
    { 'pt-br', 'Portuguese (Brazil)' },
    { 'pt', 'Portuguese (Portugal)' },
    { 'ro', 'Romanian' },
    { 'ru', 'Russian' },
    { 'es', 'Spanish' },
    { 'es-la', 'Spanish (Latin America)' },
    { 'sv', 'Swedish' },
    { 'tl', 'Tagalog' },
    { 'th', 'Thai' },
    { 'tr', 'Turkish' },
    { 'uk', 'Ukrainian' },
    { 'vi', 'Vietnamese' }
}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function DecodeRSC(flat)
	local cache = {}
	local NIL = {}

	local function resolve(i)
		if i == nil or i < 0 then return nil end

		local v = cache[i]
		if v then
			return v == NIL and nil or v
		end

		local el = flat[i + 1]

		if type(el) ~= 'table' then
			cache[i] = el or NIL
			return el
		end

		local out = {}

		if el[1] then
			for j = 1, #el do
				out[j] = resolve(el[j])
			end
		else
			for k, v in pairs(el) do
				local idx = tonumber(k:sub(2))
				if idx and k:byte() == 95 then
					out[flat[idx + 1]] = resolve(v)
				end
			end
		end

		cache[i] = out
		return out
	end

	return resolve(0)
end

local function ParseList(v)
	if not v or v == '' then return '' end
	if v:sub(1, 1) == '[' then
		return table.concat(json.decode(v), ', ')
	end
	return v
end

-- Return language names in defined order
local function GetLangList()
	local t = {}
	for _, v in ipairs(Langs) do
		table.insert(t, v[2])
	end
	return t
end

-- Return language key by index
local function FindLanguage(lang)
	return Langs[lang + 1][1]
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	local decoded = DecodeRSC(json.decode(HTTP.Document.ToString()))
	PAGENUMBER = tonumber(decoded['pages/SearchPage'].data.pagination.total_pages) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local decoded = DecodeRSC(json.decode(HTTP.Document.ToString()))
	local manga = decoded['pages/SearchPage'].data.results
	for _, v in ipairs(manga) do
		LINKS.Add('manga/' .. v.id)
		NAMES.Add(v.title)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('/([^/]+)$')
	local u = MODULE.RootURL .. '/manga/' .. mid .. '.data?_routes=pages/MangaDetailPage'

	if not HTTP.GET(u) then return net_problem end

	local rc = HTTP.ResultCode
	if (rc == 403) or (rc == 429) or (rc == 503) then MANGAINFO.Title = 'Cloudflare workaround is required' return no_error end
	local decoded = DecodeRSC(json.decode(HTTP.Document.ToString()))
	local manga = decoded['pages/MangaDetailPage'].data.mangaData.manga
	MANGAINFO.Title     = manga.title
	MANGAINFO.AltTitles = table.concat(manga.alt_titles or {}, ', ')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, manga.photo)
	MANGAINFO.Authors   = ParseList(manga.authors)
	MANGAINFO.Artists   = ParseList(manga.artists)
	MANGAINFO.Genres    = table.concat(manga.genres or {}, ', ')
	MANGAINFO.Status    = MangaInfoStatusIfPos(manga.status)
	MANGAINFO.Summary   = manga.description

	local listtype     = {'chapters/list', 'volumes'}
	local sel_listtype = (MODULE.GetOption('listtype') or 0) + 1
	local optgroup     = MODULE.GetOption('showscangroup')
	local optlang      = MODULE.GetOption('lang')
	local optlangid    = FindLanguage(optlang)
	local langparam    = optlangid and '?lang=' .. optlangid or ''

	if not HTTP.GET(MODULE.RootURL .. '/api/manga/' .. mid .. '/' .. listtype[sel_listtype] .. langparam) then return net_problem end

	local chapters = json.decode(HTTP.Document.ToString())
	for _, v in ipairs(chapters or {}) do
		local number = v.chapter_number
		local volume = v.volume_number
		local title = v.chapter_title
		local group = v.group_name or v.scanlator_name
		local source = v.source
		local lang = v.language
		local vol = ''
		local name = ''
		local path

		if volume then
			vol = 'Vol. ' .. volume .. ' '
		end

		if number then
			if title and not title:find('Chapter ' .. number, 1, true) then
				name = 'Ch. ' .. number .. ' - ' .. title
			else
				name = 'Ch. ' .. number
			end
		end

		local scanlator = ''
		if optgroup then
			if group and group ~= '' then
				scanlator = ' [' .. group .. ']'
			else
				scanlator = ' [No Group]'
			end
		end

		if not source or source == 'user' then
			path = 'uploads'
		else
			path = 'chapters'
		end

		lang = not optlangid and ' [' .. lang:upper() .. ']' or ''

		MANGAINFO.ChapterLinks.Add(path .. '/' .. v.id)
		MANGAINFO.ChapterNames.Add(vol .. name .. scanlator .. lang)
	end
	if sel_listtype == 2 then
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. '/api' .. URL .. '/images'

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).images().url').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '64b2532a580f4ebd89850bdb2a567478'
	m.Name                     = 'MangaDotNet'
	m.RootURL                  = 'https://mangadot.net'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showscangroup'] = 'Show scanlation group',
			['lang'] = 'Language:',
			['listtype'] = 'List type:',
			['type'] = 'Chapter\nVolume'
		},
		['id_ID'] = {
			['showscangroup'] = 'Tampilkan grup scanlation',
			['lang'] = 'Bahasa:',
			['listtype'] = 'Tipe daftar:',
			['type'] = 'Bab\nJilid'
		}
	}
	local lang = translations[slang] or translations.en
	local items = table.concat(GetLangList(), '\r\n')
	m.AddOptionComboBox('lang', lang.lang, items, 10)
	m.AddOptionComboBox('listtype', lang.listtype, lang.type, 0)
	m.AddOptionCheckBox('showscangroup', lang.showscangroup, false)
end