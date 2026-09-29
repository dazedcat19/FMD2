----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local domain = 'mangaball.com'
local API_URL = 'https://api.' .. domain .. '/api/v1'
local DirectoryPagination = '/title/search-advanced?sort_by=created_at&sort_order=desc&adult_mode=all&limit=10000&page='
local Langs = {
    {  nil, 'All' },
    { 'sq', 'Albanian' },
    { 'ar', 'Arabic' },
    { 'bn', 'Bengali' },
    { 'bg', 'Bulgarian' },
    { 'ca', 'Catalan' },
    { 'zh', 'Chinese' },
    { 'zh-hk', 'Chinese (Hong Kong)' },
    { 'cs', 'Czech' },
    { 'da', 'Danish' },
    { 'nl', 'Dutch' },
    { 'en', 'English' },
    { 'fi', 'Finnish' },
    { 'fr', 'French' },
    { 'de', 'German' },
    { 'el', 'Greek' },
    { 'he', 'Hebrew' },
    { 'hi', 'Hindi' },
    { 'hu', 'Hungarian' },
    { 'is', 'Icelandic' },
    { 'id', 'Indonesian' },
    { 'it', 'Italian' },
    { 'jp', 'Japanese' },
    { 'kn', 'Kannada' },
    { 'kr', 'Korean' },
    { 'ml', 'Malayalam' },
    { 'ms', 'Malay' },
    { 'ne', 'Nepali' },
    { 'no', 'Norwegian' },
    { 'fa', 'Persian' },
    { 'pl', 'Polish' },
    { 'pt-br', 'Portuguese (Brazil)' },
    { 'pt-pt', 'Portuguese (Portugal)' },
    { 'ro', 'Romanian' },
    { 'ru', 'Russian' },
    { 'sr', 'Serbian' },
    { 'sk', 'Slovak' },
    { 'sl', 'Slovenian' },
    { 'es', 'Spanish' },
    { 'es-la', 'Spanish (Latin America)' },
    { 'es-419', 'Spanish (Latin America)' },
    { 'sv', 'Swedish' },
    { 'ta', 'Tamil' },
    { 'th', 'Thai' },
    { 'tr', 'Turkish' },
    { 'uk', 'Ukrainian' },
    { 'vi', 'Vietnamese' }
}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Return language names in defined order
function GetLangList()
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
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*).pagination.total_pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).data()').Get() do
		LINKS.Add('title-detail/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local crypto = require 'fmd.crypto'
	local mid = URL:match('(%x+)/?$')
	local u = API_URL .. '/title/detail/' .. mid

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('json(*).data')
	MANGAINFO.Title     = x.XPathString('name', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(alternateName?*, ", ")', info)
	MANGAINFO.CoverLink = 'https://bulbasaur.poke-black-and-white.net/covers/' .. x.XPathString('image?cover?path', info)
	MANGAINFO.Authors   = x.XPathString('string-join(author?*?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join(tags?*?name, ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info))
	MANGAINFO.Summary   = x.XPathString('description', info)

	local optgroup  = MODULE.GetOption('showgroup')
	local optlang   = MODULE.GetOption('lang')
	local optlangid = FindLanguage(optlang)
	local langparam = optlangid and '&language=' .. optlangid or ''
	local page      = 1
	local pages     = nil

	while true do
		u = API_URL .. '/title/chapter-listing?title_id=' .. mid .. '&page=' .. page .. '&limit=100&group_by=chapter_number' .. langparam

		if not HTTP.GET(u) then return net_problem end

		x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
		for v in x.XPath('parse-json(.)?data?*').Get() do
			local language = v.GetProperty('lang').ToString()
			if not optlangid or language == optlangid then
				local number = v.GetProperty('number').ToString()
				local id     = v.GetProperty('id').ToString()
				local volume = v.GetProperty('volume').ToString()
				local title  = v.GetProperty('name').ToString()
				local group  = v.GetProperty('group_name').ToString()

				volume = volume ~= '0' and ('Vol. ' .. volume .. ' ') or ''
				title = (title == '' or title:find(number, 1, true)) and 'Ch. ' .. number or 'Ch. ' .. number .. ' - ' .. title
				local scanlators = optgroup and (' [' .. group .. ']') or ''
				local lang = (optlang == 0) and (' [' .. language .. ']') or ''

				MANGAINFO.ChapterLinks.Add('chapter-detail/' .. id)
				MANGAINFO.ChapterNames.Add(volume .. title .. scanlators .. string.upper(lang))
			end
		end
		if not pages then
			pages = tonumber(x.XPathString('json(*).pagination.total_pages')) or 1
		end
		if page >= pages then break end
		page = page + 1
	end
	if optlang == 0 then
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
	end

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/chapter-detail?chapter_id=' .. URL:match('[^/]+$')

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.chapter.pages()', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '0b6ee312575e4f3583a89c62ce2ed18f'
	m.Name                     = 'MangaBall'
	m.RootURL                  = 'https://' .. domain
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showgroup'] = 'Show group name',
			['lang'] = 'Language:'
		},
		['id_ID'] = {
			['showgroup'] = 'Tampilkan nama grup',
			['lang'] = 'Bahasa:'
		}
	}
	local lang = translations[slang] or translations.en
	local items = table.concat(GetLangList(), '\r\n')
	m.AddOptionComboBox('lang', lang.lang, items, 11)
	m.AddOptionCheckBox('showgroup', lang.showgroup, true)
end