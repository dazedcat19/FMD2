----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '0b6ee312575e4f3583a89c62ce2ed18f'
	m.Name                     = 'MangaBall'
	m.RootURL                  = 'https://mangaball.net'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
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

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://mangaball.net/api/v1'
local DirectoryPagination = 'filters[sort]=created_at_desc&filters[page]='
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

-- Set the required http headers for making a request
local function SetRequestHeaders(x)
	HTTP.Reset()
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
	HTTP.Headers.Values['X-CSRF-TOKEN'] = x.XPathString('//meta[@name="csrf-token"]/@content')
	HTTP.Cookies.Values['show18PlusContent'] = 'true'
	HTTP.MimeType = 'application/x-www-form-urlencoded; charset=UTF-8'
end

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
	local s = DirectoryPagination .. 1

	if not HTTP.GET(MODULE.RootURL) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	SetRequestHeaders(x)

	if not HTTP.POST(API_URL .. '/title/search-advanced/', s) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).pagination.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local s = DirectoryPagination .. (URL + 1)

	if not HTTP.GET(MODULE.RootURL) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	SetRequestHeaders(x)

	if not HTTP.POST(API_URL .. '/title/search-advanced/', s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="mb-2"]/h6')
	MANGAINFO.AltTitles = x.XPathStringAll('//div[contains(@class, "alternate-name-container")]/text()')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "featured-cover")]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//span[@data-person-id]')
	MANGAINFO.Genres    = x.XPathStringAll('//span[@data-tag-id]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "badge-status")]/text()'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="description-text"]/p')

	local s = 'title_id=' .. URL:match('%-(%x+)/?$')
	SetRequestHeaders(x)

	if not HTTP.POST(API_URL .. '/chapter/chapter-listing-by-title-id/', s) then return net_problem end

	local optgroup  = MODULE.GetOption('showgroup')
	local optlang   = MODULE.GetOption('lang')
	local optlangid = FindLanguage(optlang)

	local x = CreateTXQuery(HTTP.Document)
	for ch in x.XPath('json(*).ALL_CHAPTERS()').Get() do
		local chapter = ch.GetProperty('number').ToString()
		local number  = ch.GetProperty('number_float').ToString()

		for tr in x.XPath('translations?*', ch).Get() do
			local language = tr.GetProperty('language').ToString()

			if not optlangid or language == optlangid then
				local id     = tr.GetProperty('id').ToString()
				local volume = tr.GetProperty('volume').ToString()
				local title  = tr.GetProperty('name').ToString():gsub(':', ' -')
				local group  = tr.GetProperty('group').GetProperty('_id').ToString()

				volume = (volume ~= '0' and not title:find('Vol. ' .. volume, 1, true)) and ('Vol. ' .. volume .. ' ') or ''
				title = title:find(number, 1, true) and title or chapter .. ' - ' .. title
				local scanlators = optgroup and (' [' .. group .. ']') or ''
				local lang = (optlang == 0) and (' [' .. language .. ']') or ''

				MANGAINFO.ChapterLinks.Add('chapter-detail/' .. id)
				MANGAINFO.ChapterNames.Add(volume .. title .. scanlators .. string.upper(lang))
			end
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(//script[contains(., "chapterImages")]/substring-before(substring-after(., "parse(`"), "`"))()', TASK.PageLinks)

	return true
end