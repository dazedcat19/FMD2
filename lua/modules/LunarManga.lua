----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '53f3ef3178db4c20a47b752d470c8d61'
	m.Name                     = 'Lunar Manga'
	m.RootURL                  = 'https://lunaranime.ru'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['lang'] = 'Language:'
		},
		['id_ID'] = {
			['lang'] = 'Bahasa:'
		}
	}
	local lang = translations[slang] or translations.en
	local items = table.concat(GetLangList(), '\r\n')
	m.AddOptionComboBox('lang', lang.lang, items, 1)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.lunaranime.ru/api/manga'
local DirectoryPagination = '/search?'
local Langs = {
    {  nil, 'All' },
    { 'en', 'English' },
    { 'id', 'Indonesian' },
    { 'ko', 'Korean' }
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

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).manga()').Get() do
		LINKS.Add('manga/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('/([^/]+)$')
	local u = API_URL .. '/title/' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('parse-json(.)?manga')
	MANGAINFO.Title     = x.XPathString('title', json)
	if MANGAINFO.Title == '' and x.XPathString('*'):find('API access denied', 1, true) then
		MANGAINFO.Title = 'Please complete the security check on your browser'
		return no_error
	end
	MANGAINFO.AltTitles = x.XPathString('string-join(json(alternative_titles), ", ")', json)
	MANGAINFO.CoverLink = x.XPathString('cover_url', json)
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Genres    = x.XPathString('string-join((json(genres), json(themes), concat(upper-case(substring(demographic, 1, 1)), lower-case(substring(demographic, 2))))[string-length(.) > 0], ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('publication_status', json))
	MANGAINFO.Summary   = x.XPathString('description', json)

	if not HTTP.GET(u:gsub('title/', '')) then return net_problem end

	local optlang   = MODULE.GetOption('lang')
	local optlangid = FindLanguage(optlang)

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		local language = v.GetProperty('language').ToString()
		if (optlangid == nil) or (language == optlangid) then
			local ch = v.GetProperty('chapter').ToString()
			local title = v.GetProperty('chapter_title').ToString()
			local prefix = (title == 'Chapter ' .. ch) and '' or ('Chapter ' .. ch .. ' - ')
			local lang = (optlang == 0) and (' [' .. language .. ']') or ''

			MANGAINFO.ChapterLinks.Add(slug .. '/' .. ch .. '?language=' .. language)
			MANGAINFO.ChapterNames.Add(prefix .. title .. string.upper(lang))
		end
	end

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. '/manga' .. URL:match('^[^?]+')

	if not HTTP.GET(u) then return false end

	local secret_key = HTTP.Document.ToString():match('\\"secretKey\\"%s*:%s*\\"([^\\"]+)')

	local u = API_URL .. URL

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*).data')
	local session_data = x.XPathString('session_data', json)

	if session_data ~= '' then
		local js = string.format([[
		var CryptoJS = require('utils/crypto-js.min.js');

		var secret = "%s";
		var encrypted = "%s";

		var key = CryptoJS.SHA256(secret);
		var iv = CryptoJS.lib.WordArray.create([0, 0, 0, 0]);

		var decrypted = CryptoJS.AES.decrypt(
			encrypted,
			key,
			{
				iv: iv,
				mode: CryptoJS.mode.CBC,
				padding: CryptoJS.pad.Pkcs7
			}
		);

		decrypted.toString(CryptoJS.enc.Utf8);
		]], secret_key, session_data)

		local result = require 'fmd.duktape'.ExecJS(js)
		x.ParseHTML(result)
	end
	x.XPathStringAll('json(*).data.images()', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end