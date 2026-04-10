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
			['lang'] = 'Language:',
			['pw'] = 'Chapter password:'
		},
		['id_ID'] = {
			['lang'] = 'Bahasa:',
			['pw'] = 'Kata sandi bab:'
		}
	}
	local lang = translations[slang] or translations.en
	local items = table.concat(GetLangList(), '\r\n')
	m.AddOptionComboBox('lang', lang.lang, items, 1)
	m.AddOptionEdit('pw', lang.pw)
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

local function GetKeys(slug, chapter, lang)
	local url = string.format('%s/manga/%s/%s?id=%s', MODULE.RootURL, slug, chapter, lang)

	local js = [[
		const secretWait = async (page) => {
			for (let i = 0; i < 50; i++) {
				const ok = await page.evaluate(() => typeof window.__rctx0 === 'function');
				if (ok) return true;
				await new Promise(r => setTimeout(r, 100));
			}
			return false;
		};

		if (!(await secretWait(page))) {
			console.log(JSON.stringify({ error: "keys not found" }));
			return;
		}

		const result = await page.evaluate(() => ({
			secret_key: window.__rctx0(),
			xor_key: window.__rctx1()
		}));

		console.log(JSON.stringify(result));
	]]

	local result = require 'utils.nodejs'.run_html_load_with_js(url, js)

	return result
end

local b = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

local function EncodeBase64(data)
	return ((data:gsub('.', function(x)
		local r, bits = '', x:byte()
		for i = 8, 1, -1 do
			r = r .. (bits % 2 ^ i - bits % 2 ^ (i - 1) > 0 and '1' or '0')
		end
		return r
	end) .. '0000'):gsub('%d%d%d?%d?%d?%d?', function(x)
		if (#x < 6) then return '' end
		local c = 0
		for i = 1, 6 do
			c = c + (x:sub(i, i) == '1' and 2 ^ (6 - i) or 0)
		end
		return b:sub(c + 1, c +1)
	end) .. ({ '', '==', '=' })[#data % 3 + 1])
end

local function GenerateToken(secret, xor, slug, chapter)
	local function str_to_bytes(s)
		local t = {}
		for i = 1, #s do t[i] = s:byte(i) end
		return t
	end

	local a = str_to_bytes(secret)
	local b = str_to_bytes(xor)

	local cipher = {}
	local max_len = math.max(#a, #b)

	for i = 1, max_len do
		cipher[i] = a[(i - 1) % #a + 1] ~ b[(i - 1) % #b + 1]
	end

	local ts = os.time()
	local rand = ''
	local chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
	for i = 1, 8 do
		local r = math.random(#chars)
		rand = rand .. chars:sub(r, r)
	end

	local payload = string.format("%x|%s|%s|%s", ts, rand, slug, chapter)

	local out = {}
	for i = 1, #payload do
		local c = payload:byte(i)
		local k = cipher[(i - 1) % #cipher + 1]
		out[i] = string.char(c ~ k)
	end

	local encoded = EncodeBase64(table.concat(out))
	encoded = encoded:gsub('%+', '-'):gsub('/', '_'):gsub('=+$', '')

	return encoded
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
	local slug, chapter = URL:match('/([^/]+)/([^/?]+)')
	local lang = URL:match('language=([^&]+)')

	local res = GetKeys(slug, chapter, lang)
	local secret_key, xor_key = res:match('"secret_key":"(.-)","xor_key":"(.-)"')

	local pw = MODULE.GetOption('pw')
	pw = pw ~= '' and '&password=' .. pw or ''

	local token = GenerateToken(secret_key, xor_key, slug, chapter)
	local u = API_URL .. '/r/' .. token .. '?language=' .. lang .. pw

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local session_data = x.XPathString('json(*).data.session_data')

	if session_data ~= '' then
		local iv = string.rep('0', 32)
		local result = require 'fmd.crypto'.AESDecryptCBCSHA256Base64Pkcs7(session_data, secret_key, iv)
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