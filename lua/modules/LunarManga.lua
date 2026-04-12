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

-- Seed random number generator once.
math.randomseed(os.time())

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

local function RNG(seed)
	local s = (seed ~ 0x5DEECE66D) & ((1 << 48) - 1)
	return function(n)
		s = (s * 0x5DEECE66D + 0xB) & ((1 << 48) - 1)
		local result = (s >> (48 - 31)) & 0x7FFFFFFF
		return result % n
	end
end

local function GenerateRctx(data)
	local encoded
	for k, v in pairs(data) do
		if #k == 2 then 
			encoded = v:reverse()
			break 
		end
	end
	if not encoded then return '' end

	local decoded = require 'fmd.crypto'.DecodeBase64(encoded)

	local parts = {}
	for p in decoded:gmatch('[^.]+') do
		table.insert(parts, p)
	end
	
	local xor_key = tonumber(parts[1], 16)

	local hex = ''
	for i = 2, #parts do
		hex = hex .. (data[parts[i]] or '')
	end

	local d = {}
	local idx = 0
	for h in hex:gmatch('..') do
		local val = tonumber(h, 16)
		local k = (xor_key + idx * 7 + 3) & 0xFF
		table.insert(d, val ~ k)
		idx = idx + 1
	end
	if #d == 0 then return '' end

	local rand = RNG(#d)

	local h = {}
	for i = 0, 255 do h[i] = i end

	for i = 255, 1, -1 do
		local j = rand(i + 1)
		h[i], h[j] = h[j], h[i]
	end

	local s = {}
	for i = 0, 255 do s[h[i]] = i end
	
	local u = {}
	for i = 1, #d do u[i] = rand(256) end

	for round = 0, 2 do
		for t = 1, #d do
			local val = d[t] ~ u[((t - 1) + 7 * round) % #u + 1]
			val = h[val]
			local shift = ((t - 1) + 3 * round + 1) % 7 + 1
			d[t] = ((val << shift) | (val >> (8 - shift))) & 0xFF
		end
		for t = 2, #d do
			d[t] = d[t] ~ d[t - 1]
		end
	end

	local e = d
	for round = 2, 0, -1 do
		for t = #e, 2, -1 do e[t] = e[t] ~ e[t - 1] end
		for t = 1, #e do
			local shift = ((t - 1) + 3 * round + 1) % 7 + 1
			local val = ((e[t] >> shift) | (e[t] << (8 - shift))) & 0xFF
			val = s[val]
			e[t] = val ~ u[((t - 1) + 7 * round) % #u + 1]
		end
	end

	local result = ''
	for i = 1, #e do result = result .. string.char(e[i]) end

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

local function GenerateToken(rctx0, rctx1, slug, chapter)
	local len0, len1 = #rctx0, #rctx1
	local max_len = math.max(len0, len1)

	local xor_key = {}
	for i = 1, max_len do
		local a = rctx0:byte((i - 1) % len0 + 1)
		local b = rctx1:byte((i - 1) % len1 + 1)
		xor_key[i] = a ~ b
	end

	local ts = os.time()
	local chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
	local rand_tab = {}
	for i = 1, 8 do
		local r = math.random(#chars)
		rand_tab[i] = chars:sub(r, r)
	end
	local rand = table.concat(rand_tab)

	local payload = string.format("%x|%s|%s|%s", ts, rand, slug, chapter)

	local out = {}
	local key_len = #xor_key
	for i = 1, #payload do
		local c = payload:byte(i)
		local k = xor_key[(i - 1) % key_len + 1]
		out[i] = string.char(c ~ k)
	end

	local encoded = EncodeBase64(table.concat(out))
	return encoded:gsub('%+', '-'):gsub('/', '_'):gsub('=+$', '')
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
	local info = x.XPath('parse-json(.)?manga')
	MANGAINFO.Title     = x.XPathString('title', info)
	if MANGAINFO.Title == '' and x.XPathString('*'):find('API access denied', 1, true) then
		MANGAINFO.Title = 'Please complete the security check on your browser'
		return no_error
	end
	MANGAINFO.AltTitles = x.XPathString('string-join(json(alternative_titles), ", ")', info)
	MANGAINFO.CoverLink = x.XPathString('cover_url', info)
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Artists   = x.XPathString('artist', info)
	MANGAINFO.Genres    = x.XPathString('string-join((json(genres), json(themes), concat(upper-case(substring(demographic, 1, 1)), lower-case(substring(demographic, 2))))[string-length(.) > 0], ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('publication_status', info))
	MANGAINFO.Summary   = x.XPathString('description', info)

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
	
	local u = MODULE.RootURL .. '/manga' .. URL:match('^[^?]+')

	if not HTTP.GET(u) then return false end

	local body = HTTP.Document.ToString()

	local seeds = {}
	for segment in body:gmatch('self%.__next_f%.push%(%[1,"(.-)"%]%)') do
		local decoded = segment:gsub('\\\\', '\\'):gsub('\\"', '"')
		for obj in decoded:gmatch('({[^{}]-})') do
			local success, data = pcall(function()
				return require 'utils.json'.decode(obj)
			end)
			if success and type(data) == 'table' then
				for k, _ in pairs(data) do
					if #k == 2 then table.insert(seeds, data) break end
				end
			end
		end
	end

	if #seeds < 2 then return false end

	local secret_key = GenerateRctx(seeds[1])
	local xor_key = GenerateRctx(seeds[2])

	local pw = MODULE.GetOption('pw')
	pw = pw ~= '' and '&password=' .. pw or ''

	local token = GenerateToken(secret_key, xor_key, slug, chapter)

	if not HTTP.GET(API_URL .. '/r/' .. token .. '?language=' .. lang .. pw) then return false end

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

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end