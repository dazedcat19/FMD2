----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local RootURL = 'https://mangafire.to'
local API_URL = RootURL .. '/api'
local DirectoryPagination = '/titles?limit=100&order[created_at]=desc&page='

local Langs = {
	{   nil, 'All' },
	{  'en', 'English' },
	{  'fr', 'French' },
	{  'ja', 'Japanese' },
	{ 'pt-br', 'Portuguese (Br)' },
	{  'pt', 'Portuguese (Pt)' },
	{ 'es-la', 'Spanish (LATAM)' },
	{  'es', 'Spanish (Es)' }
}

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '23eb3a472201427e8824ecdd5223bad7'
	m.Name                     = 'MangaFire'
	m.RootURL                  = RootURL
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['lang'] = 'Language:',
			['listtype'] = 'List type:',
			['ltype'] = 'Chapter\nVolume',
			['chaptertype'] = 'Chapter type:',
			['ctype'] = 'All\nOfficial\nUnofficial',
			['deduplicatechapters'] = 'Deduplicate chapters (prefer official)'
		},
		['id_ID'] = {
			['lang'] = 'Bahasa:',
			['listtype'] = 'Tipe daftar:',
			['ltype'] = 'Bab\nJilid',
			['chaptertype'] = 'Tipe bab:',
			['ctype'] = 'Semua\nResmi\nTidak resmi',
			['deduplicatechapters'] = 'Hapus bab ganda (utamakan bab resmi)'
		}
	}
	local lang = translations[slang] or translations.en
	local items = table.concat(GetLangList(), '\r\n')
	m.AddOptionComboBox('lang', lang.lang, items, 1)
	m.AddOptionComboBox('listtype', lang.listtype, lang.ltype, 0)
	m.AddOptionComboBox('chaptertype', lang.chaptertype, lang.ctype, 0)
	m.AddOptionCheckBox('deduplicatechapters', lang.deduplicatechapters, false)
end

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local stages = {
	{
		table_b64 = 'yINlmUNho8VYJT+ibTIP+9ESiULpVEtMOoD6U6lRE0R/xwXo/Xp9NrUgC4cw/'
		         .. 'Lmo33vUyjUE40kUoEWIr/fxfNNcq2s79ShQ5NhNrFnJ4hXPwOu/SuXzIbuTQKG'
		         .. 'Fvfm08E9jvCfqAtoDqvQq3dVWPQFmJjgvkISBeXY3BgANR+yVnjGbcxZ47d6k'
		         .. 'LNfZPIayTq3/YGySb1KuVZodWp/WGNAO5pfMcpaK53Hhs0allBszaMaxuouOwd'
		         .. 'xbwgxIw6YunSsXjI05Yi0j9j4eHKfSXR8Ifo/Od+8iamRfCXTyvm7NGRGYdcQ'
		         .. '0ywcK/u6RXhrbcCm4t2eCtrDgQVecJGkQ+A==',
		key_b64   = '0Ec58JOY3uBzJK9m3zqIOpdlF7UFiax9DmA=',
		iv        = 0x5A
	},
	{
		table_b64 = 'IUFltCxD3Oc2cwCgkJffthaOg9cgPUb0LgW6H/VtfcF0kc5F25t+aWj6JH9V'
		         .. 'OhOaY0rAFdUxlDnl5BLNvwEJvQtP5qcw7vdb/K+chnbwnspSHT8mz5lqwz41T'
		         .. 'ezG0hkO06FTjJZhsyNuFLDpD2ZZxQj/QIRcF90zpmQ7Byu483WsQqUE0C342H'
		         .. 'L+JXngRB6fRzxRyVTaKu83h7UYTJ0QMt6ixFh6S3F8gqkKwrGTL3jHNBsD45U'
		         .. 'nifK8+RGtishQV2K3rujLKEkiZxpr2dYcudFW4oFsDKhad3CLBvuyTqsCo4B7m'
		         .. 'L5IKQ1vXo/MOOvq1I1d8ar9X6Ttu5KF4fZgiA==',
		key_b64   = 'AAdjb1iPY8CiDmq9H34tKTBF8a3oDQ==',
		iv        = 0x35
	},
	{
		table_b64 = 'NQHlu1/wVO5EmkwQymF810qqY2xG1k2obcas4Z9mCsPEIFl9pRIjFxbJ7ybM'
		         .. 'HbBckT5Ton85E0FOeHezbh/mjlEYpmpnlXOS8dgrqeq2KfxImTh1YK9y0PeMN'
		         .. 'hzA1OQzSY9brYOJq/l2QnE/hwOeZIhPixVSKIUlDb5vLcH6RWKxkIEMuP0bDw'
		         .. 'IqQ71AJJaEaMJL7A6YtyIwoRT+L5v4aZzodN/0+3nOGsfblFjgxSfPzVDjNFe'
		         .. 'Nl5P26+kEC/8AHgdrpAbt3hHz3HrRN1Y6e+JHgF7ncFWnoF0y3THL1S71WgWG'
		         .. 'Ca6KtSzTCCG58n68nTyj2T3Sshk7utqCtMi/ZQ==',
		key_b64   = 'DELOJgPsVaCcblDtTGMdHzM=',
		iv        = 0xBA
	}
}

local function EncryptStage(data, tbl, key, iv)
	local out = {}
	local previous = iv
	local keylen = #key
	for i = 1, #data do
		previous = tbl:byte((data:byte(i) ~ key:byte(((i - 1) % keylen) + 1) ~ previous) + 1)
		out[i] = string.char(previous)
	end
	return table.concat(out)
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
		return b:sub(c + 1, c + 1)
	end) .. ({ '', '==', '=' })[#data % 3 + 1])
end

local function GenerateVRF(input)
	local crypto = require 'fmd.crypto'
	local bytes = input

	for _, st in ipairs(stages) do
		local tbl = crypto.DecodeBase64(st.table_b64)
		local key = crypto.DecodeBase64(st.key_b64)
		bytes = EncryptStage(bytes, tbl, key, st.iv)
	end

	return EncodeBase64(bytes):gsub('%+', '-'):gsub('/', '_'):gsub('=+$', '')
end

function GetLangList()
	local t = {}
	for k, v in pairs(Langs) do table.insert(t, v) end
	table.sort(t)
	return t
end

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
	local path = DirectoryPagination .. 1
	local vrf = GenerateVRF(path)
	local u = API_URL .. path .. '&vrf=' .. vrf

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).meta.lastPage')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local path = DirectoryPagination .. (URL + 1)
	local vrf = GenerateVRF(path)
	local u = API_URL .. path .. '&vrf=' .. vrf

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).items()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local hid = URL:match('%.(%w+)$') or URL:match('/(%w+)%-')
	local vrf = GenerateVRF('/titles/' .. hid)
	local u = API_URL .. '/titles/' .. hid .. '?vrf=' .. vrf

	if not HTTP.GET(u) then	return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('json(*).data')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(altTitles?*, ", ")', info)
	MANGAINFO.CoverLink = x.XPathString('poster?large', info)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*?title, ", ")', info)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*?title, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?title, themes?*?title, demographics?*?title), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info), 'releasing', 'finished', 'on_hiatus', 'discontinued')

	local synopsis = x.XPathString('synopsisHtml', info)
	if synopsis ~= '' then
		MANGAINFO.Summary = CreateTXQuery(synopsis).XPathString('string-join(//text(), "\r\n")')
	end
	local slug = x.XPathString('slug', info)

	local chaptertype     = {nil, 'official', 'unofficial'}
	local listtype        = {'chapters', 'volumes'}
	local sel_chaptertype = (MODULE.GetOption('chaptertype') or 0) + 1
	local sel_listtype    = (MODULE.GetOption('listtype') or 0) + 1
	local optlang         = MODULE.GetOption('lang')
	local optlangid       = FindLanguage(optlang)
	local langparam       = optlangid and (sel_listtype == 1) and '?language=' .. optlangid or ''

	local deduplicate  = MODULE.GetOption('deduplicatechapters')
	local chapter_map  = {}
	local chapter_list = {}
	local has_integer  = {}
	local raw_chapters = {}

	local page = 1
	local pages = nil
	while true do
		local urlparam = (sel_listtype == 1) and '&limit=200&order=desc&page=' .. page .. '&sort=number' or ''
		local path = '/titles/' .. hid .. '/' .. listtype[sel_listtype] .. langparam .. urlparam
		local vrf = GenerateVRF(path)
		if not HTTP.GET(API_URL .. path .. (sel_listtype == 1 and '&vrf=' or '?vrf=') .. vrf) then return net_problem end

		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).items()').Get() do
			local cid    = v.GetProperty('id').ToString()
			local number = v.GetProperty('number').ToString()
			local name   = v.GetProperty('name').ToString()
			local ctype  = v.GetProperty('type').ToString()
			local lang   = v.GetProperty('language').ToString()

			if not optlangid or optlangid == lang then
				if not chaptertype[sel_chaptertype] or chaptertype[sel_chaptertype] == ctype then
					if not deduplicate then
						local chapter_name = (sel_listtype == 1) and 'Ch. ' .. number or 'Vol. ' .. number
						if name ~= '' then
							chapter_name = chapter_name .. ' - ' .. name
						end

						if not chaptertype[sel_chaptertype] and ctype == 'official' then
							chapter_name = chapter_name .. ' (Official)'
						end

						lang = not optlangid and ' [' .. lang .. ']' or ''

						MANGAINFO.ChapterLinks.Add(hid .. '/' .. slug .. '/' .. cid)
						MANGAINFO.ChapterNames.Add(chapter_name .. lang)
					else
						table.insert(raw_chapters, {
							cid = cid, number = number, name = name,
							ctype = ctype, lang = lang
						})
					end
				end
			end
		end

		if not pages then
			pages = tonumber(x.XPathString('json(*).meta.lastPage')) or 1
		end
		if page >= pages then break end
		page = page + 1
	end

	if deduplicate then
		for _, ch in ipairs(raw_chapters) do
			if not ch.number:find('%.') then
				has_integer[ch.number] = true
			end
		end

		for _, ch in ipairs(raw_chapters) do
			local base = ch.number:match('^(%d+)')
			local key = (ch.ctype ~= 'official' and base and has_integer[base]) and base or ch.number
			local current = chapter_map[key]

			if not current then
				chapter_map[key] = ch
				table.insert(chapter_list, key)
			elseif ch.ctype == 'official' and current.ctype ~= 'official' then
				chapter_map[key] = ch
			end
		end

		for _, key in ipairs(chapter_list) do
			local ch = chapter_map[key]

			local chapter_name = (sel_listtype == 1) and 'Ch. ' .. ch.number or 'Vol. ' .. ch.number
			if ch.name ~= '' then
				chapter_name = chapter_name .. ' - ' .. ch.name
			end

			if ch.ctype == 'official' then
				chapter_name = chapter_name .. ' (Official)'
			end

			local lang_suffix = not optlangid and ' [' .. ch.lang .. ']' or ''

			MANGAINFO.ChapterLinks.Add(hid .. '/' .. slug .. '/' .. ch.cid)
			MANGAINFO.ChapterNames.Add(chapter_name .. lang_suffix)
		end
	end

	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local cid = URL:match('/(%d+)$')
	local listtype = {'chapters', 'volumes'}
	local sel_listtype = (MODULE.GetOption('listtype') or 0) + 1
	local path = '/' .. listtype[sel_listtype] .. '/' .. cid
	local vrf = GenerateVRF(path)
	local u = API_URL .. path .. '?vrf=' .. vrf

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.pages().url', TASK.PageLinks)

	return true
end