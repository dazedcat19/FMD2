----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '23eb3a472201427e8824ecdd5223bad7'
	m.Name                     = 'MangaFire'
	m.RootURL                  = 'https://mangafire.to'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['lang'] = 'Language:',
			['listtype'] = 'List type:',
			['type'] = 'Chapter\nVolume'
		},
		['id_ID'] = {
			['lang'] = 'Bahasa:',
			['listtype'] = 'Tipe daftar:',
			['type'] = 'Bab\nJilid'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}

	local items = 'None'
	local t = GetLangList()
	for k, v in ipairs(t) do items = items .. '\r\n' .. v end
	m.AddOptionComboBox('lang', lang:get('lang'), items, 1)
	m.AddOptionComboBox('listtype', lang:get('listtype'), lang:get('type'), 0)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/newest?page='

local Langs = {
	["en"] = "English",
	["fr"] = "French",
	["ja"] = "Japanese",
	["pt-br"] = "Portuguese (Br)",
	["pt"] = "Portuguese (Pt)",
	["es-la"] = "Spanish (LATAM)",
	["es"] = "Spanish (Es)"
}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function ToBytes(s)
	local t = {}
	for i = 1, #s do
		t[#t + 1] = string.byte(s, i) & 255
	end
	return t
end

local function FromBytes(t)
	return string.char(table.unpack(t))
end

local function Rc4(key, input)
	local s = {}
	for i = 0, 255 do
		s[i] = i
	end
	local j = 0
	for i = 0, 255 do
		j = (j + s[i] + key[(i % #key) + 1]) & 255
		s[i], s[j] = s[j], s[i]
	end
	local out = {}
	local i = 0
	j = 0
	for y = 1, #input do
		i = (i + 1) & 255
		j = (j + s[i]) & 255
		s[i], s[j] = s[j], s[i]
		local k = s[(s[i] + s[j]) & 255]
		out[y] = (input[y] ~ k) & 255
	end
	return out
end

local function Transform(input, init_seed_bytes, prefix_key, prefix_len, schedule)
	local out = {}
	for i = 1, #input do
		if (i - 1) < prefix_len then
			out[#out + 1] = prefix_key[i] or 0
		end
		out[#out + 1] = schedule[((i - 1) % 10) + 1]((input[i] ~ init_seed_bytes[((i - 1) % 32) + 1]) & 255) & 255
	end
	return out
end

local function Add8(n) return function(c) return (c + n) & 255 end end
local function Sub8(n) return function(c) return (c - n + 256) & 255 end end
local function Xor8(n) return function(c) return (c ~ n) & 255 end end
local function Rotl8(n) return function(c) return ((c << n) | (c >> (8 - n))) & 255 end end
local function Rotr8(n) return function(c) return ((c >> n) | (c << (8 - n))) & 255 end end

local schedule_c = {
	Sub8(223), Rotr8(4), Rotr8(4), Add8(234), Rotr8(7),
	Rotr8(2), Rotr8(7), Sub8(223), Rotr8(7), Rotr8(6),
}

local schedule_y = {
	Add8(19), Rotr8(7), Add8(19), Rotr8(6), Add8(19),
	Rotr8(1), Add8(19), Rotr8(6), Rotr8(7), Rotr8(4),
}

local schedule_b = {
	Sub8(223), Rotr8(1), Add8(19), Sub8(223), Rotl8(2),
	Sub8(223), Add8(19), Rotl8(1), Rotl8(2), Rotl8(1),
}

local schedule_j = {
	Add8(19), Rotl8(1), Rotl8(1), Rotr8(1), Add8(234),
	Rotl8(1), Sub8(223), Rotl8(6), Rotl8(4), Rotl8(1),
}

local schedule_e = {
	Rotr8(1), Rotl8(1), Rotl8(6), Rotr8(1), Rotl8(2),
	Rotr8(4), Rotl8(1), Rotl8(1), Sub8(223), Rotl8(2),
}

local rc4_keys = {
	l = "FgxyJUQDPUGSzwbAq/ToWn4/e8jYzvabE+dLMb1XU1o=",
	g = "CQx3CLwswJAnM1VxOqX+y+f3eUns03ulxv8Z+0gUyik=",
	B = "fAS+otFLkKsKAJzu3yU+rGOlbbFVq+u+LaS6+s1eCJs=",
	m = "Oy45fQVK9kq9019+VysXVlz1F9S1YwYKgXyzGlZrijo=",
	F = "aoDIdXezm2l3HrcnQdkPJTDT8+W6mcl2/02ewBHfPzg=",
}

local seeds_32 = {
	A = "yH6MXnMEcDVWO/9a6P9W92BAh1eRLVFxFlWTHUqQ474=",
	V = "RK7y4dZ0azs9Uqz+bbFB46Bx2K9EHg74ndxknY9uknA=",
	N = "rqr9HeTQOg8TlFiIGZpJaxcvAaKHwMwrkqojJCpcvoc=",
	P = "/4GPpmZXYpn5RpkP7FC/dt8SXz7W30nUZTe8wb+3xmU=",
	k = "wsSGSBXKWA9q1oDJpjtJddVxH+evCfL5SO9HZnUDFU8=",
}

local prefix_keys = {
	O = "l9PavRg=",
	v = "Ml2v7ag1Jg==",
	L = "i/Va0UxrbMo=",
	p = "WFjKAHGEkQM=",
	W = "5Rr27rWd",
}

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
			c = c + (x:sub(i ,i) == '1' and 2 ^ (6 - i) or 0)
		end
		return b:sub(c + 1, c +1)
	end) .. ({ '', '==', '=' })[#data % 3 + 1])
end

local function GenerateVRF(input)
	local crypto = require 'fmd.crypto'
	local bytes = ToBytes(crypto.EncodeURLElement(input))

	local stages = {
		{ rc4_keys.l, seeds_32.A, prefix_keys.O, schedule_c },
		{ rc4_keys.g, seeds_32.V, prefix_keys.v, schedule_y },
		{ rc4_keys.B, seeds_32.N, prefix_keys.L, schedule_b },
		{ rc4_keys.m, seeds_32.P, prefix_keys.p, schedule_j },
		{ rc4_keys.F, seeds_32.k, prefix_keys.W, schedule_e },
	}

	for _, st in ipairs(stages) do
		local key_b64, seed_b64, prefix_b64, sched = st[1], st[2], st[3], st[4]

		local rc4_key = ToBytes(crypto.DecodeBase64(key_b64))
		bytes = Rc4(rc4_key, bytes)

		local seed_bytes = ToBytes(crypto.DecodeBase64(seed_b64))
		local prefix_bytes_str = crypto.DecodeBase64(prefix_b64)
		local prefix_bytes = ToBytes(prefix_bytes_str)
		local prefix_len = #prefix_bytes_str

		bytes = Transform(bytes, seed_bytes, prefix_bytes, prefix_len, sched)
	end

	return EncodeBase64(FromBytes(bytes)):gsub("%+", "-"):gsub("/", "_"):gsub("=+$", "")
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

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()]/a/@href/substring-after(., "=")')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="info"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="manga-detail"]//h6')
	MANGAINFO.CoverLink = x.XPathString('(//div[@class="poster"])[1]//img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="meta"]/div[./span="Author:"]/span/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="meta"]/div[./span="Genres:"]/span/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="info"]/p'), 'Releasing', 'Completed', 'On_hiatus', 'Discontinued')
	MANGAINFO.Summary   = x.XPathString('string-join(//div[@class="modal-content p-4"]/text(), "\r\n")')

	local listtype     = {'chapter', 'volume'}
	local sel_listtype = (MODULE.GetOption('listtype') or 0) + 1
	local optlang      = MODULE.GetOption('lang')
	local optlangid    = FindLanguage(optlang)

	if optlangid == nil then langparam = '' else langparam = optlangid end
	local vrf = GenerateVRF(URL:match('%.(.-)$') .. '@' .. listtype[sel_listtype] .. '@' .. langparam)

	if not HTTP.GET(MODULE.RootURL .. '/ajax/read/' .. URL:match('%.(.-)$') .. '/' .. listtype[sel_listtype] .. '/' .. langparam .. '?vrf=' .. vrf) then return net_problem end

	local s = HTTP.Document.ToString()
	if s:sub(1, 1) == '<' then print('VRF value mismatch') return no_error end
	x.ParseHTML(require 'utils.json'.decode(s).result.html)
	for v in x.XPath('//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('data-id'))
		MANGAINFO.ChapterNames.Add(x.XPathString('text()', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local listtype     = {'chapter', 'volume'}
	local sel_listtype = (MODULE.GetOption('listtype') or 0) + 1
	local vrf = GenerateVRF(listtype[sel_listtype] .. '@' .. URL:match('/(.-)$'))

	local u = MODULE.RootURL .. '/ajax/read/' .. listtype[sel_listtype] .. URL .. '?vrf=' .. vrf

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).result.images()(1)', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end