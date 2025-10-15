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
		if i - 1 < prefix_len then
			out[#out + 1] = prefix_key[i] or 0
		end
		out[#out + 1] = schedule[((i - 1) % 10) + 1]((input[i] ~ init_seed_bytes[((i - 1) % 32) + 1]) & 255) & 255
	end
	return out
end

local function Add8(n) return function(c) return (c + n) & 255 end end
local function Sub8(n) return function(c) return (c - n) & 255 end end
local function Xor8(n) return function(c) return (c ~ n) & 255 end end
local function Rotl8(n) return function(c) return ((c << n) | (c >> (8 - n))) & 255 end end

local schedule_c = {
	Sub8(48), Sub8(19), Xor8(241), Sub8(19), Add8(223),
	Sub8(19), Sub8(170), Sub8(19), Sub8(48), Xor8(8),
}

local schedule_y = {
	Rotl8(4), Add8(223), Rotl8(4), Xor8(163), Sub8(48),
	Add8(82), Add8(223), Sub8(48), Xor8(83), Rotl8(4),
}

local schedule_b = {
	Sub8(19), Add8(82), Sub8(48), Sub8(170), Rotl8(4),
	Sub8(48), Sub8(170), Xor8(8), Add8(82), Xor8(163),
}

local schedule_j = {
	Add8(223), Rotl8(4), Add8(223), Xor8(83), Sub8(19),
	Add8(223), Sub8(170), Add8(223), Sub8(170), Xor8(83),
}

local schedule_e = {
	Add8(82), Xor8(83), Xor8(163), Add8(82), Sub8(170),
	Xor8(8), Xor8(241), Add8(82), Add8(176), Rotl8(4),
}

local rc4_keys = {
	L = "u8cBwTi1CM4XE3BkwG5Ble3AxWgnhKiXD9Cr279yNW0=",
	G = "t00NOJ/Fl3wZtez1xU6/YvcWDoXzjrDHJLL2r/IWgcY=",
	B = "S7I+968ZY4Fo3sLVNH/ExCNq7gjuOHjSRgSqh6SsPJc=",
	M = "7D4Q8i8dApRj6UWxXbIBEa1UqvjI+8W0UvPH9talJK8=",
	F = "0JsmfWZA1kwZeWLk5gfV5g41lwLL72wHbam5ZPfnOVE=",
}

local seeds_32 = {
	A = "pGjzSCtS4izckNAOhrY5unJnO2E1VbrU+tXRYG24vTo=",
	V = "dFcKX9Qpu7mt/AD6mb1QF4w+KqHTKmdiqp7penubAKI=",
	N = "owp1QIY/kBiRWrRn9TLN2CdZsLeejzHhfJwdiQMjg3w=",
	P = "H1XbRvXOvZAhyyPaO68vgIUgdAHn68Y6mrwkpIpEue8=",
	K = "2Nmobf/mpQ7+Dxq1/olPSDj3xV8PZkPbKaucJvVckL0=",
}

local prefix_keys = {
	O = "Rowe+rg/0g==",
	V = "8cULcnOMJVY8AA==",
	L = "n2+Og2Gth8Hh",
	P = "aRpvzH+yoA==",
	W = "ZB4oBi0=",
}

local function BytesFromBase64(b64)
	return ToBytes(require 'fmd.crypto'.DecodeBase64(b64))
end

local function GenerateVRF(input)
	local bytes = ToBytes(input)

	bytes = Rc4(BytesFromBase64(rc4_keys.L), bytes)
	bytes = Transform(bytes, BytesFromBase64(seeds_32.A), BytesFromBase64(prefix_keys.O), 7, schedule_c)
	bytes = Rc4(BytesFromBase64(rc4_keys.G), bytes)
	bytes = Transform(bytes, BytesFromBase64(seeds_32.V), BytesFromBase64(prefix_keys.V), 10, schedule_y)
	bytes = Rc4(BytesFromBase64(rc4_keys.B), bytes)
	bytes = Transform(bytes, BytesFromBase64(seeds_32.N), BytesFromBase64(prefix_keys.L), 9, schedule_b)
	bytes = Rc4(BytesFromBase64(rc4_keys.M), bytes)
	bytes = Transform(bytes, BytesFromBase64(seeds_32.P), BytesFromBase64(prefix_keys.P), 7, schedule_j)
	bytes = Rc4(BytesFromBase64(rc4_keys.F), bytes)
	bytes = Transform(bytes, BytesFromBase64(seeds_32.K), BytesFromBase64(prefix_keys.W), 5, schedule_e)

	return require 'fmd.crypto'.EncodeBase64(FromBytes(bytes)):gsub("%+", "-"):gsub("/", "_"):gsub("=+$", "")
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

	x.ParseHTML(require 'utils.json'.decode(HTTP.Document.ToString()).result.html)
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