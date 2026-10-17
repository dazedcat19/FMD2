----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.nexusscanlation.com/api/v1'
local CDN_URL = 'https://cdn.nexusscanlation.com'
local ECIES_INFO = 'nexus:lector:v1'
local ECE_KEY_INFO = 'Content-Encoding: aes128gcm\x00'
local ECE_NONCE_INFO = 'Content-Encoding: nonce\x00'
local PAGE_LIMIT = 50
local DirectoryPages = { 'manga', 'manhwa', 'manhua' }
local crypto = require 'fmd.crypto'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Mulberry32 PRNG.
local function Mulberry32(seed)
	local state = math.floor(seed) & 0xFFFFFFFF
	return function()
		state = (state + 0x6D2B79F5) & 0xFFFFFFFF
		local t = state

		t = ((t ~ (t >> 15)) * (1 | t)) & 0xFFFFFFFF

		t = t ~ (t + ((((t ~ (t >> 7)) * (61 | t)) & 0xFFFFFFFF))) & 0xFFFFFFFF

		return ((t ~ (t >> 14)) & 0xFFFFFFFF) / 4294967296.0
	end
end

-- Fisher-Yates shuffle of 0..count-1 driven by the given PRNG instance.
local function ShuffledIndices(rng, count)
	local result = {}
	for i = 0, count - 1 do
		result[i] = i
	end

	for i = count - 1, 1, -1 do
		local j = math.floor(rng() * (i + 1))
		local tmp = result[i]
		result[i] = result[j]
		result[j] = tmp
	end

	return result
end

-- HKDF-SHA256 (RFC 5869).
local function HkdfSha256(ikm, salt, info, len)
	if salt == nil or salt == '' then salt = string.rep('\0', 32) end
	local prk = crypto.HMAC_SHA256(ikm, salt)
	local okm = ''
	local prev = ''
	local counter = 1

	while #okm < len do
		prev = crypto.HMAC_SHA256(prev .. info .. string.char(counter), prk)
		okm = okm .. prev
		counter = counter + 1
	end

	return okm:sub(1, len)
end

-- Unwrap the `r` field of the chapter response with private scalar.
local function EciesUnwrap(wrapped, priv)
	local blob = crypto.DecodeBase64URL(wrapped)
	if #blob < 93 or blob:byte(1) ~= 4 then return nil end

	local shared = crypto.P256_SharedSecret(priv, blob:sub(1, 65))
	local key = HkdfSha256(shared, '', ECIES_INFO, 16)
	local plain = crypto.AESDecryptGCM(blob:sub(78), key, blob:sub(66, 77), '')
	local json = require 'utils.json'
	local ok, keys = pcall(json.decode, plain)
	if not ok then return nil end

	return keys
end

-- Decrypt an aes128gcm payload with the raw input keying material.
local function EceDecrypt(payload, ikm)
	if #payload < 22 then return nil end

	local salt = payload:sub(1, 16)
	local rs = (payload:byte(17) << 24) | (payload:byte(18) << 16) | (payload:byte(19) << 8) | payload:byte(20)
	local pos = 22 + payload:byte(21)
	if rs < 18 or pos > #payload then return nil end

	local key = HkdfSha256(ikm, salt, ECE_KEY_INFO, 16)
	local nonce = HkdfSha256(ikm, salt, ECE_NONCE_INFO, 12)
	local n1, n2, n3, n4 = nonce:byte(9), nonce:byte(10), nonce:byte(11), nonce:byte(12)
	local out = {}
	local seq = 0

	while pos <= #payload do
		local chunk = payload:sub(pos, pos + rs - 1)
		pos = pos + #chunk
		if #chunk < 18 then return nil end
		local iv = nonce:sub(1, 8) .. string.char(
			n1 ~ ((seq >> 24) & 0xFF),
			n2 ~ ((seq >> 16) & 0xFF),
			n3 ~ ((seq >> 8) & 0xFF),
			n4 ~ (seq & 0xFF))
		local plain = crypto.AESDecryptGCM(chunk, key, iv, '')
		if plain == '' then return nil end

		local last = #plain
		while last >= 1 and plain:byte(last) == 0 do
			last = last - 1
		end
		if last < 1 then return nil end

		local is_final = pos > #payload
		if plain:byte(last) ~= (is_final and 2 or 1) then return nil end
		out[#out + 1] = plain:sub(1, last - 1)
		seq = seq + 1
	end

	return table.concat(out)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. '/catalog?tipo=' .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. '&orden=nuevo&page=' .. (URL + 1) .. '&limit=' .. PAGE_LIMIT

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('titulo').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = math.ceil(x.XPathString('json(*).meta.total') / PAGE_LIMIT) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('/([^/]+)$')
	local u = API_URL .. '/series/' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local serie = x.XPath('json(*).serie')
	MANGAINFO.Title     = x.XPathString('titulo', serie)
	MANGAINFO.AltTitles = x.XPathString('string-join(titulos_alt?*, ", ")', serie)
	MANGAINFO.CoverLink = x.XPathString('portada_url', serie)
	MANGAINFO.Authors   = x.XPathString('string-join(autores?*?nombre, ", ")', serie)
	MANGAINFO.Genres    = x.XPathString('string-join(generos?*?nombre, ", ")', serie)
	MANGAINFO.Summary   = x.XPathString('descripcion', serie)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('estado', serie), 'en_emision', 'finalizado', 'pausado')

	for v in x.XPath('json(*).capitulos()').Get() do
		if v.GetProperty('es_premium').ToString() ~= 'true' then
			MANGAINFO.ChapterLinks.Add(slug .. '/' .. v.GetProperty('slug').ToString())
			MANGAINFO.ChapterNames.Add('Capítulo ' .. v.GetProperty('numero').ToString())
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local rev = tonumber(require 'fmd.env'.Revision)
	if rev < 6948 then
		print('Require FMD2 revision 6948 and later')
		print('Revision ' .. rev .. ' detected')
		return false
	end
	local mangaslug, chapslug = URL:match('^/([^/]+)/([^/]+)$')
	local u = API_URL .. '/series/' .. mangaslug .. '/capitulos/' .. chapslug
	local priv, pub = crypto.P256_GenerateKeyPair()

	HTTP.Reset()
	HTTP.Headers.Values['X-Rs'] = crypto.EncodeBase64URL(pub)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local wrapped = x.XPathString('json(*).data.r')
	local keys = EciesUnwrap(wrapped, priv)
	for v in x.XPath('json(*).data.paginas()').Get() do
		local img_url = v.GetProperty('url').ToString()
		local orden = v.GetProperty('orden').ToString()
		local sc = v.GetProperty('sc')
		local cols = sc.GetProperty('c').ToString()
		local rows = sc.GetProperty('r').ToString()
		local ver = sc.GetProperty('v').ToString()
		local page_key = keys.k[orden] or nil
		local seed = keys.s[orden] or nil
		img_url = img_url
			.. '#seed=' .. (seed or '0')
			.. '&cols=' .. cols
			.. '&rows=' .. rows
			.. '&v=' .. ver
			.. '&key=' .. (page_key or '')
		TASK.PageLinks.Add(img_url)
	end
	return true
end

-- Download and decrypt and/or descramble image given the image URL.
function DownloadImage()
	if not HTTP.GET(URL) then return false end

	local fragment = URL:match('[^#]+#(.+)$')
	if fragment then
		local page_key = fragment:match('key=([^&]*)')
		local cols = tonumber(fragment:match('cols=(%d+)'))
		local rows = tonumber(fragment:match('rows=(%d+)'))
		if page_key ~= '' then
			local plain = EceDecrypt(HTTP.Document.ToString(), crypto.DecodeBase64URL(page_key))
			HTTP.Document.WriteString(plain)
		end
		if cols and rows then
			local seed = tonumber(fragment:match('seed=(%d+)'))
			local version = tonumber(fragment:match('v=(%d+)')) or 1
			local count = cols * rows
			local rng = Mulberry32(seed)
			local permutation = ShuffledIndices(rng, count)
			local puzzle = require 'fmd.imagepuzzle'.Create(cols, rows)

			for i = 0, count - 1 do
				puzzle.Matrix[i] = permutation[i]
			end

			if version >= 2 then
				for i = 0, count - 1 do
					puzzle.Flips[i] = math.floor(rng() * 4)
				end
			end
			puzzle.DeScramble(HTTP.Document, HTTP.Document)
		end
	end
	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'a1f3c7d9e0b64a2c8d5f1e93b7a4c6d2'
	m.Name                     = 'Nexus Scanlation'
	m.RootURL                  = 'https://nexusscanlation.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.TotalDirectory           = #DirectoryPages
	m.SortedList               = true
end
