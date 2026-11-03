----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local LOGIN_URL       = '/api/auth/login'
local DirectoryPages  = '/api/manga-custom?limit=1000&type=manga,manhwa,manhua&order=newest&page='

-- Page url encryption (ls seed + lh decoy list + ph token).
local TOKEN_ALPHABET  = 'tgEib4lqMSha5TU13QX28deN-xuk6KywP0m_roGYvBWAnzFfjcJIVROp7CsDZ9HL'
local B64_ALPHABET    = 'CDoIL78Qqwj9zKM6mOFdf0niV_AByGg4aNhbEetPsHr1x-TpZJYRk5lWu32vUcSX'
local SEED_MATRIX_V1  = {
	{ 3, 7, 1, 10 }, { 5, 2, 9, 11 }, { 8, 4, 11, 1 }, { 2, 10, 6, 3 },
	{ 9, 1, 5, 7 }, { 11, 6, 3, 8 }, { 4, 9, 2, 6 }, { 7, 11, 8, 5 } }
local SEED_MASK_V1    = { 93, 162, 25, 231 }
local SEED_MATRIX_V2  = {
	{ 12, 2, 13, 7, 1, 9, 8 }, { 11, 4, 5, 6, 3, 9, 12 }, { 9, 8, 2, 3, 1, 6, 13 }, { 8, 7, 2, 13, 1, 3, 6 },
	{ 13, 2, 11, 6, 4, 3, 8 }, { 6, 1, 3, 4, 7, 5, 11 }, { 4, 8, 7, 9, 1, 10, 13 }, { 8, 3, 6, 13, 12, 10, 11 } }
local SEED_MASK_V2    = { 234, 55, 192, 73, 249, 251, 2, 58 }
local SEED_KEY_NAME   = 1000283751
local SEED_KEY_SLICE  = 739982445
local LH_SEED_POS     = { 3, 8, 12, 17 }
local LhShuffleKey    = 1374496675
local LhStreamKey     = 795581489

local TOKEN_MAP, TOKEN_CHARS, B64_MAP = {}, {}, {}
for i = 1, 64 do
	TOKEN_MAP[TOKEN_ALPHABET:byte(i)] = i - 1
	TOKEN_CHARS[i] = TOKEN_ALPHABET:sub(i, i)
	B64_MAP[B64_ALPHABET:byte(i)] = i - 1
end

----------------------------------------------------------------------------------------------------
-- Local Functions
----------------------------------------------------------------------------------------------------

-- 32-bit multiply of two unsigned integers.
local function IMul32(a, b)
	return ((a & 0xFFFFFFFF) * (b & 0xFFFFFFFF)) & 0xFFFFFFFF
end

-- Murmur style 32-bit bit mixer.
local function Mix32(x)
	x = x & 0xFFFFFFFF
	x = IMul32(x ~ (x >> 16), 2146121005)
	x = IMul32(x ~ (x >> 15), 2221713035)
	return x ~ (x >> 16)
end

-- FNV-1a hash of a string.
local function HashString(s)
	local h = 2166136261
	for i = 1, #s do
		h = ((h ~ s:byte(i)) * 16777619) & 0xFFFFFFFF
	end
	return h
end

-- FNV-1a hash of a byte table.
local function HashBytes(t, from, to)
	local h = 2166136261
	for i = from, to do
		h = ((h ~ t[i]) * 16777619) & 0xFFFFFFFF
	end
	return h
end

-- Byte table to string.
local function BytesToString(t, from, to)
	local s = {}
	for i = from, to do s[#s + 1] = string.char(t[i]) end
	return table.concat(s)
end

-- Mulberry32 PRNG with 32-bit wrapping arithmetic returning the full 32 bit word.
local function Mulberry32Word(seed)
	local state = seed & 0xFFFFFFFF
	return function()
		state = (state + 0x6D2B79F5) & 0xFFFFFFFF
		local t = state
		t = ((t ~ (t >> 15)) * (t | 1)) & 0xFFFFFFFF
		t = t ~ ((t + (((t ~ (t >> 7)) * (t | 61)) & 0xFFFFFFFF)) & 0xFFFFFFFF)
		return t ~ (t >> 14)
	end
end

-- Custom alphabet Base64 -> byte table.
local function DecodeTokenBase64(s)
	local out, n, bits = {}, 0, 0
	for i = 1, #s do
		local v = B64_MAP[s:byte(i)]
		if not v then return nil end
		n = (n << 6) | v
		bits = bits + 6
		if bits >= 8 then
			bits = bits - 8
			out[#out + 1] = (n >> bits) & 0xFF
		end
		n = n & ((1 << bits) - 1)
	end
	return out
end

-- Undo the alphabet shift, rotation and half interleave of a ph token.
local function ScrambleToken(s, index)
	if #s < 2 then return s end
	local out = {}
	for r = 0, #s - 1 do
		local v = TOKEN_MAP[s:byte(r + 1)]
		if not v then return s end
		out[#out + 1] = TOKEN_CHARS[((v - r * 7 - 13 - index) & 63) + 1]
	end
	s = table.concat(out)
	local rot = (#s * 5 + index * 3 + 1) % #s
	s = s:sub(#s - rot + 1) .. s:sub(1, #s - rot)
	local half = (#s + 1) // 2
	local a, b = s:sub(1, half), s:sub(half + 1):reverse()
	local mix = {}
	for k = 0, #s - 1 do
		if (k & 1) == 0 then
			mix[#mix + 1] = a:sub(k // 2 + 1, k // 2 + 1)
		else
			mix[#mix + 1] = b:sub((k - 1) // 2 + 1, (k - 1) // 2 + 1)
		end
	end
	return table.concat(mix)
end

-- Unsigned 32 bit integer to base 36 string.
local function ToBase36(x)
	if x == 0 then return '0' end
	local digits = '0123456789abcdefghijklmnopqrstuvwxyz'
	local out = {}
	while x > 0 do
		out[#out + 1] = digits:sub(x % 36 + 1, x % 36 + 1)
		x = x // 36
	end
	return table.concat(out):reverse()
end

-- Name of the html variable holding the slice of the page link key.
local function KeyVariableName(e)
	return '_' .. ToBase36(Mix32((e ~ SEED_KEY_NAME) & 0xFFFFFFFF))
end

-- Recover the page link key from the key variables of a html page cached.
local function FindHtmlKey(p)
	for n = p - 1, p + 1 do
		local hex = MODULE.Storage[KeyVariableName(n)]
		if hex ~= '' then
			local i = p - n + 1
			local slice = tonumber(hex:sub(i * 8 + 1, i * 8 + 8), 16)
			if slice then
				return (slice ~ Mix32((n * 3 + i) ~ SEED_KEY_SLICE)) & 0xFFFFFFFF
			end
		end
	end
	return nil
end

-- Decrypt a ph token with the seed of the 12 byte ls variant (js fallback decoder).
local function DecodeTokenV1(token, seed, index, id)
	local data = DecodeTokenBase64(token)
	if not data or #data < 3 then return nil end

	local i = Mix32((seed ~ 2704714413) & 0xFFFFFFFF)
	local a = Mix32((i ~ IMul32(index + 1, 2654435761)) & 0xFFFFFFFF)
	local o = Mix32((a ~ 2246822519 ~ HashString(id)) & 0xFFFFFFFF)
	local s = Mix32((o ~ 668265263) & 0xFFFFFFFF)

	local function next_word()
		local e = (i + a + s) & 0xFFFFFFFF
		s = (s + 1) & 0xFFFFFFFF
		i = (a ~ (a >> 9)) & 0xFFFFFFFF
		a = (o + ((o << 3) & 0xFFFFFFFF)) & 0xFFFFFFFF
		o = ((((o << 21) & 0xFFFFFFFF) | (o >> 11)) + e) & 0xFFFFFFFF
		return e
	end
	for _ = 1, 12 do next_word() end

	local word, used = 0, 4
	local function next_byte()
		if used == 4 then word, used = next_word(), 0 end
		local b = (word >> (used * 8)) & 0xFF
		used = used + 1
		return b
	end

	local plain = {}
	for k = 1, #data do plain[k] = (data[k] ~ next_byte()) & 0xFF end

	if plain[1] ~= 3 then return nil end
	if (HashBytes(plain, 4, #plain) & 0xFFFF) ~= ((plain[2] << 8) | plain[3]) then return nil end

	return BytesToString(plain, 4, #plain)
end

-- Decrypt a ph token with the seed of the 14 byte ls variant (wasm decoder fallback).
local function DecodeTokenV2(token, seed, index, id)
	local data = DecodeTokenBase64(token)
	if not data or #data < 3 or #data > 4096 then return nil end

	local a = Mix32((seed ~ 1821285621) & 0xFFFFFFFF)
	local o = Mix32((a ~ IMul32(index + 1, 2654435761)) & 0xFFFFFFFF)
	local s = Mix32((o ~ 2246822519 ~ HashString(id)) & 0xFFFFFFFF) -- field 0 = image url
	local c = Mix32((s ~ 374761393) & 0xFFFFFFFF)

	local function next_word()
		local e = (a + o + c) & 0xFFFFFFFF
		c = (c + 1) & 0xFFFFFFFF
		a = (o ~ (o >> 9)) & 0xFFFFFFFF
		o = (s + ((s << 3) & 0xFFFFFFFF)) & 0xFFFFFFFF
		s = ((((s << 21) & 0xFFFFFFFF) | (s >> 11)) + e) & 0xFFFFFFFF
		return e
	end
	for _ = 1, 16 do next_word() end

	local word = 0
	for t = 0, #data - 1 do
		if t % 4 == 0 then word = next_word() end
		data[t + 1] = (data[t + 1] ~ ((word >> ((t % 4) * 8)) & 0xFF)) & 0xFF
	end

	if data[1] ~= 4 then return nil end
	if (HashBytes(data, 4, #data) & 0xFFFF) ~= ((data[2] << 8) | data[3]) then return nil end

	return BytesToString(data, 4, #data)
end

-- Decode the ls seed variant with a 12 byte token.
local function DecodeSeedV1(ls)
	local t = DecodeTokenBase64(ls)
	if not t or #t ~= 12 then return nil end

	local row = SEED_MATRIX_V1[(t[1] & 7) + 1]
	local b = {}
	for e = 0, 3 do
		b[#b + 1] = (t[row[e + 1] + 1] ~ SEED_MASK_V1[((e + t[1]) & 3) + 1] ~ t[1]) & 0xFF
	end

	return (b[1] << 24) | (b[2] << 16) | (b[3] << 8) | b[4]
end

-- Decode the ls seed variant with a 14 byte token, the seed still has to be
-- combined with the key of the html page of the chapter.
local function DecodeSeedV2(ls)
	local t = DecodeTokenBase64(ls)
	if not t or #t ~= 14 then return nil end

	local row = SEED_MATRIX_V2[(t[1] & 7) + 1]
	local n = {}
	for e = 0, 6 do
		n[#n + 1] = (t[row[e + 1] + 1] ~ SEED_MASK_V2[((e + t[1]) & 7) + 1] ~ t[1]) & 0xFF
	end

	local p = (n[1] << 16) | (n[2] << 8) | n[3]
	local v = (n[4] << 24) | (n[5] << 16) | (n[6] << 8) | n[7]

	return p, v
end

-- Decode the lh token to the list of zero based indexes of the decoy pages.
local function DecodeLH(lh)
	if #lh ~= 24 then return nil end
	local raw = require 'fmd.crypto'.DecodeBase64(lh)
	if #raw ~= 18 then return nil end

	local seed, rest = 0, {}
	for i = 1, 18 do
		if i == LH_SEED_POS[1] or i == LH_SEED_POS[2] or i == LH_SEED_POS[3] or i == LH_SEED_POS[4] then
			seed = ((seed << 8) | raw:byte(i)) & 0xFFFFFFFF
		else
			rest[#rest + 1] = raw:byte(i)
		end
	end

	-- undo the payload shuffle
	local rand = Mulberry32Word((seed ~ LhShuffleKey) & 0xFFFFFFFF)
	local perm = {}
	for i = 0, 13 do perm[i + 1] = i end
	for i = 13, 1, -1 do
		local j = ((rand() * (i + 1)) >> 32) + 1
		perm[i + 1], perm[j] = perm[j], perm[i + 1]
	end

	local rand2 = Mulberry32Word((seed ~ LhStreamKey) & 0xFFFFFFFF)
	local iv = {}
	for k = 0, 13 do
		iv[#iv + 1] = (rest[perm[k + 1] + 1] ~ (rand2() >> 24)) & 0xFF
	end

	local checksum = 0
	for k = 0, 12 do
		checksum = (checksum + iv[k + 1] * (k + 1)) & 0xFF
	end
	if (checksum ~ 165) ~= iv[14] then return nil end

	local count = iv[1]
	if count < 1 or count > 5 then return nil end

	local drop = {}
	for j = 0, count - 1 do
		drop[#drop + 1] = (iv[2 + j * 2] << 8) | iv[3 + j * 2]
	end

	return drop
end

-- Decrypt the page links of a chapter pages response and add them to the task.
local function ProcessPages(x)
	local ls = x.XPathString('json(*).ls')
	if ls == 'null' then ls = '' end

	-- select the encryption mode of the response
	local mode, seed = 'plain'
	if ls ~= '' then
		seed = DecodeSeedV1(ls)
		if seed then
			mode = 'v1'
		else
			local p, v = DecodeSeedV2(ls)
			if not p then return nil end
			local key = FindHtmlKey(p)
			if not key then return 'retry' end
			seed = (v ~ key) & 0xFFFFFFFF
			mode = 'v2'
		end
	end

	-- decrypt every page token, the token index is zero based
	local links = {}
	local i, tokens, decoded = 0, 0, 0
	for v in x.XPath('json(*).data()').Get() do
		local url = v.GetProperty('imageUrl').ToString()
		if mode ~= 'plain' then
			local token = v.GetProperty('ph').ToString()
			if token ~= '' then
				tokens = tokens + 1
				local id = v.GetProperty('id').ToString():gsub('%.0$', '')
				local decrypted
				if mode == 'v1' then
					decrypted = DecodeTokenV1(ScrambleToken(token, i), seed, i, id)
				else
					decrypted = DecodeTokenV2(ScrambleToken(token, i), seed, i, id)
				end
				if decrypted then
					decoded = decoded + 1
					url = decrypted
				end
			end
		end
		i = i + 1
		links[i] = url
	end
	if i == 0 then return nil end
	if tokens > 0 and decoded == 0 then
		return mode == 'v2' and 'retry' or nil
	end

	-- remove the decoy pages hidden in the response
	local lh = x.XPathString('json(*).lh')
	if lh ~= '' and lh ~= 'null' then
		local drop = DecodeLH(lh)
		if drop and #drop > 0 then
			local valid = true
			for _, d in ipairs(drop) do
				if d <= 0 or d >= #links then valid = false; break end
			end
			if valid then
				local dropped = {}
				for _, d in ipairs(drop) do dropped[d + 1] = true end
				local kept = {}
				for j = 1, #links do
					if not dropped[j] then kept[#kept + 1] = links[j] end
				end
				links = kept
			end
		end
	end

	for j = 1, #links do
		if links[j] ~= '' then TASK.PageLinks.Add(links[j]) end
	end

	return 'ok'
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Sign in to the current website.
function Login()
	local crypto = require 'fmd.crypto'
	local u = MODULE.RootURL .. LOGIN_URL

	if not MODULE.Account.Enabled then return false end

	local s = 'email=' .. crypto.EncodeURLElement(MODULE.Account.Username) ..
		'&password=' .. crypto.EncodeURLElement(MODULE.Account.Password)
	MODULE.Account.Status = asChecking

	if HTTP.POST(u, s) then
		if HTTP.ResultCode == 200 then
			MODULE.Account.Cookies = CreateTXQuery(HTTP.Document).XPathString('json(*).data.token')
			MODULE.Account.Status = asValid
			return true
		else
			MODULE.Account.Status = asInvalid
			return false
		end
	else
		MODULE.Account.Status = asUnknown
		return false
	end
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPages .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*).data.maxPage')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPages .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).data.items()').Get() do
		LINKS.Add(v.GetProperty('organization').GetProperty('slug').ToString() .. '/manga/' .. v.GetProperty('manga').GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local site, slug = URL:match('/([^/]+)/manga/([^/]+)')
	local endpoint = URL:find('/joint/', 1, true) and 'joint' or 'manga-custom'
	::endpoint::
	local u = MODULE.RootURL .. '/api/' .. endpoint .. '/' .. slug

	HTTP.Headers.Values['X-Organization'] = site

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('parse-json(.)?data')
	local joint_slug = x.XPathString('jointSlug', info)
	if joint_slug ~= '' then
		endpoint = 'joint'
		site = 'joint'
		slug = joint_slug
		MANGAINFO.Link = '/' .. endpoint .. '/manga/' .. slug
		MANGAINFO.URL  = MODULE.RootURL .. '/' .. endpoint .. '/manga/' .. slug
		goto endpoint
	end

	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('alternativeTitle', info)
	MANGAINFO.CoverLink = x.XPathString('imageUrl', info)
	MANGAINFO.Authors   = x.XPathString('string-join(manga?authors?*?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, manga?demography?name, manga?bookType?name), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('replace(status, "finished", "completed")', info))
	MANGAINFO.Summary   = x.XPathString('description', info)

	for v in x.XPath('chapters?*[not(isUnreleased=true)]', info).Get() do
		local number = v.GetProperty('number').ToString()
		local title = v.GetProperty('title').ToString()

		title = (title ~= '') and not title:find('Capítulo ' .. number, 1, true) and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(site .. '/' .. slug .. '/' .. number)
		MANGAINFO.ChapterNames.Add('Capítulo ' .. number .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	if MODULE.Account.Enabled and MODULE.Account.Cookies == '' then Login() end
	local is_joint = URL:find('/joint/', 1, true)
	local site, slug, cid = URL:match('^/([^/]+)/([^/]+)/([^/]+)$')
	if not site then return false end

	local endpoint    = is_joint and 'joint' or 'manga-custom'
	local api_url     = MODULE.RootURL .. '/api/' .. endpoint .. '/' .. slug .. '/chapter/' .. cid .. '/pages'
	local chapter_url = MODULE.RootURL .. '/' .. site .. '/manga/' .. slug .. '/chapters/' .. cid

	for attempt = 1, 3 do
		if MODULE.Storage.Text == '' then
			HTTP.Reset()
			if HTTP.GET(chapter_url) then
				for name, hex in HTTP.Document.ToString():gmatch('([%w_$]+)%s*=%s*"([0-9a-f]+)"') do
					if #hex == 24 then MODULE.Storage[name] = hex end
				end
			end
		end

		HTTP.Reset()
		HTTP.Headers.Values['X-Organization'] = site
		HTTP.Headers.Values['Authorization'] = 'Bearer ' .. MODULE.Account.Cookies

		if not HTTP.GET(api_url) then return false end

		local x = CreateTXQuery(HTTP.Document)
		if x.XPathString('json(*).errorType') == 'login_required' then
			if not MODULE.Account.Enabled or attempt == 3 then return false end
			MODULE.Account.Cookies = ''
			if not Login() then return false end
		else
			local status = ProcessPages(x)
			if status == 'ok' then return true end
			if status ~= 'retry' then return false end
			MODULE.Storage.Text = ''
		end
	end

	return false
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '97dcd409f781467eb2da4a8634e5a1d9'
	m.Name                     = 'CapibaraTraductor'
	m.RootURL                  = 'https://capibaratraductor.com'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnLogin                  = 'Login'
	m.AccountSupport           = true
	m.SortedList               = true
end
