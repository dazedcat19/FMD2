----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local DirectoryPages = '/mangas_'
local SORA_MANIFEST_KEY = '/fuCkYou!!!'
local SORA_IMAGE_KEY = '202508055d0db38bae2e86cc41649f90'
local SORA_CANVA2_SALT = '6a0248ad1ca4208275aed64d336e81595ecb149422a8e621f70e23b9f01b9c1c'
local SORA_TARGET, SORA_SEG_MAX, SORA_SEG_MIN = 222, 259, 185
local crypto = require 'fmd.crypto'
local imagepuzzle = require 'fmd.imagepuzzle'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local WIDTH, CHUNKS, DIGITS, MASK = 256, 6, 52, 255

local function SoraArc4G(self, count)
	local r = 0
	for _ = 1, count do
		self.i = (self.i + 1) & MASK
		local t = self.s[self.i]
		self.j = (self.j + t) & MASK
		local sj = self.s[self.j]
		self.s[self.i] = sj
		self.s[self.j] = t
		r = r * WIDTH + self.s[(sj + t) & MASK]
	end
	return r
end

function SeedRandom_new(seed)
	local self = {}
	self.startdenom = WIDTH ^ CHUNKS
	self.significance = 2.0 ^ DIGITS
	self.overflow = self.significance * 2

	local key = {}
	do
		local k = {}
		for i = 0, MASK do k[i] = 0 end
		local smear = 0
		local n = math.min(#seed, WIDTH)
		for j = 1, n do
			smear = smear ~ (k[(j - 1) & MASK] * 19)
			k[(j - 1) & MASK] = (smear + seed:byte(j)) & MASK
		end
		for i = 0, n - 1 do key[i] = k[i] end
		self.key, self.keyLen = key, math.max(n, 1)
		if n == 0 then key[0] = 0 end
	end

	self.s = {}
	for i = 0, MASK do self.s[i] = i end
	self.i, self.j = 0, 0
	local j = 0
	for k = 0, MASK do
		local t = self.s[k]
		j = (j + key[k % self.keyLen] + t) & MASK
		self.s[k] = self.s[j]
		self.s[j] = t
	end
	SoraArc4G(self, WIDTH)

	function self:next_double()
		local n = SoraArc4G(self, CHUNKS) + 0.0
		local d = self.startdenom
		local x = 0
		while n < self.significance do
			n = (n + x) * WIDTH
			d = d * WIDTH
			x = SoraArc4G(self, 1)
		end
		while n >= self.overflow do
			n = n / 2
			d = d / 2
			x = x >> 1
		end
		return (n + x) / d
	end
	return self
end

-- Fisher-Yates over a 1-based list
local function SoraShuffle(list, rng)
	for i = #list - 1, 1, -1 do
		local j = math.floor(rng:next_double() * (i + 1))
		list[i + 1], list[j + 1] = list[j + 1], list[i + 1]
	end
end

local function SoraSplit(total, rng)
	local count = math.max(1, math.floor(total / SORA_TARGET + 0.5))
	while math.floor(total / count) > SORA_SEG_MAX do count = count + 1 end
	while math.floor(total / count) < SORA_SEG_MIN and count > 1 do count = count - 1 end
	local base = math.floor(total / count)
	local rem = total - base * count
	local sizes = {}
	for i = 1, count do
		sizes[i] = (i <= rem) and (base + 1) or base
	end
	SoraShuffle(sizes, rng)
	return sizes
end

----------------------------------------------------------------------------------------------------
-- canva mode: uneven grid + per-tile rotation/flip arrangement
----------------------------------------------------------------------------------------------------

local function SoraTransform(rng, swap, square)
	local rot
	if swap then
		rot = rng:next_double() < 0.5 and 1 or 3
	elseif square then
		rot = math.floor(rng:next_double() * 4)
	else
		rot = rng:next_double() < 0.5 and 0 or 2
	end
	return rot, rng:next_double() < 0.5, rng:next_double() < 0.5
end

local function SoraRestMoves(rest, perm_seed, tf_seed, square, moves)
	if #rest == 0 then return end
	local order = {}
	for _, idx in ipairs(rest) do order[#order + 1] = idx end
	SoraShuffle(order, SeedRandom_new(perm_seed))
	for i = 1, #rest do
		moves[rest[i]] = {order[i], SoraTransform(SeedRandom_new(tf_seed .. '|' .. (i - 1)), false, square)}
	end
end

function SoraCanvaConfig(key, width, height)
	local rng = SeedRandom_new(key)
	local cols = SoraSplit(width, rng)
	local rows = SoraSplit(height, rng)

	-- tiles (1-based): {x, y, w, h}
	local tiles = {}
	local y = 0
	for _, h in ipairs(rows) do
		local x = 0
		for _, w in ipairs(cols) do
			tiles[#tiles + 1] = {x, y, w, h}
			x = x + w
		end
		y = y + h
	end

	-- tiles grouped by size class
	local classes, class_order = {}, {}
	for i, t in ipairs(tiles) do
		local k = t[3] .. 'x' .. t[4]
		if not classes[k] then
			classes[k] = {}
			class_order[#class_order + 1] = k
		end
		classes[k][#classes[k] + 1] = i
	end

	-- moves[slot] = {src, rot, flipH, flipV}
	local moves, done = {}, {}
	for _, a in ipairs(class_order) do
		if not done[a] then
			done[a] = true
			local aw, ah = a:match('^(%d+)x(%d+)$')
			local square = aw == ah
			local b = ah .. 'x' .. aw
			local members, partner = classes[a], (b ~= a) and classes[b] or nil
			if not partner then
				local order = {}
				for _, idx in ipairs(members) do order[#order + 1] = idx end
				SoraShuffle(order, SeedRandom_new(key .. '|perm|' .. a))
				for i = 1, #members do
					moves[members[i]] = {order[i], SoraTransform(SeedRandom_new(key .. '|tf|self|' .. a .. '|' .. (i - 1)), false, square)}
				end
			else
				done[b] = true
				local pair_rng = SeedRandom_new(key .. '|pair|' .. a .. '|' .. b)
				local sa, sb = {}, {}
				for _, idx in ipairs(members) do sa[#sa + 1] = idx end
				for _, idx in ipairs(partner) do sb[#sb + 1] = idx end
				SoraShuffle(sa, pair_rng)
				SoraShuffle(sb, pair_rng)
				local m = math.min(#sa, #sb)
				for i = 1, m do
					moves[sa[i]] = {sb[i], SoraTransform(SeedRandom_new(key .. '|tf|AtoB|' .. a .. '|' .. (i - 1)), true, false)}
				end
				for i = 1, m do
					moves[sb[i]] = {sa[i], SoraTransform(SeedRandom_new(key .. '|tf|BtoA|' .. b .. '|' .. (i - 1)), true, false)}
				end
				local rest_a, rest_b = {}, {}
				for i = m + 1, #sa do rest_a[#rest_a + 1] = sa[i] end
				for i = m + 1, #sb do rest_b[#rest_b + 1] = sb[i] end
				SoraRestMoves(rest_a, key .. '|perm|A|' .. a, key .. '|tf|A|' .. a .. '|rest', square, moves)
				SoraRestMoves(rest_b, key .. '|perm|B|' .. b, key .. '|tf|B|' .. b .. '|rest', square, moves)
			end
		end
	end

	local matrix, rotations, flips = {}, {}, {}
	for i = 0, #tiles - 1 do
		matrix[i], rotations[i], flips[i] = i, 0, 0
	end
	for slot, mv in pairs(moves) do
		matrix[mv[1] - 1] = slot - 1
		rotations[mv[1] - 1] = mv[2]
		flips[mv[1] - 1] = (mv[3] and 1 or 0) + (mv[4] and 2 or 0)
	end

	return cols, rows, matrix, rotations, flips
end

function SoraCanvaKey(chapter_id, token)
	return crypto.HMAC_SHA256Hex(chapter_id, token)
end

----------------------------------------------------------------------------------------------------
-- canva2 mode: n x n grid (chapter.n, default 8) with remainder tiles, rotation only (no flips)
----------------------------------------------------------------------------------------------------

local SoraMulberry32

do
	local state = 0
	local function next()
		state = (state + 0x6D2B79F5) & 0xFFFFFFFF
		local t = ((state ~ (state >> 15)) * (state | 1)) & 0xFFFFFFFF
		local sum = (t + (((t ~ (t >> 7)) * (t | 61)) & 0xFFFFFFFF)) & 0xFFFFFFFF
		t = (sum ~ t) & 0xFFFFFFFF
		t = (t ~ (t >> 14)) & 0xFFFFFFFF
		return t / 4294967296.0
	end
	SoraMulberry32 = function(seed0)
		state = seed0 & 0xFFFFFFFF
		return next
	end
end

function SoraCanva2Config(chapter_id, grid, width, height)
	local h1 = crypto.SHA256Hex(chapter_id .. SORA_CANVA2_SALT)
	local h2 = crypto.SHA256(h1 .. ':nxn:' .. grid .. ':' .. width .. 'x' .. height)
	local rng_next = SoraMulberry32(h2:byte(1) * 16777216 + h2:byte(2) * 65536 + h2:byte(3) * 256 + h2:byte(4))
	local function next_int(bound)
		return math.floor(rng_next() * bound)
	end

	-- grid x grid tiles, remainder tiles are one pixel wider/taller
	local tile_w = math.floor(width / grid)
	local tile_h = math.floor(height / grid)
	local extra_w = width - tile_w * grid
	local extra_h = height - tile_h * grid
	local tiles = {}
	local y = 0
	for r = 0, grid - 1 do
		local h = tile_h + (r < extra_h and 1 or 0)
		local x = 0
		for c = 0, grid - 1 do
			local w = tile_w + (c < extra_w and 1 or 0)
			tiles[#tiles + 1] = {x, y, w, h}
			x = x + w
		end
		y = y + h
	end
	local cols, rows = {}, {}
	for c = 1, grid do cols[c] = tiles[c][3] end
	for r = 1, grid do rows[r] = tiles[(r - 1) * grid + 1][4] end

	-- per-size-class source order + rotations
	local classes, class_order = {}, {}
	for i, t in ipairs(tiles) do
		local k = t[3] .. 'x' .. t[4]
		if not classes[k] then
			classes[k] = {}
			class_order[#class_order + 1] = k
		end
		classes[k][#classes[k] + 1] = i
	end
	local sources, rotations = {}, {}
	for _, k in ipairs(class_order) do
		local order = {}
		for _, idx in ipairs(classes[k]) do order[#order + 1] = idx end
		for count = #order, 2, -1 do
			local j = next_int(count) + 1
			order[count], order[j] = order[j], order[count]
		end
		sources[k] = order
		local aw, ah = k:match('^(%d+)x(%d+)$')
		local rots = {}
		for _ = 1, #order do
			if aw == ah then
				rots[#rots + 1] = next_int(4)
			else
				rots[#rots + 1] = rng_next() < 0.5 and 0 or 2
			end
		end
		rotations[k] = rots
	end

	local matrix, rotations_f, flips = {}, {}, {}
	for i = 0, #tiles - 1 do
		matrix[i], rotations_f[i], flips[i] = i, 0, 0
	end
	local position = {}
	for dstIdx, dst in ipairs(tiles) do
		local k = dst[3] .. 'x' .. dst[4]
		local pos = position[k] or 0
		position[k] = pos + 1
		local src_idx = sources[k][pos + 1]
		matrix[src_idx - 1] = dstIdx - 1
		rotations_f[src_idx - 1] = rotations[k][pos + 1]
	end

	return cols, rows, matrix, rotations_f, flips
end

local function SoraXorKey(s, key)
	local out = {}
	for i = 1, #s do
		out[i] = string.char((s:byte(i) ~ key:byte(((i - 1) % #key) + 1)) & 0xFF)
	end
	return table.concat(out)
end

local function SoraIsoToMillis(s)
	local y, mo, d, h, mi, se, ms = s:match('^(%d%d%d%d)-(%d%d)-(%d%d)T(%d%d):(%d%d):(%d%d)%.?(%d?%d?%d?)')
	if not y then return 0 end
	local yy = tonumber(y)
	local mm = tonumber(mo)
	if mm <= 2 then yy = yy - 1 end
	local era = math.floor(yy / 400)
	local yoe = yy - era * 400
	local doy = math.floor((153 * ((mm + 9) % 12) + 2) / 5) + tonumber(d) - 1
	local doe = yoe * 365 + math.floor(yoe / 4) - math.floor(yoe / 100) + doy
	local days = era * 146097 + doe - 719468
	local secs = days * 86400 + tonumber(h) * 3600 + tonumber(mi) * 60 + tonumber(se)
	local millis = 0
	if ms and #ms > 0 then millis = math.floor(tonumber(ms) * 10 ^ (3 - #ms)) end
	return secs * 1000 + millis
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPages .. (URL + 1) .. '.json'

	if not HTTP.GET(u) then return net_problem end

	local series = CreateTXQuery(HTTP.Document).XPath('json(*).list()')
	if series.Count == 0 then return no_error end

	for v in series.Get() do
		LINKS.Add('manga/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('parse-json(//script[@id="__NEXT_DATA__"])?props?pageProps?data?manga')
	MANGAINFO.Title     = x.XPathString('name', info)
	MANGAINFO.AltTitles = x.XPathStringAll('names?*?name', ', ', info)
	MANGAINFO.CoverLink = 'https://i.mangaraw.lat/' .. x.XPathString('image', info)
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Genres    = x.XPathStringAll('genres?*?name', ', ', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('type', info), 'incomplete', 'complete')
	local ok, summary   = pcall(require 'utils.json'.decode, x.XPathString('content', info))
	if ok then
		MANGAINFO.Summary = CreateTXQuery(summary.blocks[2].data.text).XPathString('string-join(//text(), "\r\n")')
	end

	for v in x.XPath('chapters?*', info).Get() do
		local path = v.GetProperty('path').ToString():gsub('%-ch%-', '/ch-')
		local name = v.GetProperty('name').ToString()
		local title = v.GetProperty('title').ToString()

		title = title ~= '' and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add('manga/' .. path)
		MANGAINFO.ChapterNames.Add(name .. '話' .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	if tonumber(require 'fmd.env'.Revision) < 7007 then
		print('Require unreleased FMD2 build')
		return false
	end
	local json = require 'utils.json'
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local data = CreateTXQuery(HTTP.Document).XPath('parse-json(//script[@id="__NEXT_DATA__"])?props?pageProps?data?chapter')

	if MODULE.Storage['api_image'] == '' then
		if not HTTP.GET(MODULE.RootURL .. '/config.json') then return false end
		MODULE.Storage['api_image'] = json.decode(HTTP.Document.ToString()).apiImage or ''
	end

	local fragment
	if data.GetProperty('mode').ToString() == 'canva' then
		fragment = 'canva=' .. data.GetProperty('id').ToString() .. ':' .. data.GetProperty('token').ToString()
	elseif data.GetProperty('mode').ToString() == 'canva2' then
		local n = tonumber(data.GetProperty('n').ToString())
		if not n or n < 1 or n > 64 then n = 8 end
		fragment = 'canva2=' .. data.GetProperty('id').ToString() .. ':' .. math.floor(n)
	end

	local manifest_url = MODULE.Storage['api_image'] .. '/' .. data.GetProperty('manga_id').ToString() .. '/' .. data.GetProperty('id').ToString() .. '.json?t=' .. SoraIsoToMillis(data.GetProperty('updated_at').ToString())

	if not HTTP.GET(manifest_url) then return false end

	local raw = SoraXorKey(crypto.DecodeBase64URL(json.decode(HTTP.Document.ToString()).d), SORA_MANIFEST_KEY)
	while #raw > 0 and raw:byte(#raw) == 0 do raw = raw:sub(1, -2) end
	local entries = json.decode(raw)
	table.sort(entries, function(a, b) return a.order < b.order end)

	local uuidKey = crypto.HexToStr(data.GetProperty('uuid').ToString())
	for _, entry in ipairs(entries) do
		local blob = SoraXorKey(crypto.DecodeBase64URL(entry.b), SORA_IMAGE_KEY)
		local path = crypto.AESCTR(blob:sub(17), uuidKey, blob:sub(1, 16))
		local image_url = data.GetProperty('_b').ToString() .. '/' .. path
		if fragment then image_url = image_url .. '#' .. fragment end
		TASK.PageLinks.Add(image_url)
	end

	return true
end

-- Download, decrypt and/or descramble image given the image URL.
function DownloadImage()
	if not HTTP.GET(URL) then return false end

	local mode, args = URL:match('^.-#(canva2?)=(.+)$')
	if not mode then return true end

	local w, h = imagepuzzle.ImageSize(HTTP.Document)
	if not w then return false end

	local cols, rows, matrix, rotations, flips
	if mode == 'canva' then
		local chapter_id, token = args:match('^(.-):(.*)$')
		cols, rows, matrix, rotations, flips = SoraCanvaConfig(SoraCanvaKey(chapter_id, token), w, h)
	else
		local chapter_id, n = args:match('^(.-):(%d+)$')
		cols, rows, matrix, rotations, flips = SoraCanva2Config(chapter_id, tonumber(n), w, h)
	end

	local puzzle = imagepuzzle.Create(#cols, #rows)
	puzzle.SetUnevenGrid(cols, rows)
	local count = #cols * #rows
	for i = 0, count - 1 do
		puzzle.Matrix[i] = matrix[i]
		puzzle.Rotations[i] = rotations[i]
		puzzle.Flips[i] = flips[i]
	end
	puzzle.DeScramble(HTTP.Document, HTTP.Document)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'bc63a468979e4808aaf4e87d8299a0f2'
	m.Name                     = 'SoraRaw'
	m.RootURL                  = 'https://soraraw.com'
	m.Category                 = 'Raw'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
end
