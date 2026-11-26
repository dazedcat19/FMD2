----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'

local Crypto = require 'fmd.crypto'
local JSON = require 'utils.json'

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

-- Royscans adds two client-side protection layers on top of the plain MangaThemesia reader
-- (see the obfuscated inline script on every chapter page). This module mirrors both:
--
-- 1) ts_reader's image list mixes the real pages with "decoy" images taken from other
--    chapters. The decoy positions are derived from an FNV-1a hash + xorshift32 PRNG seeded
--    with manga id, chapter dir and the image count; the ?v= value decides which scheme
--    (singles / blocks / none) is used for a chapter.
--
-- 2) The real images are served with a big watermark banner baked in. The clean pixels for
--    the watermarked area are stored in <imagehost>/uploads/manga-images/<manga>/<chapter>/
--    patches.js as window.__rsP - a Base64 blob encrypted with an FNV-1a/xorshift32 keystream
--    keyed by manga id + chapter dir + an 8-byte nonce embedded in the blob. It decrypts to
--    JSON: { "001.jpg": [ [left%, top%, width%, height%, webpPatchBase64], ... ], ... }
--    The browser draws each webp patch over the watermarked area; this module does the same
--    merge natively through imagepuzzle.MergeOverlay() (FMD2 build with the Royscans patch).
--
local PATCH_SEED_KEY  = 'r0sy_sh1eld_9472_k9x'
local PATCH_CRYPT_KEY = 'r0sy_p4tch_8831_m7z'
local MIN_VERSION     = 1760000000          -- chapters with a smaller ?v= carry no protection
local MANGA_IMAGES    = '/uploads/manga%-images/'
local PATCH_FILE      = '/patches.js?v='

----------------------------------------------------------------------------------------------------
-- Local Functions
----------------------------------------------------------------------------------------------------

-- 32 bit FNV-1a over the bytes of s (mirror of the site's _0x2b33).
local function FNV1a(s)
	local h = 0x811c9dc5
	for i = 1, #s do
		h = h ~ s:byte(i)
		h = (h * 0x01000193) & 0xFFFFFFFF
	end
	return h & 0xFFFFFFFF
end

-- 32 bit xorshift32 (mirror of the site's _0x6025).
local function XorShift32(x)
	x = x & 0xFFFFFFFF
	if x == 0 then
		x = 0x9e3779b9
	end
	x = x ~ ((x << 13) & 0xFFFFFFFF)
	x = x ~ (x >> 17)
	x = x ~ ((x << 5) & 0xFFFFFFFF)
	return x & 0xFFFFFFFF
end

-- Mirror of the site's _0x8d21: k single decoy positions (0 based), used when v % 89 == 37.
local function DecoySingles(key1, key2, total)
	if total < 8 then
		return {}
	end
	local k = math.min(6, math.max(2, math.floor(total / 8)))
	local lo, hi = 1, total - 2
	local range = hi - lo + 1
	if range < k then
		return {}
	end
	local x = FNV1a(PATCH_SEED_KEY .. '|' .. key1 .. '|' .. key2 .. '|' .. total)
	local out = {}
	for i = 0, k - 1 do
		local a = lo + math.floor((i * range) / k)
		local b = lo + math.floor(((i + 1) * range) / k) - 1
		if b < a then
			b = a
		end
		x = XorShift32(x)
		out[#out + 1] = a + (x % (b - a + 1))
	end
	return out
end

-- Mirror of the site's _0x5db3: b blocks of blocksize decoys, used when v % 89 is 79/71
-- (16, "v3", 38, 64, 99) or 53 (8, "v2", 22, 40, 67).
local function DecoyBlocks(key1, key2, total, blocksize, tag, t2, t3, t4)
	local b = 0
	if total >= t4 then
		b = 4
	elseif total >= t3 then
		b = 3
	elseif total >= t2 then
		b = 2
	end
	if b <= 0 then
		return {}
	end
	local free = total - b * blocksize
	if free < b + 1 then
		return {}
	end
	local lo, hi = 1, free - 1
	local range = hi - lo + 1
	local x = FNV1a(PATCH_SEED_KEY .. '|' .. tag .. '|' .. key1 .. '|' .. key2 .. '|' .. total)
	local out = {}
	for i = 0, b - 1 do
		local a = lo + math.floor((i * range) / b)
		local e = lo + math.floor(((i + 1) * range) / b) - 1
		if e < a then
			e = a
		end
		x = XorShift32(x)
		local pos = a + (x % (e - a + 1)) + i * blocksize
		for off = 0, blocksize - 1 do
			out[#out + 1] = pos + off
		end
	end
	return out
end

-- Decrypt window.__rsP (mirror of the site's _0x2fe5). Returns the decrypted JSON string.
local function DecryptPatchPayload(b64, key1, key2)
	local bin = Crypto.DecodeBase64(b64)
	if not bin or #bin < 9 then
		return nil
	end
	local nonce = {}
	for n = 1, 8 do
		nonce[#nonce + 1] = string.format('%02x', bin:byte(n))
	end
	nonce = table.concat(nonce)
	local s1 = FNV1a(PATCH_CRYPT_KEY .. '|' .. key1 .. '|' .. key2 .. '|' .. nonce)
	local s2 = FNV1a(nonce .. '|' .. key2 .. '|' .. PATCH_CRYPT_KEY .. '|' .. key1)
	local out = {}
	local ks = 0
	for i = 0, #bin - 9 do
		if (i & 3) == 0 then
			s1 = XorShift32(s1)
			s2 = XorShift32(s2 ~ s1)
			ks = s1 ~ s2
		end
		out[#out + 1] = string.char(bin:byte(i + 9) ~ ((ks >> ((i & 3) * 8)) & 0xFF))
	end
	return table.concat(out)
end

-- Extract manga id, chapter dir and version from an image URL; nil when it is not a
-- manga-images link.
local function ParseImageInfo(u)
	if not u then
		return nil
	end
	local key1, key2 = u:match(MANGA_IMAGES .. '(%d+)/([^/?#]+)/')
	if not key1 then
		return nil
	end
	return key1, key2, tonumber(u:match('[?&]v=(%d+)'))
end

-- The decrypted patches for the chapter currently processed by this thread:
-- { info = '<key1>|<key2>|<ver>', data = { ['001.jpg'] = { { x, y, b64 }, ... }, ... } }
local Patches = nil
local MergeWarned = false

local function LoadPatches(firstLink)
	local key1, key2, ver = ParseImageInfo(firstLink)
	if not key1 or not ver or ver < MIN_VERSION then
		Patches = nil
		return
	end

	-- the site only loads patches for these v % 89 branches
	local rem = ver % 89
	if rem ~= 83 and rem ~= 79 and rem ~= 71 and rem ~= 53 then
		Patches = nil
		return
	end

	local info = key1 .. '|' .. key2 .. '|' .. ver
	if Patches and Patches.info == info then
		return
	end
	Patches = nil

	local scheme, host = firstLink:match('^(https?://)([^/]+)')
	if not scheme then
		return
	end
	local u = scheme .. host .. '/uploads/manga-images/' .. key1 .. '/' .. key2 .. PATCH_FILE .. ver

	-- patches.js rejects requests without a Referer of the main site.
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	if not HTTP.GET(u) then
		print('Royscans: failed to download patches.js (' .. u .. ')')
		return
	end

	local payload = HTTP.Document.ToString():match('window%.__rsP%s*=%s*"([^"]+)"')
	if not payload then
		print('Royscans: patches.js does not contain __rsP')
		return
	end

	local ok, plain = pcall(DecryptPatchPayload, payload, key1, key2)
	if not ok or not plain then
		print('Royscans: failed to decrypt patches.js')
		return
	end

	local okd, data = pcall(JSON.decode, plain)
	if not okd or type(data) ~= 'table' then
		print('Royscans: failed to parse decrypted patches: ' .. tostring(data))
		return
	end

	local patches = { info = info, data = {} }
	for fname, overlays in pairs(data) do
		if type(overlays) == 'table' and #overlays > 0 then
			local list = {}
			for _, it in ipairs(overlays) do
				if type(it) == 'table' and it[5] then
					list[#list + 1] = { x = tonumber(it[1]) or 0, y = tonumber(it[2]) or 0, b64 = it[5] }
				end
			end
			if #list > 0 then
				patches.data[fname] = list
			end
		end
	end
	Patches = patches
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local s = x.XPathString('//script[contains(., "ts_reader")]')
	if s == '' then return false end
	local payload = s:match('run%((.-)%);')
	if not payload then return false end
	x.ParseHTML(payload:gsub('!0', 'true'):gsub('!1', 'false'))
	x.XPathStringAll('json(*).sources()[1].images()', TASK.PageLinks)
	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub('i%d.wp.com/', '')
		if string.find(TASK.PageLinks[i], 'blogger') or string.find(TASK.PageLinks[i], 'blogspot') then
			TASK.PageLinks[i] = TASK.PageLinks[i]:gsub('/s1600/', '/s0/')
		end
	end

	-- Drop the decoy images the site mixed into the reader list.
	local count = TASK.PageLinks.Count
	if count > 0 then
		local key1, key2, ver = ParseImageInfo(TASK.PageLinks[0])
		if key1 and ver and ver >= MIN_VERSION then
			local rem = ver % 89
			local decoys = {}
			if rem == 79 or rem == 71 then
				decoys = DecoyBlocks(key1, key2, count, 16, 'v3', 38, 64, 99)
			elseif rem == 53 then
				decoys = DecoyBlocks(key1, key2, count, 8, 'v2', 22, 40, 67)
			elseif rem == 37 then
				decoys = DecoySingles(key1, key2, count)
			end
			if #decoys > 0 then
				local removed = 0
				table.sort(decoys, function(a, b) return a > b end)
				for _, idx in ipairs(decoys) do
					-- sanity check: only drop links that really point somewhere else
					local k = ParseImageInfo(TASK.PageLinks[idx])
					if idx < count and (k == nil or k ~= key1) then
						TASK.PageLinks.Delete(idx)
						removed = removed + 1
					end
				end
				if removed > 0 then
					print('Royscans: removed ' .. removed .. ' decoy images')
				end
			end
		end
	end

	return true
end

-- Download and decrypt and/or descramble image given the image URL.
function DownloadImage()
	-- LoadPatches() reuses HTTP for its own GET, so it must run before the image download.
	local fname = URL:match('.*/([^/?#]+)%?') or URL:match('.*/([^/?#]+)$')
	if fname and ParseImageInfo(URL) then
		LoadPatches(URL)
	end

	if not HTTP.GET(URL) then return false end

	if fname and Patches then
		local overlays = Patches.data[fname]
		if overlays then
			local ok, imagepuzzle = pcall(require, 'fmd.imagepuzzle')
			if ok and imagepuzzle and type(imagepuzzle.MergeOverlay) == 'function' then
				local okw, w, h = pcall(imagepuzzle.ImageSize, HTTP.Document)
				if okw and w and h and w > 0 and h > 0 then
					for _, it in ipairs(overlays) do
						local okb, patch = pcall(Crypto.DecodeBase64, it.b64)
						if okb and patch and #patch > 0 then
							local px = math.floor(w * it.x / 100 + 0.5)
							local py = math.floor(h * it.y / 100 + 0.5)
							if not imagepuzzle.MergeOverlay(HTTP.Document, patch, px, py) then
								print('Royscans: MergeOverlay failed for ' .. fname)
								break
							end
						end
					end
				end
			elseif not MergeWarned then
				MergeWarned = true
				print('Royscans: this FMD2 build has no imagepuzzle.MergeOverlay, so the site watermark stays baked in. Rebuild FMD2 with the Royscans patch (see the fix README).')
			end
		end
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'eec17a12c6ef426da1b424a192e21edf'
	m.Name                     = 'Royscans'
	m.RootURL                  = 'https://rosyscans.com'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.OnDownloadImage          = 'DownloadImage'
end
