----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local PatchSecret  = 'r0sy_p4tch_8831_m7z'
local ShieldSecret = 'r0sy_sh1eld_9472_k9x'
local PatchModulus = 89
local PatchMinV    = 1760000000

-- Cache of the decrypted patches JSON of the last chapter.
local PatchCache = { key = nil, json = nil }

local function Fnv1a(s)
	local h = 0x811c9dc5
	for i = 1, #s do
		h = ((h ~ s:byte(i)) * 0x01000193) & 0xFFFFFFFF
	end
	return h
end

local function XorShift(x)
	x = x & 0xFFFFFFFF
	if x == 0 then x = 0x9e3779b9 end
	x = x ~ ((x << 13) & 0xFFFFFFFF)
	x = x ~ (x >> 17)
	x = x ~ ((x << 5) & 0xFFFFFFFF)
	return x & 0xFFFFFFFF
end

-- Decrypt the base64 payload of window.__rsP. Layout: 8 byte nonce + ciphertext.
local function DecryptPatches(b64, manga_id, chapter)
	local bin = require('fmd.crypto').DecodeBase64(b64)
	if #bin < 9 then return nil end

	local nonce = ''
	for i = 1, 8 do nonce = nonce .. string.format('%02x', bin:byte(i)) end

	local s1 = Fnv1a(PatchSecret .. '|' .. manga_id .. '|' .. chapter .. '|' .. nonce)
	local s2 = Fnv1a(nonce .. '|' .. chapter .. '|' .. PatchSecret .. '|' .. manga_id)

	local len   = #bin - 8
	local out   = {}
	local pos   = 9
	local count = 0
	while len - (pos - 9) >= 4 do
		s1 = XorShift(s1)
		s2 = XorShift((s2 ~ s1) & 0xFFFFFFFF)
		local k = (s1 ~ s2) & 0xFFFFFFFF
		local w = string.unpack('<I4', bin, pos)
		count = count + 1
		out[count] = string.pack('<I4', w ~ k)
		pos = pos + 4
	end
	local rest = len - (pos - 9)
	if rest > 0 then
		s1 = XorShift(s1)
		s2 = XorShift((s2 ~ s1) & 0xFFFFFFFF)
		local k = (s1 ~ s2) & 0xFFFFFFFF
		for j = 0, rest - 1 do
			count = count + 1
			out[count] = string.char(bin:byte(pos + j) ~ ((k >> (j * 8)) & 0xFF))
		end
	end
	return table.concat(out)
end

-- Split an image URL into host, manga id, chapter folder, file name and version.
local function ParseImageURL(url)
	local host, id, chapter, file = url:match('^(https?://[^/]+)/uploads/manga%-images/(%d+)/([^/?#]+)/([^/?#]+)')
	if not host then return nil end
	return {
		host    = host,
		id      = id,
		chapter = chapter,
		file    = file,
		v       = tonumber(url:match('[?&]v=(%d+)')),
	}
end

local function UsesPatches(v)
	if not v or v < PatchMinV then return false end
	local rem = v % PatchModulus
	return rem == 79 or rem == 83
end

-- Indexes of the decoy images the site's script hides.
local function DecoyBlocks(id, chapter, total, tag, size, t2, t3, t4)
	local b = 0
	if total >= t4 then b = 4 elseif total >= t3 then b = 3 elseif total >= t2 then b = 2 end
	if b <= 0 then return {} end
	local rest = total - b * size
	if rest < b + 1 then return {} end
	local lo, span = 1, rest - 1
	local seed = Fnv1a(ShieldSecret .. '|' .. tag .. '|' .. id .. '|' .. chapter .. '|' .. total)
	local out = {}
	for i = 0, b - 1 do
		local a = lo + (i * span) // b
		local e = lo + ((i + 1) * span) // b - 1
		if e < a then e = a end
		seed = XorShift(seed)
		local start = a + seed % (e - a + 1) + i * size
		for off = 0, size - 1 do out[#out + 1] = start + off end
	end
	return out
end

local function DecoyScattered(id, chapter, total)
	if total < 8 then return {} end
	local k = math.min(6, math.max(2, total // 8))
	local lo, span = 1, total - 2
	if span < k then return {} end
	local seed = Fnv1a(ShieldSecret .. '|' .. id .. '|' .. chapter .. '|' .. total)
	local out = {}
	for i = 0, k - 1 do
		local a = lo + (i * span) // k
		local e = lo + ((i + 1) * span) // k - 1
		if e < a then e = a end
		seed = XorShift(seed)
		out[#out + 1] = a + seed % (e - a + 1)
	end
	return out
end

local function GetDecoyIndexes(info, total)
	if not info.v or info.v < PatchMinV then return {} end
	local rem = info.v % PatchModulus
	if rem == 79 or rem == 71 then
		return DecoyBlocks(info.id, info.chapter, total, 'v3', 16, 38, 64, 99)
	elseif rem == 53 then
		return DecoyBlocks(info.id, info.chapter, total, 'v2', 8, 22, 40, 67)
	elseif rem == 37 then
		return DecoyScattered(info.id, info.chapter, total)
	end
	return {}
end

-- Download and decrypt patches.js of the chapter the given image belongs to (cached per chapter).
local function GetPatchesJSON(info)
	local key = info.id .. '/' .. info.chapter .. '/' .. info.v
	if PatchCache.key == key then return PatchCache.json end

	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	local u = info.host .. '/uploads/manga-images/' .. info.id .. '/' .. info.chapter .. '/patches.js?v=' .. info.v
	if not HTTP.GET(u) then
		if HTTP.ResultCode == 404 then
			PatchCache.key, PatchCache.json = key, ''
			return ''
		end
		return nil
	end

	local json = ''
	local enc  = HTTP.Document.ToString():match('__rsP%s*=%s*"([^"]+)"')
	if enc then
		json = DecryptPatches(enc, info.id, info.chapter) or ''
		if json:sub(1, 1) ~= '{' then
			require('fmd.logger').SendWarning('Royscans: could not decrypt patches.js, the secret may have changed.')
			json = ''
		end
	end
	PatchCache.key, PatchCache.json = key, json
	return json
end

-- Return the list of { left, top, width, height, base64 } patches for one image file name.
local function GetPatchesForFile(json, file)
	local list = {}
	local s, e = json:find('"' .. file .. '":[', 1, true)
	if not s then return list end

	local pos = e + 1
	local num = '(-?[%d%.eE%+%-]+)'
	local pat = '^%s*%[' .. num .. ',' .. num .. ',' .. num .. ',' .. num .. ',"([^"]*)"%]'
	while true do
		local _, ne, l, t, w, h, data = json:find(pat, pos)
		if not ne then break end
		list[#list + 1] = { tonumber(l), tonumber(t), tonumber(w), tonumber(h), (data:gsub('\\/', '/')) }
		pos = ne + 1
		if json:sub(pos, pos) == ',' then pos = pos + 1 end
	end
	return list
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
	Template.GetPageNumber()

	local first = ParseImageURL(TASK.PageLinks[0])
	if first then
		local hidden = {}
		for _, idx in ipairs(GetDecoyIndexes(first, TASK.PageLinks.Count)) do hidden[idx] = true end
		local keep = {}
		for i = 0, TASK.PageLinks.Count - 1 do
			if not hidden[i] then keep[#keep + 1] = TASK.PageLinks[i] end
		end
		if #keep > 0 then
			TASK.PageLinks.Clear()
			for _, link in ipairs(keep) do TASK.PageLinks.Add(link) end
		end
	end

	return true
end

-- Download the image and paste the original pixels back over the watermark banner.
function DownloadImage()
	local info = ParseImageURL(URL)
	local json = ''
	if info and UsesPatches(info.v) then
		json = GetPatchesJSON(info)
		if json == nil then return false end
	end

	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	if not HTTP.GET(URL) then return false end

	if json ~= '' then
		local list = GetPatchesForFile(json, info.file)
		if #list > 0 then
			local ok, lib = pcall(require, 'fmd.imagepatch')
			if not ok then
				require('fmd.logger').SendWarning('Royscans: "imagepatch" is missing from this FMD2 build, the watermark can not be removed.')
				return true
			end
			local patch = lib.Create()
			patch.JpegQuality = 90
			for _, p in ipairs(list) do
				patch.Add(p[1], p[2], p[3], p[4], p[5])
			end
			if not patch.Apply(HTTP.Document, HTTP.Document) then return false end
		end
	end

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
	m.OnDownloadImage          = 'DownloadImage'
end
