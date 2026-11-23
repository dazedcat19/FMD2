----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local crypto = require 'fmd.crypto'
local json = require 'utils.json'
DirectoryPages = '/manga/page/'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Extract the {...} JSON object literal from a script.
local function ObjectLiteral(s)
	local first = s:find('{', 1, true)
	if not first then return '' end
	local last = #s
	while last > first and s:byte(last) ~= string.byte('}') do
		last = last - 1
	end
	return s:sub(first, last)
end

-- DJB2-style 32-bit hash of the site's imip reader.
local function ImipHash(text, length)
	local h = 5381
	for i = 1, #text do
		h = (h * 33 + text:byte(i)) % 0x100000000
	end
	h = (h * 33 + (math.floor(length) % 0x10000)) % 0x100000000
	if h == 0 then h = 1 end
	return h
end

-- xorshift PRNG seeded with ImipHash.
local function ImipRandom(seed)
	local state = seed % 0x100000000
	return function()
		state = state ~ ((state << 13) % 0x100000000)
		state = state ~ (state >> 17)
		state = state ~ ((state << 5) % 0x100000000)
		return state
	end
end

-- Proof for the imip gate unlock request.
local function ImipProof(token, chapter_id)
	return tostring(ImipHash(token .. ':' .. chapter_id .. ':imip_v24', tonumber(chapter_id) or 0))
end

-- Unlock the imip gate.
local function ImipUnlock(gate, token, chapter_id)
	local seed
	if not gate.s then
		seed = token .. '_' .. chapter_id
	else
		seed = token .. '_' .. chapter_id .. '_' .. gate.s
	end
	local random = ImipRandom(ImipHash(seed, 32))
	local masked = crypto.DecodeBase64(gate.k)
	if #masked < 32 then return nil end
	local key = {}
	for i = 1, 32 do
		key[i] = string.char((masked:byte(i) ~ (random() % 256)) & 0xFF)
	end
	key = table.concat(key)
	local raw = crypto.DecodeBase64(gate.u)
	local out = {}
	for i = 1, #raw do
		out[i] = string.char((raw:byte(i) ~ (random() % 256) ~ key:byte(((i - 1) % 32) + 1)) & 0xFF)
	end
	local urls = json.decode(table.concat(out))
	if not urls then return nil end
	return key, urls
end

-- Decrypt the imip-protected image bytes.
local function ImipDecrypt(data, key)
	local size = #data
	if size < 68 or data:byte(size - 2) ~= 0x38 or data:byte(size - 1) ~= 0x7E then return nil end
	local shuffled
	if data:byte(size) == 0x91 then
		shuffled = false
	elseif data:byte(size) == 0x92 then
		shuffled = true
	else
		return nil
	end

	local total_len = size - 36
	local scramble_len = math.min(1024, total_len - 32)

	local random = ImipRandom(ImipHash(key, total_len))
	local body_mask = {}
	for i = 1, scramble_len do body_mask[i] = random() % 256 end
	local header_mask = {}
	for i = 1, 32 do header_mask[i] = random() % 256 end

	local checksum = 0x5A
	local header = {}
	for i = 0, 31 do
		local b = (data:byte(total_len + i + 1) ~ key:byte(((i * 13 + 3) % #key) + 1) ~ header_mask[i + 1]) & 0xFF
		checksum = checksum ~ b
		header[i + 1] = string.char(b)
	end
	if (data:byte(size - 3) ~ 0xA5) ~= checksum then return nil end

	local out = {}
	for i = 1, scramble_len do
		local b = (data:byte(32 + i) ~ key:byte((((i - 1) * 7 + 11) % #key) + 1) ~ body_mask[i]) & 0xFF
		out[i] = string.char(((b << 4) | (b >> 4)) & 0xFF)
	end
	return table.concat(header) .. table.concat(out) .. data:sub(32 + scramble_len + 1, total_len), shuffled
end

-- Strip order of the shuffled pages.
local function ImipStrips(key)
	local random = ImipRandom(ImipHash(key .. '_imip_s25', 16))
	local strips = {}
	for i = 0, 15 do strips[i] = i end
	for i = 15, 1, -1 do
		local j = (random() % 0x80000000) % (i + 1)
		strips[i], strips[j] = strips[j], strips[i]
	end
	return strips
end

-- Register one reader element.
local function AddReaderPage(v)
	local url = v.GetAttribute('data-enc')
	if url ~= '' then
		local raw = crypto.DecodeBase64(url)
		local out = {}
		for i = #raw, 1, -1 do out[#out + 1] = raw:sub(i, i) end
		url = table.concat(out)
	else
		for _, attr in ipairs({ 'data-original-src', 'data-src', 'src' }) do
			local u = v.GetAttribute(attr)
			if u ~= '' and not u:find('^data:') then
				url = u
				break
			end
		end
	end
	if url ~= '' then
		TASK.PageLinks.Add(url)
	end
end

-- Ask the REST endpoint for the chapter AES key.
local function FetchChapterKey(payload, nonce, chapter_url)
	if not payload.cid or not payload.e or not payload.g then return nil end
	local token = ''
	for _ = 1, 3 do
		local body = '{"chapter_id":' .. payload.cid
			.. ',"epoch":' .. payload.e
			.. ',"grant":' .. json.encode(payload.g)
			.. ',"verdict":"pass","score":0,"codes":[],"challenge_token":' .. json.encode(token) .. '}'
		HTTP.Reset()
		if nonce and nonce ~= '' then
			HTTP.Headers.Values['X-WP-Nonce'] = nonce
		end
		HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
		HTTP.Headers.Values['Referer'] = chapter_url
		HTTP.MimeType = 'application/json'
		if not HTTP.POST(MODULE.RootURL .. '/wp-json/initmanga/v1/chapter-key', body) then return nil end
		local resp = json.decode(HTTP.Document.ToString())
		if not resp then return nil end
		if resp.key and resp.key ~= '' then
			return crypto.HexToStr(resp.key)
		elseif resp.code == 'imc_challenge_required' and resp.data and resp.data.token then
			token = resp.data.token
			if sleep then sleep(500) end
		elseif resp.code == 'imc_rate_limited' then
			return nil
		elseif nonce and nonce ~= '' and resp.code == 'rest_cookie_invalid_nonce' then
			nonce = nil
		else
			return nil
		end
	end
	return nil
end

-- Unlock the imip protected gallery and register the encrypted page urls.
local function GetProtectedPages(gallery, scripts, chapter_url)
	local vault
	for _, s in ipairs(scripts) do
		if s:find('admin-ajax', 1, true) and s:find('"nonce"', 1, true) then
			local v = json.decode(ObjectLiteral(s))
			if v and v.url and v.nonce and v.tok then
				vault = v
				break
			end
		end
	end
	if not vault then return false end

	local chapter_id = gallery.GetAttribute('data-cid')
	if chapter_id == '' then return false end

	local boundary = '----InitManga' .. math.random(100000000, 999999999)
	local body = {}
	for _, field in ipairs({
		{ 'action', 'imip_gate_unlock' },
		{ 'cid', chapter_id },
		{ 'nonce', vault.nonce },
		{ 'tok', vault.tok },
		{ 'p', ImipProof(vault.tok, chapter_id) },
		{ 'e', gallery.GetAttribute('data-e') ~= '' and gallery.GetAttribute('data-e') or '0' },
		{ 'g', gallery.GetAttribute('data-g') },
	}) do
		body[#body + 1] = '--' .. boundary .. '\r\nContent-Disposition: form-data; name="' .. field[1] .. '"\r\n\r\n' .. field[2] .. '\r\n'
	end
	body[#body + 1] = '--' .. boundary .. '--\r\n'

	HTTP.Reset()
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
	HTTP.Headers.Values['Referer'] = chapter_url
	HTTP.MimeType = 'multipart/form-data; boundary=' .. boundary
	if not HTTP.POST(vault.url, table.concat(body)) then return false end

	local resp = json.decode(HTTP.Document.ToString())
	local gate = resp and resp.data or nil
	if not gate or not gate.k or not gate.u then return false end

	local key, urls = ImipUnlock(gate, vault.tok, chapter_id)
	if not key then return false end

	for _, u in ipairs(urls) do
		TASK.PageLinks.Add(u .. '#imip=' .. key)
	end
	return true
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPages .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[contains(@class, "uk-pagination")]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPages
	if DirectoryPages:find('/page/', 1, true) then u = u .. (URL + 1) end

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[contains(@class, "manga-item-grid")]//h2/a | //div[contains(@class, "manga-item-details")]//h2/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)

	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//span[@id="comic-othername"]')
	MANGAINFO.CoverLink = x.XPathString('//a[contains(@class, "story-cover")]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//div[contains(@class, "manga-info-details") and contains(., "Author")]/span')
	MANGAINFO.Artists   = x.XPathString('//div[contains(@class, "manga-info-details") and contains(., "Illustrator")]/span')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@id="genre-tags"]/a[not(contains(@class, "uk-disabled"))]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@id="manga-status"]'), 'ongoing|caught_up', 'completed', 'season_end|source_hiatus', 'dropped')
	MANGAINFO.Summary   = x.XPathString('//div[@id="manga-description"]/p')

	local manga_id = x.XPathString('//body/@class'):match('postid%-(%d+)')
	local slug = URL:match('/([^/]+)/$')

	local show_paid = MODULE.GetOption('showpaid')

	local page, pages = 1
	while true do
		HTTP.Reset()
		HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
		HTTP.Headers.Values['Referer'] = u

		u = MODULE.RootURL .. '/wp-json/initmanga/v1/chapters?manga_id=' .. manga_id .. '&per_page=50&paged=' .. page

		if not HTTP.GET(u) then return net_problem end

		local data = json.decode(HTTP.Document.ToString())
		for _, chapter in ipairs(data.items or {}) do
		local is_locked = chapter.lock_type ~= 'none' and not chapter.is_purchased
		if show_paid or not is_locked then
			local name = chapter.title
			if not name:find('Chapter', 1, true) then
				name = 'Chapter ' .. chapter.number
				if chapter.title ~= '' then name = name .. ' - ' .. chapter.title end
			end
			if is_locked then name = '🔒 ' .. name end
			MANGAINFO.ChapterLinks.Add(slug .. '/' .. chapter.slug)
			MANGAINFO.ChapterNames.Add(name)
		end
		end
		if not pages then
			pages = tonumber(data.total_pages) or 1
		end
		if page >= pages then break end
		page = page + 1
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MODULE.RootURL .. '/manga' .. URL

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPathCount('//div[contains(@class, "lock-card")]') > 0 then return true end

	local scripts = {}
	for v in x.XPath('//script').Get() do
		local b64 = v.GetAttribute('src'):match('base64,(.+)$')
		if b64 then
			local decoded = crypto.DecodeBase64(b64)
			if decoded ~= '' then scripts[#scripts + 1] = decoded end
		else
			local code = v.ToString()
			if code ~= '' then scripts[#scripts + 1] = code end
		end
	end

	local gallery
	for v in x.XPath('//div[contains(@class, "imip-protected-gallery")]').Get() do
		gallery = v
		break
	end
	if gallery then
		return GetProtectedPages(gallery, scripts, u)
	end

	local payload, data
	for _, s in ipairs(scripts) do
		if not payload and s:find('"ciphertext"', 1, true) then payload = json.decode(ObjectLiteral(s)) end
		if not data and s:find('"decryption_key"', 1, true) then data = json.decode(ObjectLiteral(s)) end
	end

	if payload then
		if not data or not data.decryption_key then return false end
		if not payload or not payload.ciphertext or not payload.iv then return false end
		local key
		if payload.salt then
			key = crypto.PBKDF2SHA512(crypto.DecodeBase64(data.decryption_key), crypto.HexToStr(payload.salt), 999, 32)
		else
			key = FetchChapterKey(payload, data.nonce, u)
		end
		if not key or key == '' then return false end
		local content = crypto.AESDecryptCBC(crypto.DecodeBase64(payload.ciphertext), key, crypto.HexToStr(payload.iv))
		if content == '' then return false end
		x.ParseHTML(content)
		for v in x.XPath('//img | //canvas[@data-enc]').Get() do
			AddReaderPage(v)
		end
	else
		for v in x.XPath('//div[@id="chapter-content"]/img').Get() do
			AddReaderPage(v)
		end
	end

	return true
end

-- Download, decrypt and/or descramble the image of the current page.
function _M.DownloadImage()
	local key = URL:match('#imip=(.+)$')

	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	if not HTTP.GET(URL) then return false end

	if not key then return true end

	local image, shuffled = ImipDecrypt(HTTP.Document.ToString(), key)
	if not image then return false end

	HTTP.Document.WriteString(image)

	if shuffled then
		local imagepuzzle = require 'fmd.imagepuzzle'
		local w, h = imagepuzzle.ImageSize(HTTP.Document)
		local strip_height = h and h // 128 * 8 or 0
		if w and w >= 64 and strip_height >= 16 then
			local rows = {}
			for _ = 1, 16 do rows[#rows + 1] = strip_height end
			local rest = h - 16 * strip_height
			if rest > 0 then rows[#rows + 1] = rest end
			local strips = ImipStrips(key)
			local puzzle = imagepuzzle.Create(1, #rows)
			puzzle.SetUnevenGrid({ w }, rows)
			for i = 0, 15 do puzzle.Matrix[i] = strips[i + 1] end
			if rest > 0 then puzzle.Matrix[16] = 16 end
			puzzle.DeScramble(HTTP.Document, HTTP.Document)
		end
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M