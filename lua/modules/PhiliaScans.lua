----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c280ce32f36843fbba73dcc891e979af'
	m.Name                     = 'Philia Scans'
	m.RootURL                  = 'https://philiascans.org'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/all-mangas?m_orderby=new-manga&page='
local crypto = require 'fmd.crypto'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function XorStrings(a, b)
	local r = {}
	for i = 1, math.min(#a, #b) do
		r[i] = string.char(string.byte(a, i) ~ string.byte(b, i))
	end
	return table.concat(r)
end

local function U32LE(s, pos)
	return string.byte(s, pos) + string.byte(s, pos + 1) * 256 + string.byte(s, pos + 2) * 65536 + string.byte(s, pos + 3) * 16777216
end

local function GetChapterKey(chapterKeyB64, payloadA, payloadB)
	if payloadA ~= '' and payloadA ~= 'null' and payloadB ~= '' and payloadB ~= 'null' then
		return XorStrings(crypto.DecodeBase64(payloadA), crypto.DecodeBase64(payloadB))
	end

	return crypto.DecodeBase64(chapterKeyB64)
end

local function XorKeystream(chapter_key, page_index, data)
	local out = {}

	for block = 0, math.floor((#data + 31) / 32) - 1 do
		local hash = crypto.HMAC_SHA256('page:' .. page_index .. ':' .. block, chapter_key)

		local base = block * 32
		for j = 0, 31 do
			local pos = base + j + 1

			if pos > #data then
				break
			end

			out[pos] = string.char(string.byte(data, pos) ~ string.byte(hash, j + 1))
		end
	end

	return table.concat(out)
end

local function BuildPermutation(chapter_key, page_index, grid_size)
	local n = grid_size * grid_size
	local c = {}

	for i = 0, n - 1 do
		c[i] = i
	end

	if n >= 2 then
		local tiles_sig = crypto.HMAC_SHA256('tiles:' .. page_index, chapter_key)

		local counter = 0
		local rnd = ''
		local idx = 9

		local function NextRandom()
			if idx > 8 then
				rnd = crypto.HMAC_SHA256('perm:' .. counter, tiles_sig)

				counter = counter + 1
				idx = 1
			end

			local p = (idx - 1) * 4 + 1
			local v = U32LE(rnd, p)

			idx = idx + 1

			return v
		end

		for i = n - 1, 1, -1 do
			local j = NextRandom() % (i + 1)
			c[i], c[j] = c[j], c[i]
		end
	end

	local w = {}

	for i = 0, n - 1 do
		w[c[i]] = i
	end

	return w
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//span[@class="page-current"]'):match('%d+$')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//a[@class="manga-card"]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('.//h3', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local s = HTTP.Document.ToString():gsub('\\"', '"'):gsub('\\\\', '\\'):gsub('"%]%)</script><script>self%.__next_f%.push%(%[1,"', '')
	local x = CreateTXQuery(s)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Authors   = x.XPathString('//li[span="Author"]//a')
	MANGAINFO.Artists   = x.XPathString('//li[span="Artist"]//a')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="detail-genres"]/a, upper-case(substring(//div[span="Type"]/span[2], 1, 1)) || lower-case(substring(//div[span="Type"]/span[2], 2)))')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[span="Status"]/span[2]'), 'On Going', 'Completed', 'On Hold', 'Canceled')
	MANGAINFO.Summary   = x.XPathString('//div[@class="synopsis-text"]/p')

	local slug = URL:match('/([^/]+)$')
	for v in x.XPath('parse-json(//script[contains(., "langChapters")]/substring-before(substring-after(., "langChapters"":"), ",""hasVolumes"))()[coinPrice=0]').Get() do
		local number = v.GetProperty('number').ToString()
		local title = v.GetProperty('title').ToString()

		title = (title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add('manga/' .. slug .. '/chapters/' .. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. number .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local json = require 'utils.json'
	local u = MODULE.RootURL .. '/api' .. URL

	if not HTTP.GET(u) then return false end

	local chapter = json.decode(HTTP.Document.ToString()).chapter
	local chapter_id = chapter.id

	-- access token

	HTTP.Reset()
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
	if not HTTP.POST(MODULE.RootURL .. '/api/reader/access-token') then return false end

	local token = json.decode(HTTP.Document.ToString()).token

	-- page keys

	HTTP.Reset()
	HTTP.Headers.Values['X-Reader-Access-Token'] = token

	if not HTTP.GET(MODULE.RootURL .. '/api/chapters/' .. chapter_id .. '/page-keys') then return false end

	local page_keys = json.decode(HTTP.Document.ToString())

	-- open

	HTTP.Reset()
	HTTP.Headers.Values['X-Reader-Access-Token'] = token

	if not HTTP.POST(MODULE.RootURL .. '/api/chapters/' .. chapter_id .. '/open') then return false end

	local open_data = json.decode(HTTP.Document.ToString())

	-- drm

	HTTP.Reset()
	HTTP.Headers.Values['X-Reader-Access-Token'] = token

	if not HTTP.GET(MODULE.RootURL .. '/api/chapters/' .. chapter_id .. '/get-drm?session=' .. open_data.sessionId) then return false end

	local drm = json.decode(HTTP.Document.ToString())
	local payloadB = drm.payloadB or 'null'

	local isScrambled = chapter.scrambled and '1' or '0'

	for i, page in ipairs(chapter.pages) do
		local image_url = MaybeFillHost(MODULE.RootURL, page.url)

		TASK.PageLinks.Add(
			image_url ..
			'#' ..
			isScrambled .. ';' ..
			page.mime .. ';' ..
			page_keys.chapterKeyB64 .. ';' ..
			page_keys.gridSize .. ';' ..
			(open_data.payloadA or 'null') .. ';' ..
			payloadB .. ';' ..
			(i - 1)
		)
	end

	return true
end

-- Download and decrypt and/or descramble image given the image URL.
function DownloadImage()
	if not HTTP.GET(URL) then return false end

	local parts = {}

	local fragment = URL:match('#(.+)$')
	for v in fragment:gmatch('[^;]+') do
		parts[#parts + 1] = v
	end

	local isScrambled = parts[1] == '1'
	local chapterKeyB64 = parts[3]
	local grid_size = tonumber(parts[4]) or 4
	local payloadA = parts[5]
	local payloadB = parts[6]
	local page_index = tonumber(parts[7]) or 0

	local chapter_key = GetChapterKey(chapterKeyB64, payloadA, payloadB)

	local data = HTTP.Document.ToString()

	if string.byte(data, 1) ~= 0xFF then return true end

	local b1 = string.byte(data, 1)
	local b2 = string.byte(data, 2)

	local aes2 = b1 == 0xFF and b2 == 0x02
	local aes4 = b1 == 0xFF and b2 == 0x04

	local pos

	if aes2 or aes4 then
		pos = 3
	else
		pos = 1
	end

	pos = pos + 4

	local payload = data:sub(pos)

	if aes4 then
		local derived_key = crypto.HMAC_SHA256('aesctr4:' .. page_index, chapter_key)
		payload = crypto.AESCTR(payload, derived_key, string.rep('\0', 16))
	elseif aes2 then
		local derived_key = crypto.HMAC_SHA256('aesctr:' .. page_index, chapter_key)
		payload = crypto.AESCTR(payload, derived_key, string.rep('\0', 16))
	else
		payload = XorKeystream(chapter_key, page_index, payload)
	end

	HTTP.Document.WriteString(payload)

	if not isScrambled or aes4 then return true end

	local puzzle = require 'fmd.imagepuzzle'.Create(grid_size, grid_size)
	local matrix = BuildPermutation(chapter_key, page_index, grid_size)

	for i = 0, grid_size * grid_size - 1 do
		puzzle.Matrix[i] = matrix[i]
	end

	puzzle.DeScramble(HTTP.Document, HTTP.Document)

	return true
end