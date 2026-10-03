----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://hipapi1.s3file.top'
local IMG_HOST = 'https://hip-tx-1.s3imgs.top'
local IMG_HOST_S = 'https://hip-tx-s1.s3imgs.top'
local DirectoryPagination = '/v1/mangas?sort=newest&per_page=100&page='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local PREFIX, SUFFIX = 'qM9', 'Z7'
local MARKER, SEPARATOR = 'Vx', 'pL0'
local BLOCK_SIZE = 7
local SRC_TABLE = '_-9876543210abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
local B64_TABLE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

-- custom-alphabet char -> standard base64 char
local REMAP = {}
for i = 1, #SRC_TABLE do REMAP[SRC_TABLE:sub(i, i)] = B64_TABLE:sub(i, i) end

local function DecodeImagesPayload(raw)
	local crypto = require 'fmd.crypto'

	if raw:sub(1, #PREFIX) ~= PREFIX or raw:sub(-#SUFFIX) ~= SUFFIX then
		return nil
	end

	local body = raw:sub(#PREFIX + 1, -#SUFFIX - 1)
	local content_length = #body - #MARKER - #SEPARATOR
	if content_length <= 0 then return nil end

	local last_len = math.floor(content_length / 3)
	local first_len = math.floor((content_length - last_len) / 2)
	local middle_len = content_length - last_len - first_len

	local marker_end = first_len + #MARKER
	local middle_end = marker_end + middle_len
	local separator_end = middle_end + #SEPARATOR

	if body:sub(first_len + 1, marker_end) ~= MARKER then return nil end
	if body:sub(middle_end + 1, separator_end) ~= SEPARATOR then return nil end

	-- reorder as p5+p1+p3 and flip every odd 7-char block
	local reordered = body:sub(separator_end + 1) .. body:sub(1, first_len) .. body:sub(marker_end + 1, middle_end)
	local buf, block = {}, 0
	for i = 1, #reordered, BLOCK_SIZE do
		local chunk = reordered:sub(i, i + BLOCK_SIZE - 1)
		if block % 2 == 1 then chunk = chunk:reverse() end
		buf[#buf + 1] = chunk
		block = block + 1
	end

	-- remap to standard base64, pad and decode
	local encoded = table.concat(buf)
	if not encoded:match('^[%w%-_]+$') then return nil end
	encoded = encoded:gsub('[%w%-_]', REMAP) .. string.rep('=', (4 - #encoded % 4) % 4)

	return crypto.DecodeBase64(encoded)
end

-- Extract the path list from the decoded JSON array
local function ParseImagePaths(json)
	local paths = {}
	for s in json:gmatch('"([^"]*)"') do
		paths[#paths + 1] = s
	end
	return paths
end

-- Exact decimal-string -> 64-bit integer
local function ToInt(s)
	if not s:match('^-?%d+$') then return nil end
	local n = 0
	for d in s:gmatch('%d') do
		n = n * 10 + (d:byte() - 48)
	end
	return s:sub(1, 1) == '-' and -n or n
end

-- Remove the single decoy entry hidden in the page list
local function RemoveDecoyImage(paths, order_id, sid)
	local size = #paths
	order_id = ToInt(order_id)
	sid = ToInt(sid)
	if size == 0 or order_id == nil or sid == nil then return end

	local x = (sid * 2654435761) ~ (size * 2246822507)
	local offset = x % size
	if x < 0 and offset ~= 0 then
		offset = offset - size
	end
	local decoyIndex = order_id ~ offset
	if decoyIndex >= 0 and decoyIndex < size then
		table.remove(paths, decoyIndex + 1)
	end
end

-- URL-safe base64
local function B64Url(s)
	return (s:gsub('%+', '-'):gsub('/', '_'):gsub('=+$', ''))
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).data.total_pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.items()').Get() do
		LINKS.Add('works/' .. v.GetProperty('mid').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local crypto = require 'fmd.crypto'
	local mid = URL:match('^/works/([^-]+)')
	local u = API_URL .. '/v1/manga?mid=' .. mid

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('json(*).data')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(alt_titles?*, ", ")', info)
	MANGAINFO.CoverLink = 'https://cover.s3imgs.top' .. x.XPathString('vertical_image_url', info)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join(genres?*?name, ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info))
	MANGAINFO.Summary   = x.XPathString('description', info)

	local page  = 1
	local pages = nil
	while true do
		u = API_URL .. '/v1/manga/chapters?mid=' .. mid .. '&page=' .. page .. '&per_page=50&order=asc'

		if not HTTP.GET(u) then return net_problem end

		x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).data.items()').Get() do
			local first, second = v.GetProperty('hid').ToString():match('^(.-)%-(.+)$')
			local hid = B64Url(crypto.EncodeBase64(crypto.DecodeBase64(first):match('%-(.+)$')))
				.. '-' .. B64Url(crypto.EncodeBase64(crypto.DecodeBase64(second)))

			MANGAINFO.ChapterLinks.Add(hid)
			MANGAINFO.ChapterNames.Add(v.GetProperty('title').ToString())
		end
		if not pages then
			pages = tonumber(x.XPathString('json(*).data.total_pages')) or 1
		end
		if page >= pages then break end
		page = page + 1
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/v2/chapter?hid=' .. URL:match('[^/]+$')

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local data = x.XPath('json(*).data')
	local images = DecodeImagesPayload(x.XPathString('images', data))
	if not images then return false end
	local paths = ParseImagePaths(images)
	RemoveDecoyImage(paths, x.XPathString('order_id', data), x.XPathString('sid', data))
	local line = tonumber(x.XPathString('line', data)) or 1
	local host = line == 9 and IMG_HOST_S or IMG_HOST
	for _, p in ipairs(paths) do
		TASK.PageLinks.Add(MaybeFillHost(host, p))
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd514daec688b4f309ddfa4f5cc8eb3bc'
	m.Name                     = '嬉皮漫畫 (HipMH)'
	m.RootURL                  = 'https://m.hipmh.com'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end