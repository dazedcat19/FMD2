----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api-get-v3.mgsearcher.com/api'
local DirectoryPagination = '/manga/page/'
local CDN_URLs = {'https://t40-1-4.g-mh.online', 'https://f40-1-4.g-mh.online'}
USE_API = false

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Seed random number generator once.
math.randomseed(os.time())

-- Get a random CDN URL from the provided list.
local function GetRandomCDN(CDN_URLs)
	local random_index = math.random(1, #CDN_URLs)
	return CDN_URLs[random_index]
end

-- Set the required http header for making a request.
local function SetRequestHeaders()
	HTTP.Reset()
    HTTP.Headers.Values['Origin'] = MODULE.RootURL
    HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'
end

local B64_ALPHA = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
local CUSTOM_ALPHA = '_-9876543210abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
local PREFIX = 'J7r'
local SUFFIX = 'nQ'
local MARKER1 = 'kD'
local MARKER2 = 'W4s'
local CHUNK_SIZE = 7

local CUSTOM_ALPHA_MAP = {}
for i = 1, #CUSTOM_ALPHA do
	CUSTOM_ALPHA_MAP[CUSTOM_ALPHA:sub(i, i)] = i
end

local function ChunkShuffle(s)
	local result = {}
	local chunk_idx = 0
	local i = 1
	while i <= #s do
		local chunk = s:sub(i, math.min(i + CHUNK_SIZE - 1, #s))
		if chunk_idx % 2 == 1 then
			chunk = chunk:reverse()
		end
		result[#result + 1] = chunk
		chunk_idx = chunk_idx + 1
		i = i + CHUNK_SIZE
	end
	return table.concat(result)
end

local function CustomAlphaDecode(s)
	local result = {}
	for i = 1, #s do
		local ch = s:sub(i, i)
		local idx = CUSTOM_ALPHA_MAP[ch]
		if not idx then print('Invalid character: ' .. ch) end
		result[#result + 1] = B64_ALPHA:sub(idx, idx)
	end
	return table.concat(result)
end

local function DecodeImageList(encoded)
	if encoded:sub(1, #PREFIX) ~= PREFIX then print('invalid prefix')	end
	if encoded:sub(-#SUFFIX) ~= SUFFIX then print('invalid suffix') end

	local stripped = encoded:sub(#PREFIX + 1, #encoded - #SUFFIX)

	local remaining = #stripped - #MARKER1 - #MARKER2
	if remaining <= 0 then print('Invalid data length') end

	local num_groups = math.floor(remaining / 3)
	local first_group_len = math.floor((remaining - num_groups) / 2)
	local second_group_len = remaining - num_groups - first_group_len

	local seg1 = stripped:sub(1, first_group_len)
	local seg2 = stripped:sub(first_group_len + 1, first_group_len + #MARKER1)
	local seg3 = stripped:sub(first_group_len + #MARKER1 + 1, first_group_len + #MARKER1 + second_group_len)
	local seg4 = stripped:sub(first_group_len + #MARKER1 + second_group_len + 1, first_group_len + #MARKER1 + second_group_len + #MARKER2)
	local seg5 = stripped:sub(first_group_len + #MARKER1 + second_group_len + #MARKER2 + 1)

	if seg2 ~= MARKER1 then print('Marker1 mismatch') end
	if seg4 ~= MARKER2 then print('Marker2 mismatch') end
	if #seg5 ~= num_groups then print('Payload length mismatch') end

	local combined = seg5 .. seg1 .. seg3
	local shuffled = ChunkShuffle(combined)
	local alpha_decoded = CustomAlphaDecode(shuffled)

	local pad = #alpha_decoded % 4
	if pad ~= 0 then alpha_decoded = alpha_decoded .. ('='):rep(4 - pad) end

	return require 'fmd.crypto'.DecodeBase64(alpha_decoded)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//button[contains(@class, "w-9 h-9 text-small")])[last()]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[contains(@class, "cardlist")]//a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('div/h3', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1/text()')
	MANGAINFO.CoverLink = x.XPathString('(//img[contains(@class, "object-cover")])[1]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[./span="作者：" or ./span="Author:"]/a/span'):gsub(' ,,', ',')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="block text-left mx-auto"]/div[@class="py-1"]/a, //div[./span="類型：" or ./span="Genres:"]/a/span)'):gsub('#', '')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//h1/span'), '連載中|Ongoing', '完結', '休刊', '停止更新')
	MANGAINFO.Summary   = x.XPathString('//p[contains(@class, "line-clamp-4")]/text()')

	local base = USE_API and API_URL .. '/manga/get' or MODULE.RootURL .. '/manga/get'
	local mid  = x.XPathString('//div[@id="mangachapters"]/@data-mid')
	SetRequestHeaders()

	if not HTTP.GET(base .. '?mid=' .. mid .. '&mode=all') then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	if USE_API then
		for v in x.XPath('json(*).data.chapters()').Get() do
			MANGAINFO.ChapterLinks.Add(mid .. '/' .. v.GetProperty('id').ToString())
			MANGAINFO.ChapterNames.Add(v.GetProperty('attributes').GetProperty('title').ToString())
		end
	else
		for v in x.XPath('//div[@id="allchapterlist"]//a').Get() do
			MANGAINFO.ChapterLinks.Add(mid .. '/' .. v.GetAttribute('data-cs'))
			MANGAINFO.ChapterNames.Add(v.GetAttribute('data-ct'))
		end
	end

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local base = USE_API and API_URL .. '/v2/chapter/getinfo' or MODULE.RootURL .. '/chapter/getcontent'
	local mid, cid = URL:match('^/([^/]+)/([^/]+)$')
	local u = base .. '?m=' .. mid .. '&c=' .. cid
	SetRequestHeaders()

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	if USE_API then
		x.ParseHTML(DecodeImageList(x.XPathString('json(*).data.info.images.images')))
		for v in x.XPath('json(*)()').Get() do
			TASK.PageLinks.Add(GetRandomCDN(CDN_URLs) .. v.GetProperty('url').ToString())
		end
	else
		x.XPathStringAll('//div[@id="chapcontent"]/div/img/@data-src', TASK.PageLinks)
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M