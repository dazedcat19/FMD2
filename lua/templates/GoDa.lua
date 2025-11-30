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
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "object-cover")]/@src')
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

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local base = USE_API and API_URL .. '/chapter/getinfo' or MODULE.RootURL .. '/chapter/getcontent'
	local mid, cid = URL:match('^/([^/]+)/([^/]+)$')
	local u = base .. '?m=' .. mid .. '&c=' .. cid
	SetRequestHeaders()

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	if USE_API then
		for v in x.XPath('json(*).data.info.images.images()').Get() do
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