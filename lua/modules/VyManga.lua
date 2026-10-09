----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/search?sort=created_at&sort_type=desc&page='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function ChapterStableId(title, occurrence)
	local key = title
	if occurrence > 1 then
		key = string.format('%s\x01%d', title, occurrence)
	end
	local hash = require 'fmd.crypto'.MD5(key)
	hash = hash:gsub('.', function(c) return string.format('%02x', c:byte()) end)
	return hash:lower():sub(-10)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@class="comic-item"]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('div[@class="comic-title"]', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title"]')
	MANGAINFO.AltTitles = x.XPathString('//h1[@class="title"]/following-sibling::p[1]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="img-manga"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//p[span="Authors"]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//p[span="Artists"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//p[span="Genres"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[span="Status"]'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="summary"]/p[@class="content"]')

	local seen = {}
	for v in x.XPath('//div[@class="list-group"]/a').Get() do
		local title = x.XPathString('span[1]', v)
		local occurrence = (seen[title] or 0) + 1
		seen[title] = occurrence
		local id = ChapterStableId(title, occurrence)
		MANGAINFO.ChapterLinks.Add(id)
		MANGAINFO.ChapterNames.Add(title)
		MODULE.Storage[id] = v.GetAttribute('href')
	end

	if MANGAINFO.ChapterLinks.Count == 0 then
		local title = x.XPathString('//div[contains(@class, "div-chapter")]//a/span')
		local id = ChapterStableId(title, 1)
		MANGAINFO.ChapterLinks.Add(id)
		MANGAINFO.ChapterNames.Add(title)
		MODULE.Storage[id] = x.XPathString('//div[contains(@class, "div-chapter")]//a/@href')
	end

	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.Storage[URL:match('[^/]+$')]
	if u == '' then
		if URL:sub(1, 4) == 'http' then
			u = URL
		else
			require 'fmd.logger'.SendWarning('VyManga: chapter URL is not cached, refresh the manga info and try again')
			return false
		end
	end

	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, u)) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[contains(@class, "d-block")]/@data-src', TASK.PageLinks)

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
	m.ID                       = '4f4a3f7ae9c24a83a52fdcfd8b1f5c7d'
	m.Name                     = 'VyManga'
	m.RootURL                  = 'https://mangavyvy.com'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end