----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/manga-list.html?pr=new&s=post&st=DESC&p='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function DecodeChaoticPayload(payload)
	local key = 'NicoMangaX2'
	local keylen = #key
	local out = {}

	local i = 0
	for p = 1, #payload, 3 do
		local b1, b2, b3 = payload:byte(p, p + 2)
		local cp = ((b1 & 0x0F) << 12) | ((b2 & 0x3F) << 6) | (b3 & 0x3F)
		out[#out + 1] = string.char((cp - 19968) ~ key:byte((i % keylen) + 1))
		i = i + 1
	end

	return require 'utils.json'.decode(table.concat(out))
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="custom-pagination"]/a[last()-2]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//a[@class="manga-title"]', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local crypto = require 'fmd.crypto'
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = crypto.DecodeBase64(x.XPathString('//h1/@data-enc'))
	MANGAINFO.AltTitles = x.XPathString('//div[div="Other names"]/div[2]')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "manga-cover-image")]/@src')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[./div="Status"]//a'), 'On going', 'Completed')
	MANGAINFO.Summary   = x.XPathString('//div[@class="description-text-content"]')

	local authors = {}
	local genres = {}

	for v in x.XPath('//div[div="Author(s)"]//a').Get() do
		authors[#authors + 1] = crypto.DecodeBase64(v.ToString())
	end
	MANGAINFO.Authors = table.concat(authors, ', ')

	for v in x.XPath('//div[div="Genre(s)"]//a').Get() do
		genres[#genres + 1] = crypto.DecodeBase64(v.ToString())
	end
	MANGAINFO.Genres = table.concat(genres, ', ')

	for v in x.XPath('//div[@id="chapter-grid"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'):gsub('.html', ''))
		MANGAINFO.ChapterNames.Add(x.XPathString('.//div[@class="chapter-name-grid"]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL) .. '.html'

	if not HTTP.GET(u) then return false end

	local html = HTTP.Document.ToString()
	local ref = html:match('chaotic_payload\\":\\"%$(%d+)\\"')
	local payload = html:match(ref .. ':T%x+,%"%]%)</script><script>self%.__next_f%.push%(%[1,%"([^"]+)')
	if not payload then
		return false
	end

	local images = DecodeChaoticPayload(payload).images

	for _, v in ipairs(images) do
		TASK.PageLinks.Add(v)
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'cc9b87e0e2fe4da5b6e8eb7500c3f8c2'
	m.Name                     = 'NicoManga'
	m.RootURL                  = 'https://nicomanga.com'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end