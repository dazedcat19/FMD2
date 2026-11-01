----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local DirectoryPages = '/bbs/board.php?bo_table=toon_c&sord=&type=upd&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPages

	if not HTTP.GET(u) then return false end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//a[contains(@class, "pg_end")] ! substring-after(@href, "&page=")')) or 1

	return true
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPages .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('//ul[@id="free-genre-list"]//li[@data-id]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(v.GetAttribute('data-ga-event-label'))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h2[@class="title"]')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="banner"]/@src')
	MANGAINFO.Authors   = x.XPathString('//span[contains(., "작가")]/following-sibling::span')
	MANGAINFO.Genres    = x.XPathString('//span[contains(., "장르")]/following-sibling::span')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(., "분류")] ! following-sibling::span'), '주간|월간|연재|격주', '완결')
	MANGAINFO.Summary   = x.XPathString('//span[contains(., "소개")]/following-sibling::span')

	local page = 1
	local pages = tonumber(x.XPathString('//a[contains(@class, "pg_end")] ! substring-after(@href, "&page=")')) or 1
	while page <= pages do
		for v in x.XPath('//ul[@class="episode-list"]//li/button').Get() do
			MANGAINFO.ChapterLinks.Add('/bbs' .. v.GetAttribute('onclick'):match("%.(/[^']*)"))
			MANGAINFO.ChapterNames.Add(x.XPathString('.//div[@class="episode-title ellipsis"]', v))
		end
		page = page + 1
		if not HTTP.GET(MANGAINFO.URL .. '&sord=&type=&page=' .. page) then break end
		x.ParseHTML(HTTP.Document)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(//script[contains(., "img_list")] ! substring-before(substring-after(., "var img_list ="), ";"))()', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c7b742a4dc8a451fa95baec1aa183b39'
	m.Name                     = '11Toon'
	m.RootURL                  = 'https://www.11toon.com'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

