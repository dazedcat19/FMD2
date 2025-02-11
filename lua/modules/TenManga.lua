----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '7acb300c93504802b08657ba374d8bfb'
	m.Name                     = 'TenManga'
	m.RootURL                  = 'https://www.tenmanga.com'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetImageURL            = 'GetImageURL'
	m.TotalDirectory           = AlphaList:len()
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

AlphaList           = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'
DirectoryPagination = '/category/index_'
DirectorySuffix     = '.html'
MangaInfoParameters = '?waring=1'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local s, i, x = nil
	if MODULE.CurrentDirectoryIndex == 0 then
		s = '0'
	else
		i = MODULE.CurrentDirectoryIndex + 1
		s = AlphaList:sub(i, i)
	end
	local u = MODULE.RootURL .. DirectoryPagination .. s .. '_latest_' .. (URL + 1) .. DirectorySuffix

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFTitleAll('//td[@class="book-right-td"]/a[1]', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//div[@class="page-all-count"]'):match('(%d+) pages')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL) .. MangaInfoParameters

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="bk-name"]')
	MANGAINFO.AltTitles = x.XPathString('//div[./div="Alternative:"]/div[@class="attr-val"]')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "bk-cover")]//img/@lazy_url')
	MANGAINFO.Authors   = x.XPathString('//div[./div="Author(s):"]/div[@class="attr-val"]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="bk-info-tag-item"]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="bk-status"]/a'))
	MANGAINFO.Summary   = x.XPathString('//div[contains(@class, "bk-info-summary")]/div')

	for v in x.XPath('//div[@class="chp-item"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('table/tbody/tr/td[@class="chp-idx"]/text()', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@option_name="page_head"]/div/@option_val', TASK.PageContainerLinks)
	TASK.PageNumber = TASK.PageContainerLinks.Count

	return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
	local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])

	if not HTTP.GET(u) then return net_problem end

	TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[contains(@class, "manga_pic")]/@src')

	return true
end