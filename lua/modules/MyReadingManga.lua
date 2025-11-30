----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '84451c35b7764bb5a5e3dd8692e84682'
	m.Name                     = 'MyReadingManga'
	m.RootURL                  = 'https://myreadingmanga.info'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[contains(@class, "archive-pagination")]/ul/li[last()-1]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//h2[@class="entry-title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="entry-title"]'):gsub('%s*%(update[^)]*%)$', '')
	MANGAINFO.AltTitles = x.XPathString('//p[@class="alt-title-class"]/following-sibling::p[1]')
	MANGAINFO.Authors   = x.XPathStringAll('//header//span[@class="entry-terms" and ./span="Creator"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('(//header//span[@class="entry-terms" and ./span="Genre"]/a, //header//span[@class="entry-tags"]/a, //header//span[@class="entry-categories"]/a)')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//header//span[@class="entry-terms" and ./span="Status"]/a'))
	MANGAINFO.Summary   = x.XPathString('//p[@class="info-class"]/following-sibling::p/text()')

	local page  = 1
	local pages = tonumber(x.XPathString('//div[@class="chapter-class"]//a[last()-1]')) or 1
	while true do
		if not HTTP.GET(MANGAINFO.URL .. tostring(page)) then return net_problem end
		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL .. page .. '/')
		MANGAINFO.ChapterNames.Add(CreateTXQuery(HTTP.Document).XPathString('//div[@class="entry-content"]/div[contains(@style, "display")]/following-sibling::p/text()'))
		page = page + 1
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="img-myreadingmanga"]/@src', TASK.PageLinks)

	return true
end