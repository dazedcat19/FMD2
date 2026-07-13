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

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/manga-list.html?pr=new&s=post&st=DESC&p='

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
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//div[./div="Other names"]/div[2]')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "manga-cover-image")]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[./div="Author(s)"]//a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[./div="Genre(s)"]//a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[./div="Status"]//a'), 'On going', 'Completed')
	MANGAINFO.Summary   = x.XPathString('//div[@class="description-text-content"]')

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

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('//script[contains(., "const images")]/substring-before(substring-after(., "const images = "), ";")'):gsub('\\"', '"'):gsub('\\/', '/'))
	x.XPathStringAll('json(*)()', TASK.PageLinks)

	return true
end