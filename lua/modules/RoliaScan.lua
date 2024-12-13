----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'eec17a12c6ef426da1b424a192e21edf'
	m.Name                     = 'RoliaScan'
	m.RootURL                  = 'https://roliascan.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//h6/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('(//img[contains(@class, "poster")])[1]/@src')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="d-inline-block"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//tr[(./th="Status")]/td'), 'currently publishing')
	MANGAINFO.Summary   = x.XPathString('//div[(./h5="Synopsis")]/p')

	if not HTTP.GET(u .. 'chapterlist/') then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//a[@class="seenchapter"]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="manga-child-the-content my-5"]/img/@src', TASK.PageLinks)

	return no_error
end