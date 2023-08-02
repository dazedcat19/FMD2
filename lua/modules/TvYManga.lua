----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryParameters = '/lista-de-mangas/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="entry-title"]')

	x.XPathHREFAll('//ul[@class="lcp_catlist"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	local x = nil
	local u = MODULE.RootURL .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFAll('//div[contains(@class, "entry-content")]//li//a', LINKS, NAMES)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathStringAll('//div[contains(@class, "entry-content")]//div//img/@src', TASK.PageLinks)

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2020026642ec4cd48a895d81e57f8ef8'
	m.Name                     = 'TvYManga'
	m.RootURL                  = 'https://tvymanga.com'
	m.Category                 = 'Spanish'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end
