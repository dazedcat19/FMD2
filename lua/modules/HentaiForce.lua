----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'f01a84c87ca64c399a161a055f71aa2e'
	m.Name                     = 'HentaiForce'
	m.RootURL                  = 'https://hentaiforce.net'
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

DirectoryPagination = '/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination flex-wrap"]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="gallery" and not(./div[@class="gallery-type-category"])]/div//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[@id="gallery-main-cover"]//img/@data-src')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@id="gallery-main-info"]/div[contains(., "Artists")]/a/text()')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@id="gallery-main-info"]/div[contains(., "Tags") or contains(., "Languages") or contains(., "Category")]/a/text()')

	MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="single-thumb"]//img/@data-src/replace(., "t\\.", ".")', TASK.PageLinks)

	return no_error
end