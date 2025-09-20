----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fb5ffb6bb1804573a7c76bc645fb66db'
	m.Name                     = 'XlecX'
	m.RootURL                  = 'https://xlecx.one'
	m.Category                 = 'Adult'
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

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@id="pagination"]//a[last()-1]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@id="dle-content"]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('div[1]/h3', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title      = x.XPathString('//meta[@property="og:title"]/@content')
	MANGAINFO.CoverLink  = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Artists    = x.XPathStringAll('//div[contains(., "Artist")]/a')
	MANGAINFO.Genres     = x.XPathStringAll('//div[contains(@class, "tags-list")]/a')

	MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//ul[contains(@class, "testimage")]//a/@href', TASK.PageLinks)

	return true
end