----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '9185eb6c49324a849c7d7925a41ef3a3'
	m.Name                     = 'TuMangaOnline'
	m.RootURL                  = 'https://zonatmo.org'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/biblioteca?order_item=release_date&order_dir=desc&title&_pg=1&filter_by=title&author_filter&type&demography&status&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination
	local page = 1290

	if not HTTP.GET(u .. page) then return net_problem end

	local s = CreateTXQuery(HTTP.Document).XPathString('//a[@rel="next"]/@href')

	while s ~= '' do
		page = page + 1
		if not HTTP.GET(u .. page) then return net_problem end
		s = CreateTXQuery(HTTP.Document).XPathString('//a[@rel="next"]/@href')
	end
	PAGENUMBER = page

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@id="library-grid"]/div/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('.//h4', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[contains(@class, "element-title")]/text()')
	MANGAINFO.AltTitles = x.XPathString('//p[@class="element-alternative-title"]/span')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class,"book-thumbnail")]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="staff-card"]//a[contains(@href, "author")]')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@class="staff-card"]//a[contains(@href, "artist")]')
	MANGAINFO.Genres    = x.XPathStringAll('(//a[contains(@class, "badge")], //div[contains(@class, "demography")], concat(upper-case(substring(//h1[contains(@class, "book-type")], 1, 1)), lower-case(substring(//h1[contains(@class, "book-type")], 2))))')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "book-status")]'), 'public', 'final')
	MANGAINFO.Summary   = x.XPathString('//p[@class="element-description"]/text()')

	for v in x.XPath('//ul[@class="list-group list-chapters"]/li').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('.//a[contains(@class, "btn-primary")]/@href', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('.//span[contains(@class, "chapter-number")]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	
	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="reader-img-wrap"]/img/@data-src', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end