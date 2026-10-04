----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Directory = '/changeMangaList?type=text'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. Directory

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//ul[@class="ms-directory-list manga-list"]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//p[@class="ms-detail-alias"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="ms-detail-cover"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[dt="Penulis"]/dd/a')
	MANGAINFO.Artists   = x.XPathStringAll('//div[dt="Ilustrator"]/dd/a')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="ms-detail-genres"]/a, //div[dt="Format"]/dd)')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[dt="Status"]/dd'))
	MANGAINFO.Summary   = x.XPathString('//section[@class="ms-detail-panel ms-detail-synopsis"]/p')

	for v in x.XPath('//ul[@class="ms-chapter-list"]/li/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('.//strong', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="all"]//img/@src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c6f3fb4e39ed4b3184d2dbf406008717'
	m.Name                     = 'MangaSusuSpace'
	m.RootURL                  = 'https://mangasusu.space'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end