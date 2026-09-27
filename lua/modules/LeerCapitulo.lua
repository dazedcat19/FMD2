----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/manga/?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[contains(@class, "pagination")]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//a[@class="lc-card-name d-block"]', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//h1/following-sibling::p[1]'):gsub(' · ', ', ')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="lc-cover-lg"]/img/@src'))
	MANGAINFO.Authors   = x.XPathString('//li[span="Autor"]/span[2]')
	MANGAINFO.Artists   = x.XPathString('//li[span="Dibujo"]/span[2]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="col-7 col-md-9"]//a[contains(@href, "?genre") or contains(@href, "?theme")]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//li[span="Estado"]/a'), 'Ongoing', 'Completed', 'Paused', 'Cancelled')
	MANGAINFO.Summary   = x.XPathString('//section[@id="sinopsis"]//p[@class="mb-0"]')

	for v in x.XPath('//div[@id="chapterList"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[@class="n"]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//main[@id="lcPages"]//img/@data-src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c67d163c51b24bc498e777e2b0d810d2'
	m.Name                     = 'LeerCapitulo'
	m.RootURL                  = 'https://www.leercapitulo.co'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end