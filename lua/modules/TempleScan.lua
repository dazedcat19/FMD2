----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'df435a30cf8a44cb8e684e99d1b84b5d'
	m.Name                     = 'Temple Scan'
	m.RootURL                  = 'https://templetoons.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/comics'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('null,', '}}]}]', x.XPathString('//script[contains(., "allComics")]'):gsub('\\"', '"'):gsub('\\n', '')))
	for v in x.XPath('json(*).allComics()').Get() do
		LINKS.Add('comic/' .. v.GetProperty('series_slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[contains(@class, "text-3xl md:text-5xl")]')
	MANGAINFO.CoverLink = MODULE.RootURL .. x.XPathString('//img[contains(@class, "object-cover w-full h-full")]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//p[contains(., "Author")]/substring-after(., ":")')
	MANGAINFO.Genres    = x.XPathStringAll('//p[contains(@class, "px-3 py-1")]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[contains(., "Status")]/substring-after(., ":")'), 'Ongoing|Hiatus', 'Completed|Dropped')
	MANGAINFO.Summary   = x.XPathString('//p[contains(., "Description")]/following-sibling::p')

	for v in x.XPath('//a[contains(@class, "col-span-full") and not(.//span="PREMIUM")]').Get() do
		MANGAINFO.ChapterLinks.Add('comic/' .. v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div[2]/h1', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "relative w-auto")]/img/@src', TASK.PageLinks)

	return no_error
end