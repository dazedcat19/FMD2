----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'b8206e754d4541689c1d367f7e19fd64'
	m.Name                     = 'KomikCast'
	m.RootURL                  = 'https://komikcast01.com'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/daftar-komik/?list'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@class="list-update"]//ul//a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('normalize-space(.)', v):gsub('Bahasa Indonesia$', ''))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="komik_info-content-body-title"]'):gsub('Bahasa Indonesia$', '')
	MANGAINFO.CoverLink = x.XPathString('//div[@itemprop="image"]//img/@src')
	MANGAINFO.Authors   = x.XPathString('//span[@class="komik_info-content-info" and contains(b, "Author")]/text()')
	MANGAINFO.Genres    = x.XPathStringAll('//span[@class="komik_info-content-genre"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="komik_info-content-info" and contains(b, "Status")]/text()'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="komik_info-description-sinopsis"]/p')

	for v in x.XPath('//div[@class="komik_info-chapters"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('normalize-space(.)', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="main-reading-area"]/img/@src', TASK.PageLinks)

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end