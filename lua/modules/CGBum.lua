----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/daftar-komik?sort=newest'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local page = 1
	while true do
		UPDATELIST.UpdateStatusText('Loading page ' .. page)
		x.XPathHREFAll('//h3/a', LINKS, NAMES)
		local next_url = x.XPathString('//nav[contains(@class, "pagination")]/a[@rel="next"]/@href')
		if next_url == '' then break end
		if not HTTP.GET(next_url) then break end
		x.ParseHTML(HTTP.Document)
		page = page + 1
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1'):gsub('Bahasa Indonesia$', '')
	MANGAINFO.AltTitles = x.XPathString('//p[@class="comic-alt-title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "comic-cover")]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//div[span="Author"]/span[2]')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="comic-genres"]/a, //div[span="Tipe"]/span[2])')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="comic-cover-badges-right"]'), 'Ongoing', 'Tamat')
	MANGAINFO.Summary   = x.XPathString('//div[@class="comic-synopsis"]/div')

	x.XPathHREFAll('//div[@class="chapter-grid"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="reader-images"]/div/@data-url', TASK.PageLinks)

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '463da66c3229438e9908e60dc86d71ec'
	m.Name                     = 'CGBum'
	m.RootURL                  = 'https://cgbum.com'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end