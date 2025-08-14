----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '889f027efe74448883db0f3fecf3d279'
	m.Name                     = 'MangaPill'
	m.RootURL                  = 'https://mangapill.com'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.TotalDirectory           = #DirectoryPages
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPages = {'Action', 'Adventure', 'Cars', 'Comedy', 'Dementia', 'Demons', 'Doujinshi', 'Drama', 'Ecchi', 'Fantasy', 'Game', 'Gender Bender', 'Harem', 'Hentai',
    'Historical', 'Horror', 'Isekai', 'Josei', 'Kids', 'Magic', 'Martial Arts', 'Mecha', 'Military', 'Music', 'Mystery', 'Parody', 'Police', 'Psychological',
    'Romance', 'Samurai', 'School', 'Sci-Fi', 'Seinen', 'Shoujo', 'Shoujo Ai', 'Shounen', 'Shounen Ai', 'Slice of Life', 'Space', 'Sports', 'Super Power',
    'Supernatural', 'Tragedy', 'Thriller', 'Vampire', 'Yaoi', 'Yuri'}

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/search?genre=' .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. '&page=' .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//a[contains(@class, "mb-2")]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('div', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = 20

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="container"]//(div[@class="mb-3"])[1]/h1')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="container"]//(div[@class="mb-3"])[1]/div[last()]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="container"]//img/@data-src')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="container"]//div[@class="mb-3" and ./label="Genres"]/a') .. ', ' .. x.XPathString('//div[@class="container"]//div[./label="Type"]/div'):gsub("^%l", string.upper)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="container"]//div[./label="Status"]/div'), 'publishing', 'finished', 'on hiatus', 'discontinued')
	MANGAINFO.Summary   = x.XPathString('//div[@class="container"]//div[@class="mb-3"]/p')

	x.XPathHREFAll('//div[@id="chapters"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="js-page"]/@data-src', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end