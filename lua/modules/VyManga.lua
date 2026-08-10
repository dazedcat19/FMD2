----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '4f4a3f7ae9c24a83a52fdcfd8b1f5c7d'
	m.Name                     = 'VyManga'
	m.RootURL                  = 'https://mangavyvy.com'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/search?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. (URL + 1)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPathCount('//div[contains(@class, "comic-item")]//a[.//div[contains(@class, "comic-title")]]') == 0 then return no_error end
	local v for v in x.XPath('//div[contains(@class, "comic-item")]//a[.//div[contains(@class, "comic-title")]]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('.//div[contains(@class, "comic-title")]', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title"]')
	MANGAINFO.AltTitles = x.XPathString('//h1[@class="title"]/following-sibling::p[1]')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "img-manga")]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//p[span[contains(@class, "pre-title") and contains(., "Author")]]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//p[span[contains(@class, "pre-title") and contains(., "Artist")]]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//p[span[contains(@class, "pre-title") and contains(., "Genre")]]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[span[contains(@class, "pre-title") and contains(., "Status")]]/span[not(contains(@class, "pre-title")) and not(contains(@class, "space"))]'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="summary"]/p[@class="content"]')

	local v for v in x.XPath('//div[contains(@class, "list-group")]//a[contains(@href, "aovheroes") or contains(@href, "/rds") or starts-with(@id, "chapter-")]').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('normalize-space(.//span[1])', v))
	end

	-- Single-chapter series don't render the full .list-group list (nor a "New Chapter" box),
	-- only the "First Chapter" quick-nav box, which sits outside .list-group and is missed above.
	-- Fall back to it only when the main list is empty, so multi-chapter series keep no duplicate.
	if MANGAINFO.ChapterLinks.Count == 0 then
		local href = x.XPathString('(//a[contains(., "First Chapter")])[1]/@href')
		if href ~= '' then
			MANGAINFO.ChapterLinks.Add(href)
			local name = Trim(x.XPathString('substring-after((//a[contains(., "First Chapter")])[1], "First Chapter")'))
			if name == '' then name = 'Chapter 1' end
			MANGAINFO.ChapterNames.Add(name)
		end
	end

	MANGAINFO.ChapterLinks.Reverse()
	MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[contains(@class, "d-block")]/@data-src', TASK.PageLinks)

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end
