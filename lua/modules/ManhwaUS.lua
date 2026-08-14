----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd41f6b0a2c8e4b7f9a3d5c1e8f2a6b4c'
	m.Name                     = 'ManhwaUS'
	m.RootURL                  = 'https://manhwaus.net'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

-- Full catalog lives at '/webtoons'; use '/genre/manhwa' to restrict to manhwa only.
local DirectoryPagination = '/webtoons'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
-- The site only exposes PREV/NEXT pagination, so take the highest visible
-- "/page/N" link (usually just NEXT). GetNameAndLink keeps advancing past
-- this value until an empty page is reached, mirroring the Madara template.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	local pages = 1
	for v in CreateTXQuery(HTTP.Document).XPath('//a[contains(@href, "/page/")]').Get() do
		local n = tonumber(v.GetAttribute('href'):match('/page/(%d+)'))
		if n and n > pages then pages = n end
	end
	PAGENUMBER = pages

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination
	if (URL + 1) > 1 then u = u .. '/page/' .. (URL + 1) end

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPathCount('//h3/a[contains(@href, "/webtoon/")]') == 0 then return no_error end
	x.XPathHREFAll('//h3/a[contains(@href, "/webtoon/")]', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.AltTitles = Trim(SeparateRight(x.XPathString('//*[strong[contains(., "Alternative Titles")]][1]'), ':'))
	MANGAINFO.Authors   = x.XPathStringAll('//a[contains(@href, "/author/")]')
	MANGAINFO.Artists   = x.XPathStringAll('//a[contains(@href, "/artist/")]')
	MANGAINFO.Genres    = x.XPathStringAll('//*[strong[contains(., "Genres")]][1]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//*[self::li or self::p or self::div][starts-with(normalize-space(.), "Status")][1]'))
	MANGAINFO.Summary   = x.XPathString('//h2[contains(., "Synopsis")]/following-sibling::*[1]')
	if MANGAINFO.Summary == '' then
		MANGAINFO.Summary = x.XPathString('//meta[@property="og:description"]/@content')
	end

	-- Chapter list entries are <li> items; the "First/Latest Chapter" buttons and
	-- related-series blocks are excluded by requiring an <li> parent. Dedupe as a safety net.
	local seen = {}
	for v in x.XPath('//li/a[contains(@href, "/chapter-")]').Get() do
		local href = v.GetAttribute('href')
		if not seen[href] then
			seen[href] = true
			MANGAINFO.ChapterLinks.Add(href)
			MANGAINFO.ChapterNames.Add(x.XPathString('normalize-space(.)', v))
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	-- Chapter images live inside <div class="read-content"> and are served from
	-- the ii###.manhwaus.net CDN. The path segment varies per series ("/online/",
	-- "/chapters/", ...), so select by container instead of by URL path.
	x.XPathStringAll('//div[contains(@class, "read-content")]//img/@data-src', TASK.PageLinks)
	if TASK.PageLinks.Count == 0 then
		x.XPathStringAll('//div[contains(@class, "read-content")]//img/@src', TASK.PageLinks)
	end

	-- Fallback: scrape every ii###.manhwaus.net image URL straight from the raw
	-- HTML, in case the container class changes.
	if TASK.PageLinks.Count == 0 then
		local s = HTTP.Document.ToString():gsub('\\/', '/')
		local seen = {}
		for link in s:gmatch('https?://ii%d+%.[%w%.%-]+/[^"\'%s<>%)]+%.[%w]+') do
			if not seen[link] then
				seen[link] = true
				TASK.PageLinks.Add(link)
			end
		end
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])

	return true
end
