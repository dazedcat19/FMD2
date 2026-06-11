----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                        = 'f4cfcaa6ca9b4e25b7cfd4a41bcce99c'
	m.Name                      = 'LectorXD'
	m.RootURL                   = 'https://lectorxd.com'
	m.Category                  = 'Spanish'
	m.OnGetDirectoryPageNumber  = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink          = 'GetNameAndLink'
	m.OnGetInfo                 = 'GetInfo'
	m.OnGetPageNumber           = 'GetPageNumber'
	m.OnBeforeDownloadImage     = 'BeforeDownloadImage'
	m.SortedList                = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/catalogo?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. '1'
	if not HTTP.GET(u) then return net_problem end
	local x = CreateTXQuery(HTTP.Document)
	local s = x.XPathString('//nav//a[contains(@href, "page=")][last()]/@href')
	local p = s:match('page=(%d+)')
	PAGENUMBER = tonumber(p) or 1
	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)
	if not HTTP.GET(u) then return net_problem end
	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[contains(@class, "manga-grid")]//a[contains(@class, "flex")]').Get() do
		local link = v.GetAttribute('href')
		local name = x.XPathString('.//h4', v)
		if link ~= '' and name ~= '' then
			LINKS.Add(link)
			NAMES.Add(name)
		end
	end
	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if not HTTP.GET(MANGAINFO.URL) then return net_problem end
	local x = CreateTXQuery(HTTP.Document)
	local s = HTTP.Document.ToString()

	MANGAINFO.Title     = Trim(x.XPathString('//h1'))
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Summary   = Trim(x.XPathString('//div[contains(@class,"prose")]//p'))
	MANGAINFO.Genres    = x.XPathStringAll('//a[contains(@href, "catalogo?tags=")]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(s, 'en_emision', 'completado')

	-- Extract base path (e.g. /manga/kaoru-hana-wa-rin-to-saku)
	local base = MANGAINFO.URL:match('https?://[^/]+(/.+)$') or ''

	-- Parse chapters from embedded chaptersList JS array (supports decimal numbers).
	-- Uses a set to skip duplicates (the same data also appears in Astro component props).
	local seen = {}
	for ch in s:gmatch('"chapter":"([^"]+)"') do
		if not seen[ch] then
			seen[ch] = true
			MANGAINFO.ChapterLinks.Add(base .. '/leer/' .. ch)
			MANGAINFO.ChapterNames.Add('Capítulo ' .. ch)
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not HTTP.GET(u) then return false end
	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "page-container")]/img/@data-src', TASK.PageLinks)
	if TASK.PageLinks.Count == 0 then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "page-container")]/img/@src', TASK.PageLinks)
	end
	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'
	return true
end
