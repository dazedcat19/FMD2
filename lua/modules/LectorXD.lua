----------------------------------------------------------------------------------------------------
-- LectorXD (lectorxd.com) — FMD2 module
-- Spanish manhwa/manga/manhua reader (Astro SSR site).
-- Series: /manhwa/<slug> , /manga/<slug> or /manhua/<slug>  ·  chapter: /<type>/<slug>/leer/<n>
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                    = 'f4cfcaa6ca9b4e25b7cfd4a41bcce99c'
	m.Name                  = 'LectorXD'
	m.RootURL               = 'https://lectorxd.com'
	m.Category              = 'Spanish'
	m.OnGetInfo             = 'GetInfo'
	m.OnGetPageNumber       = 'GetPageNumber'
	m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Series info: title, cover and chapter list
----------------------------------------------------------------------------------------------------

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if not HTTP.GET(MANGAINFO.URL) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)

	-- Title
	MANGAINFO.Title = Trim(x.XPathString('//h1'))
	if MANGAINFO.Title == '' then
		local slug = MANGAINFO.URL:match('/([^/]+)$') or ''
		MANGAINFO.Title = (slug:gsub('%-', ' '))
	end

	-- Cover image
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL,
		x.XPathString('//meta[@property="og:image"]/@content'))

	MANGAINFO.Summary = Trim(x.XPathString('//div[contains(@class,"description")]'))

	-- Extract base path (/<type>/<slug>) and highest chapter number from visible links
	local base   = ''
	local maxCap = 0
	local v = x.XPath('//a[contains(@href, "/leer/")]')
	for i = 1, v.Count do
		local href = v.Get(i).GetAttribute('href')
		local b, n = href:match('(/%w+/[^/]+)/leer/(%d+)')
		if b and base == '' then
			base = b
		end
		if n then
			n = tonumber(n)
			if n > maxCap then maxCap = n end
		end
	end

	-- Generate ALL chapters 1..maxCap (ascending order, covers pagination)
	if base ~= '' and maxCap > 0 then
		for n = 1, maxCap do
			MANGAINFO.ChapterLinks.Add(base .. '/leer/' .. n)
			MANGAINFO.ChapterNames.Add('Capítulo ' .. n)
		end
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Chapter pages (images)
----------------------------------------------------------------------------------------------------

local function collectImages(x, xpath)
	local count = 0
	local v = x.XPath(xpath)
	for i = 1, v.Count do
		local node = v.Get(i)
		local src = node.GetAttribute('data-src')
		if src == '' then src = node.GetAttribute('src') end
		if src:sub(1, 4) == 'http' then
			TASK.PageLinks.Add(src)
			count = count + 1
		end
	end
	return count
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return false end

	local x = CreateTXQuery(HTTP.Document)
	if collectImages(x, '//*[contains(@class, "page-container")]//img') == 0 then
		collectImages(x, '//img[contains(@class, "page-image")]') -- fallback
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- The site requires a Referer header to serve images
----------------------------------------------------------------------------------------------------

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'
	return true
end