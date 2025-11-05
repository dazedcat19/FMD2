----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2ad2886d5617444a9933a971e11abccd'
	m.Name                     = 'MangaTaro'
	m.RootURL                  = 'https://mangataro.org'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/wp-json/manga/v1/load'
	local s = '{"page":"'.. (URL + 1) .. '","sort":"release_desc"}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	local series = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*)()')
	if series.Count == 0 then return no_error end

	for v in series.Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[contains(@class, "flex-1 text-center")]/h1')
	MANGAINFO.AltTitles = x.XPathString('//div[contains(@class, "flex-1 text-center")]/p')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@alt, "Cover background")]/@src')
	MANGAINFO.Authors   = x.XPathString('//div[contains(@class, "flex-1 text-center")]//div[./div="Author & Artist"]/div[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[contains(@class, "flex-1 text-center")]//a[contains(@href, "/tag/")]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[contains(@class, "flex-1 text-center")]//span[contains(@class, "capitalize")]/text()'))
	MANGAINFO.Summary   = x.XPathString('//div[@id="description-content-tab"]/string-join(p, "\r\n")')

	for v in x.XPath('//div[contains(@class, "chapter-list")]/a').Get() do
		local chapter = x.XPathString('(.//span[contains(@class, "text-neutral-100")])[2]', v)
		local title   = x.XPathString('(.//p[contains(@class, "text-neutral-300")])[1]', v)

		title = (title ~= 'N/A' and title ~= '—' and title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(chapter .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@id="reader-media"]//img').Get() do
		local src = v.GetAttribute('src')
		if src:find('base64', 1, true) then
			src = v.GetAttribute('data-src')
		end
		TASK.PageLinks.Add(src)
	end

	return true
end