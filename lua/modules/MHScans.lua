----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local DirectoryPages = '/library?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPages .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('substring-after(//p[@class="lib-head-count"], " / ")')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPages .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@class="lib-grid"]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('.//p[contains(@class, "leading-snug")]', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('(//div[contains(@class, "comic-cover")])[1]/img/@src'))
	MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="comic-chips"]/li')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "comic-meta__status")]'), 'en emisión', 'completado', 'en hiato', 'cancel')
	MANGAINFO.Summary   = x.XPathString('//p[@id="comic-description"]')

	local page = 1
	local pages = math.ceil(x.XPathString('//button[@id="chapterlist-tab-chapters"]/span'):match('%d+') / x.XPathCount('//a[@data-chapter-label]')) or 1
	while true do
		for v in x.XPath('//a[@data-chapter-label and not(contains(@href, "/auth/"))]').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(v.GetAttribute('data-chapter-label'))
		end
		if page >= pages then break end
		page = page + 1
		if not HTTP.GET(MANGAINFO.URL .. '/chapters?page=' .. page) then break end
		x.ParseHTML(HTTP.Document)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "page-img-wrap")]/img/(@data-src|@src)', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'a29b6757649f4683b72d2e669d9fbffc'
	m.Name                     = 'MHScans'
	m.RootURL                  = 'https://mhscans.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end