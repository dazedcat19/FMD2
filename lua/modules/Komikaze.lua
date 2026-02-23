----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'a5cad200c65c451eb899e79ddaed9858'
	m.Name                     = 'Komikaze'
	m.RootURL                  = 'https://komikaze.my.id'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/allkomik/latestupdate'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	while true do
		for v in x.XPath('//div[contains(@class, "comic-card")]/a').Get() do
			LINKS.Add(v.GetAttribute('href'))
			NAMES.Add(x.XPathString('.//h3', v))
		end
		local next_url = x.XPathString('//div[@class="pagination"]/a[last()]/@href')
		if x.XPathString('//div[@class="pagination"]/a[last()]/@class'):find('disabled', 1, true) then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('(%d+)') or ''))
		if not HTTP.GET(u .. next_url) then break end
		x.ParseHTML(HTTP.Document)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="comic-title"]')
	MANGAINFO.AltTitles = x.XPathString('//p[@class="alt-title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="comic-cover"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[./span="Author"]/div/span')
	MANGAINFO.Artists   = x.XPathStringAll('//div[./span="Artist"]/div/span')
	MANGAINFO.Genres    = x.XPathStringAll('//div[./span="Genres"]/div/span')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="comic-status"]'))
	MANGAINFO.Summary   = x.XPathString('string-join(//div[@id="synopsis-content"]/text(), "\r\n")')

	while true do
		for v in x.XPath('//ul[@class="chapter-list"]//a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('span[@class="chapter-number"]', v))
		end
		local next_url = x.XPathString('//div[@class="pagination"]/a[last()]/@href')
		if next_url == '' then break end
		if x.XPathString('//div[@class="pagination"]/a[last()]/@class'):find('disabled', 1, true) then break end
		if not HTTP.GET(MANGAINFO.URL .. next_url) then break end
		x.ParseHTML(HTTP.Document)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
	
	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(//script[contains(., "INITIAL_STATE")]/substring-before(substring-after(., "pages"":"), "},"))()', TASK.PageLinks)
	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = MODULE.RootURL .. '/komikaze.php?url=' .. TASK.PageLinks[i]
	end

	return true
end