----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ccc87d4fcc2f4687a177c43f0aca4007'
	m.Name                     = 'MangaDemon'
	m.RootURL                  = 'https://demonicscans.org'
	m.Category                 = 'Webcomics'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/newmangalist.php?list=1'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local next_url, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	while true do
		x.XPathHREFTitleAll('//h2/a', LINKS, NAMES)
		next_url = x.XPathString('(//div[@class="pagination"])[1]/ul/a[last()]/@href')
		if x.XPath('//h2/a').Count == 0 then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('list=(%d+)$') or ''))
		if HTTP.GET(MODULE.RootURL .. next_url) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="border-box"]/@src')
	MANGAINFO.Authors   = x.XPathString('//div[@id="manga-info-stats"]/div[contains(., "Author")]/li[2]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres-list"]/li')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@id="manga-info-stats"]/div[contains(., "Status")]/li[2]'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="white-font"]')

	for v in x.XPath('//div[@id="chapters-list"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('text()', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="imgholder"]/@src', TASK.PageLinks)

	return no_error
end