----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/series'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@id="searched_series_page"]/button/div/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "photoURL")]/@style'):match('--photoURL:url%((.-)%)')
	MANGAINFO.Authors   = x.XPathString('//div[@alt="Author"]/span|//div[@class="grid gap-2 h-fit" and contains(., "Author")]/div[2]')
	MANGAINFO.Artists   = x.XPathString('//div[@alt="Artist"]/span|//div[@class="grid gap-2 h-fit" and contains(., "Artist")]/div[2]')
	MANGAINFO.Genres    = x.XPathStringAll('//a[contains(@href, "genre")]/span')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@alt="Status"]/span|//div[@class="grid gap-2 h-fit" and contains(., "Status")]/div[2]'))
	MANGAINFO.Summary   = x.XPathString('string-join(//div[@id="expand_content"]/p/text()|//div[@class="overflow-hidden"]/p/text(), "\r\n")')

	if MODULE.GetOption('showpaidchapters') then
		x.XPathHREFTitleAll('//div[@id="chapters"]/a[not(.//span="Upcoming")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	else
		x.XPathHREFTitleAll('//div[@id="chapters"]/a[not(.//img[@alt="Coin"]) and not(.//span="Upcoming")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local CDN_URL, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	CDN_URL = x.XPathString('//script[contains(., "realUrl")]/substring-before(substring-after(., "realUrl = `"), "${uid}")')
	for v in x.XPath('//div[@id="pages"]/img/@uid').Get() do
		TASK.PageLinks.Add(CDN_URL .. v.ToString())
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M