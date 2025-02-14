----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPages      = {'/search/?search_type=1&completed_series=YES', '/search/?search_type=1&completed_series=NO'}
DirectoryParameters = '&page=%s.html'
MangaInfoParameters = '/chapters.html'
StatusOngoing       = 'Ongoing'
StatusCompleted     = 'Completed'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local x = nil
	local u = MODULE.RootURL .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. DirectoryParameters:format((URL + 1))

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFAll('//div[@class="manga-part-inner-info"]/a[1]', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//div[@class="page-all-num"]'):match('(%d+)')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL):gsub('(.-/manga/.-)/.-(%.html)', '%1%2'):gsub('(.-/original/.-)/.-(%.html)', '%1%2')

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//td[@class="bookside-general-type"]//div[./span="Alternative(s):"]/text()')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="bookside-img-box"]//img[@itemprop="image"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//td[@class="bookside-general-type"]//div[@itemprop="author"]/a/span')
	MANGAINFO.Artists   = x.XPathStringAll('//td[@class="bookside-general-type"]//div[@itemprop="author"]/a/span')
	MANGAINFO.Genres    = x.XPathStringAll('//td[@class="bookside-general-type"]//span[@itemprop="genre"]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="book-status"]'), StatusOngoing, StatusCompleted)
	MANGAINFO.Summary   = x.XPathString('//section[contains(@class, "detail-synopsis")]/text()[not(a)]')

	u = MANGAINFO.URL:gsub('(.-/manga/.-)/.-', '%1'):gsub('(.-/original/.-)/.-', '%1'):gsub('%.html', '') .. MangaInfoParameters

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('//ul[contains(@class, "chapter-list")]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('li/div/span[@class="chp-title"]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('(//select[@class="sl-page"])[last()]/option/@value', TASK.PageContainerLinks)
	TASK.PageNumber = TASK.PageContainerLinks.Count

	return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function _M.GetImageURL()
	local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID]:gsub('^https?://[^/]+(/.+)$', '%1'))

	if not HTTP.GET(u) then return net_problem end

	TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[contains(@class, "manga_pic")]/@src')

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M