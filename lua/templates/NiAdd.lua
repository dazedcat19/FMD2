----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

AlphaList = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local i, s
	if MODULE.CurrentDirectoryIndex == 0 then
		s = '0-9'
	else
		i = MODULE.CurrentDirectoryIndex + 1
		s = AlphaList:sub(i, i)
	end
	local u = MODULE.RootURL .. '/category/' .. s .. '_' .. (URL + 1) .. '.html?sort=name'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	x.XPathHREFAll('//div[@class="manga-part-inner-info"]/a[1]', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//script[contains(., "all_pages")] ! substring-before(substring-after(., "all_pages = """), """")')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL):gsub('(.-/manga/.-)/.-(%.html)', '%1%2'):gsub('(.-/original/.-)/.-(%.html)', '%1%2')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//td[@class="bookside-general-type"]//div[./span="' .. XPathTokenAltTitles .. ':"]/text()')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="bookside-img-box"]//img[@itemprop="image"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//td[@class="bookside-general-type"]//div[@itemprop="author" and span="' .. XPathTokenAuthors .. ':"]/a/span')
	MANGAINFO.Artists   = x.XPathStringAll('//td[@class="bookside-general-type"]//div[@itemprop="author" and span="' .. XPathTokenArtists .. ':"]/a/span')
	MANGAINFO.Genres    = x.XPathStringAll('//td[@class="bookside-general-type"]//span[@itemprop="genre"]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="book-status"]'), StatusOngoing, StatusCompleted)
	MANGAINFO.Summary   = x.XPathString('//section[contains(@class, "detail-synopsis")]/text()[not(a)]')

	u = MANGAINFO.URL:gsub('(.-/manga/.-)/.-', '%1'):gsub('(.-/original/.-)/.-', '%1'):gsub('%.html', '') .. '/chapters.html'

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('//ul[contains(@class, "chapter-list")]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('li/div/span[@class="chp-title"]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('(//select[@class="sl-page"])[last()]/option/@value', TASK.PageContainerLinks)
	TASK.PageNumber = TASK.PageContainerLinks.Count

	return true
end

-- Extract/Build/Repair image urls before downloading them.
function _M.GetImageURL()
	local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID]:gsub('^https?://[^/]+', ''))

	if not HTTP.GET(u) then return false end

	TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[contains(@class, "manga_pic")]/@src')

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M