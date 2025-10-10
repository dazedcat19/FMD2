----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/wp-admin/admin-ajax.php?action=advanced_search'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination
	local s = 'page=' .. (URL + 1)
	HTTP.MimeType = 'application/x-www-form-urlencoded'

	if not HTTP.POST(u, s) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local series = x.XPath('//div[contains(@class, "flex justify-center")]/a')
	if series.Count == 0 then return no_error end

	for v in series.Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('h1', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@itemprop="name"]')
	MANGAINFO.AltTitles = x.XPathString('//div[contains(@class, "text-sm text-text")]')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "wp-post-image")]/@src')
	MANGAINFO.Genres    = x.XPathStringAll('//a[@itemprop="genre"]') .. ', ' .. x.XPathString('(//div[./h4/span="Type"])[last()]/div/p')
	MANGAINFO.Summary   = x.XPathString('//div[@data-show="false" and @itemprop="description"]/string-join(p, "\r\n")')

	local mid = x.XPathString('//div[@id="chapter-list"]/@hx-get/substring-before(substring-after(., "manga_id="), "&")')

	if not HTTP.GET(MODULE.RootURL .. '/wp-admin/admin-ajax.php?action=get_chapters&manga_id=' .. mid) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetProperty('url').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('title').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//section[@data-image-data]/img/@src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M