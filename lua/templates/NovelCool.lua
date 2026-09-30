----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

AlphaList = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http headers for making a request.
local function SetRequestHeaders()
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
end

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
	x.XPathHREFTitleAll('//div[@class="book-info"]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//div[@class="page-nav-center-num hidden-pm"]'):match('/(%d+)')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="bookinfo-title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="bk-intro"]//img[@class="bookinfo-pic-img"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="bk-intro"]//div[@class="bookinfo-author"]/a/span')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="bookinfo-category-list"])[1]//span')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[contains(@class, "bk-cate-type1")]/a'), StatusOngoing, StatusCompleted)
	MANGAINFO.Summary   = x.XPathString('(//div[@class="bk-summary-txt"])[1]')

	x.XPathHREFTitleAll('//div[@class="chp-item"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	if URL:find('/rds/', 1, true) then
		local u = 'https://workexplained.com' .. URL

		SetRequestHeaders()
		if not HTTP.GET(u) then return false end
	
		u = HTTP.Document.ToString():match('document%.location%s*=%s*["\'](.-)["\']')

		SetRequestHeaders()
		if not HTTP.GET(u) then return false end

		CreateTXQuery(HTTP.Document).XPathStringAll('json("[" || //script[contains(., "all_imgs_url")] ! substring-before(substring-after(., "all_imgs_url: ["), "]") || "]")()', TASK.PageLinks)
	else
		local u = MaybeFillHost(MODULE.RootURL, URL)

		if not HTTP.GET(u) then return false end

		CreateTXQuery(HTTP.Document).XPathStringAll('(//select[@class="sl-page"])[last()]/option/@value', TASK.PageContainerLinks)
		TASK.PageNumber = TASK.PageContainerLinks.Count
	end

	return true
end

-- Extract/Build/Repair image urls before downloading them.
function _M.GetImageURL()
	local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])

	if not HTTP.GET(u) then return net_problem end

	TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('(//img[contains(@class, "manga_pic")])[1]/@src')

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
