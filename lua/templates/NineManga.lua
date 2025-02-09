----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/search/?name_sel=contain&wd=&author_sel=contain&author=&artist_sel=contain&artist=&category_id=&out_category_id=&completed_series=either&page=1.html'
MangaInfoParameters = '?waring=1'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local next_url, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	while true do
		x.XPathHREFAll('//dl[@class="bookinfo"]/dd/a[@class="bookname"]', LINKS, NAMES)
		next_url = x.XPathString('(//div[@class="page"])[1]//li[last()]/a[not(@class="selected")]/@href')
		if next_url == '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('page=(%d+)') or ''))
		if HTTP.GET(next_url) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL) .. MangaInfoParameters
	if not u:find(MangaInfoParameters) then u = u .. MangaInfoParameters end

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@itemprop="name"]'):gsub('^(.*) Manga$', '%1')
	MANGAINFO.CoverLink = x.XPathString('//a[@class="bookface"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//ul[@class="message"]//a[@itemprop="author"]')
	MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="message"]/li[@itemprop="genre"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="message"]/li[contains(b, "Status")]/a[1]'))
	MANGAINFO.Summary   = Trim(x.XPathString('//p[@itemprop="description"]/substring-after(., ":")'))

	x.XPathHREFAll('//div[@class="chapterbox"]//li/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('(//select[@id="page"])[last()]/option/@value', TASK.PageContainerLinks)
	TASK.PageNumber = TASK.PageContainerLinks.Count

	return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function _M.GetImageURL()
	local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])

	if not HTTP.GET(u) then return net_problem end

	TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[contains(@class, "manga_pic")]/@src')

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M