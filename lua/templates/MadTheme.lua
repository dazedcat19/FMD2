----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/newest?page='
UseLegacyApi  = false
UseSlugSearch = false

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="paginator"]/a[last() and not(@type="button")]/@href'):match('page=(%d+)$')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	if UseSlugSearch then
		local u = MODULE.RootURL

		if not HTTP.GET(u .. DirectoryPagination .. 1) then return net_problem end

		local x = CreateTXQuery(HTTP.Document)
		while true do
			x.XPathHREFAll('//div[@class="book-item"]//div[@class="title"]//a', LINKS, NAMES)

			local next_url = x.XPathString('//div[@class="paginator"]/a[@rel="next"]/@href')
			if next_url == '' then break end

			UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('(%d+)') or ''))
			if HTTP.GET(u .. next_url) then
				x.ParseHTML(HTTP.Document)
			else
				break
			end
		end
	else
		local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

		if not HTTP.GET(u) then return net_problem end

		CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="book-item"]//div[@class="title"]//a', LINKS, NAMES)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//h2')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="img-cover"]/img/@data-src')
	MANGAINFO.Authors   = x.XPathStringAll('//p[./strong[contains(., "Authors")]]/a/span')
	MANGAINFO.Genres    = x.XPathStringAll('//p[./strong[contains(., "Genres")]]/a/normalize-space(.)'):gsub(' ,', '')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[./strong[contains(., "Status")]]/a'))
	MANGAINFO.Summary   = x.XPathString('//p[@class="content"]')

	local id = x.XPathString('//script[contains(., "bookId")]/substring-before(substring-after(., "var bookId = "), ";")')
	local slug = x.XPathString('//script[contains(., "bookId")]/substring-before(substring-after(., "var bookSlug = """), """;")')

	local s = UseSlugSearch and slug or id
	local u = MODULE.RootURL .. (UseLegacyApi and '/service/backend/chaplist/?manga_id=' .. id .. '&manga_name=' .. MANGAINFO.Title
		or '/api/manga/' .. s .. '/chapters?source=detail')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//ul[@class="chapter-list"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div/strong', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local body = HTTP.Document.ToString()
	local pages = body:match("var chapImages%s*=%s*['\"]([^'\"]+)['\"]")
	if pages then
		for v in pages:gmatch('[^,]+') do
			TASK.PageLinks.Add(v)
		end
	else
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="chapter-images"]//img/@data-src', TASK.PageLinks)
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M