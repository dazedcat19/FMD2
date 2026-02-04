----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/wp-json/neoglass/v1/mangas?per_page=100&paged='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).total_pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).items()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="ng-detail-title"]')
	MANGAINFO.AltTitles = x.XPathString('//p[./strong="Alternative:"]/text()')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="ng-detail-cover"]/@style'):match("background%-image:url%('(.-)'%)")
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[./strong="Status:"]/text()'), 'On-going', 'End')
	MANGAINFO.Summary   = x.XPathString('//p[@class="ng-desc"]')

	local genres = x.XPathStringAll('(//div[./strong="Genres:"]/text(), //p[./strong="Type:"]/text())')
	local list = {}
	for genre in genres:gmatch('[^,]+') do
		genre = genre:match('^%s*(.-)%s*$'):gsub('^%l', string.upper)
		list[#list + 1] = genre
	end
	MANGAINFO.Genres = table.concat(list, ', ')

	for v in x.XPath('//li[@class="ng-chapter-item"]').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('div/a/@href', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('div/span', v))
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="ng-chapter-image"]/img/@src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M