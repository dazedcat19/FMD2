----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/directory/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local s = 'adult=true'
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.POST(u, s) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="next"]/a[1]/@href'):match('/(%d+)/?')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local s = 'adult=true'
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.POST(u, s) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="list series"]/div/div[@class="title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local s = 'adult=true'
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.POST(u, s) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="thumbnail"]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//div[@class="info"]//b[text()="Author"]/following-sibling::text()[1]'):gsub('^[%s:]*', '')
	MANGAINFO.Artists   = x.XPathString('//div[@class="info"]//b[text()="Artist"]/following-sibling::text()[1]'):gsub('^[%s:]*', '')
	MANGAINFO.Summary   = x.XPathString('//div[@class="info"]//b[text()="Synopsis" or text()="Descripción"]/following-sibling::text()[1]'):gsub('^[%s:]*', '')

	for v in x.XPath('//div[@class="list"]//div[@class="title"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(v.GetAttribute('title'))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local s = 'adult=true'
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.POST(u, s) then return false end

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('//script[contains(., "var pages")]/substring-before(substring-after(., "var pages = "), ";")'))
	x.XPathStringAll('json(*)().url', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M