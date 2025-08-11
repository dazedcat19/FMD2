----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="container mx-auto px-3"]//a[starts-with(@href, "/manga/")]', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="container px-3 mx-auto"]/h1')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="flex justify-center"]/img/@src')
	MANGAINFO.Summary   = x.XPathString('//div[./div="Description"]/div[2]')

	for v in x.XPath('//div[@class="col-span-4 lg:col-span-3"]').Get() do
		local title = x.XPathString('div', v)
		title = title ~= '' and string.format(' - %s', title) or ''

		MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('a/text()', v) .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Accept'] = '*/*'

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="js-pages-container"]//img/@src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M