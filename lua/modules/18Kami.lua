----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '73bedd818a034fcfb50144ff26298368'
	m.Name                     = '18Kami'
	m.RootURL                  = 'https://18kami.com'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/albums?o=mr&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@class="img_above"]').Get() do
		LINKS.Add(x.XPathString('div[4]/a/@href', v))
		NAMES.Add(x.XPathString('span', v))
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('(//h1)[1]')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('(//div[@class="show_zoom"])[1]/img/@src'))
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="p-t-5 p-b-5" and contains(., "Author")]//a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@id="albumTagBox_m"]/a')
	MANGAINFO.Summary   = x.XPathString('(//div[@class="p-t-5 p-b-5" and contains(., "description")])[1]/substring-after(., "：")')

	for v in x.XPath('//div[@class="episode"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('li/text()', v))
	end
	if MANGAINFO.ChapterLinks.Count == 0 then
		MANGAINFO.ChapterLinks.Add(x.XPathString('(//div[@class="p-t-5 p-b-5 read-block"])[1]/a/@href'))
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local v = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('//select[@id="pageselect"]/option').Get() do
		TASK.PageLinks.Add(MODULE.RootURL .. '/media/photos/' .. URL:match('photo/(%d+)') .. '/' .. v.GetAttribute('data-page'))
	end

	return no_error
end