----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '18470a3b19034f3f91289c2b8b7d3ab3'
	m.Name                     = 'Pururin'
	m.RootURL                  = 'https://pururin.me'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/browse?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination mb-2 flex-wrap"]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="row-gallery"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local pages, v, x = nil
	local p = 1
	local u = MaybeFillHost(MODULE.RootURL, URL:gsub('(.*)?sort=.*', '%1'))

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="cover-wrapper"]//img/@src|//div[@class="row-gallery"]/a[1]/img/@src')
	MANGAINFO.Artists   = x.XPathStringAll('//table[@class="table table-info"]//tr[contains(td, "Artist")]//a')
	MANGAINFO.Genres    = x.XPathStringAll('//table[@class="table table-info"]//tr[(./td="Circle") or (./td="Parody") or contains(td, "Character") or contains(td, "Content") or (./td="Language") or (./td="Convention") or (./td="Category")]//a')
	MANGAINFO.Summary   = x.XPathString('string-join(//div[@class="box box-header"]/p/text(), "\r\n")')

	MANGAINFO.ChapterLinks.Add(x.XPathString('//div[@class="gallery-action"]/a[1]/@href'))
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	if string.find(u, '/tags/', 1, true) then
		pages = tonumber(x.XPathString('//ul[@class="pagination mb-2 flex-wrap"]/li[last()-1]/a')) or 1
		while true do
			for v in x.XPath('//div[@class="row-gallery"]/a').Get() do
				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'):gsub('/gallery/', '/read/'):gsub('read/%d/', 'read/%d/01/'))
				MANGAINFO.ChapterNames.Add(v.GetAttribute('title'))
			end
			p = p + 1
			if p > pages then
				break
			elseif HTTP.GET(MANGAINFO.URL .. '?page=' .. tostring(p)) then
				x.ParseHTML(HTTP.Document)
			else
				break
			end
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local dir, i, svr, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	svr = x.XPathString('//div[@class="img-viewer"]/@data-svr') .. '/'
	dir = x.XPathString('json(//div[@class="img-viewer"]/@data-img).directory') .. '/'
	for i in x.XPath('json(//div[@class="img-viewer"]/@data-img).images().filename').Get() do
		TASK.PageLinks.Add(svr .. dir .. i.ToString())
	end

	return no_error
end