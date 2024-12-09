----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '4ae5a894bea542aeb847c774335e7ecd'
	m.Name                     = 'ManyToonOrg'
	m.RootURL                  = 'https://manytoon.org'
	m.Category                 = 'H-Sites'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/azlist'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local next_url, last_url, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	while true do
		x.XPathHREFTitleAll('//div[@class="info"]/a', LINKS, NAMES)
		next_url = MaybeFillHost(MODULE.RootURL, x.XPathString('//ul[@class="pagination"]/li[last()]/a/@href'))
		last_url = x.XPathString('//ul[@class="pagination"]/li[last()]/a/@tabindex')
		if last_url ~= '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('(%d+)') or ''))
		if HTTP.GET(next_url) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//meta[@name="og:image"]/@content')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres mt-15"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[contains(., "Status")]/span'))
	MANGAINFO.Summary   = x.XPathString('//meta[@name="og:description"]/@content')

	for v in x.XPath('//div[@class="chapter-list mt-15"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div[@class="chapter-name"]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(//script[contains(., "read_image_list")]/substring-before(substring-after(., "= "), ";"))()', TASK.PageLinks)

	return no_error
end