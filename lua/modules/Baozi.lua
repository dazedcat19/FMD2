----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'cce9b395e95c459ea96a03ac9c83c275'
	m.Name                     = '包子漫画 (Baozi)'
	m.RootURL                  = 'https://www.baozimh.com'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/bzmhq/amp_comic_list?limit=50&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination
	local page = 1500

	if not HTTP.GET(u .. page) then return net_problem end

	local s = CreateTXQuery(HTTP.Document).XPathString('json(*).next')

	while s ~= '' do
		page = page + 1
		if not HTTP.GET(u .. page) then return net_problem end
		s = CreateTXQuery(HTTP.Document).XPathString('json(*).next')
	end
	PAGENUMBER = page

	return no_error
end



-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).items()').Get() do
		LINKS.Add(v.GetProperty('comic_id').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//meta[@name="og:novel:book_name"]/@content')
	MANGAINFO.CoverLink = x.XPathString('//meta[@name="og:image"]/@content')
	MANGAINFO.Authors   = x.XPathString('//meta[@name="og:novel:author"]/@content')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//meta[@name="og:novel:status"]/@content'), '連載中|连载中', '已完結|已完结')
	MANGAINFO.Summary   = x.XPathString('//p[contains(@class, "comics-detail__desc")]')

	for v in x.XPath('//div[@id="chapter-items" or @id="chapters_other_list"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div/span', v))
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	while u do
		if not HTTP.GET(u) then return false end

		local x = CreateTXQuery(HTTP.Document)
		x.XPathStringAll('//ul[@class="comic-contain"]//amp-img/@src', TASK.PageLinks)

		local next_chapter = x.XPathString('//a[@id="next-chapter" and span="下一頁" or span="下一页"]/@href')
		u = next_chapter ~= '' and MaybeFillHost(MODULE.RootURL, next_chapter) or nil
	end

	return true
end