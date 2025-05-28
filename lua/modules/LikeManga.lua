----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c7aebe73845f43149bd5a8cbe84fd926'
	m.Name                     = 'Like Manga'
	m.RootURL                  = 'https://likemanga.ink'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/?act=search&f[status]=all&f[sortby]=lastest-manga&&pageNum='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[contains(@class, "pagination")]/li[last()]/a/@href'):match('=(%d+)#')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//p[@class="card-text title-manga"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local id, pages, v, x = nil
	local page = 1
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title-detail"]')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[contains(@class, "col-lg-4")]/img/@src'))
	MANGAINFO.Authors   = x.XPathString('//li[@class="author row"]/p[2]')
	MANGAINFO.Genres    = x.XPathStringAll('//li[@class="kind row"]/p[2]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//li[@class="status row"]/p[2]'), 'In process', 'Complete')
	MANGAINFO.Summary   = x.XPathString('//div[@id="summary_shortened"]')

	id = x.XPathString('//h1[@class="title-detail"]/@data-manga')
	pages = tonumber(x.XPathString('//div[@class="chapters_pagination mt-2"]//li[last()-1]/a')) or 1
	while true do
		if HTTP.GET(MODULE.RootURL .. '/?act=ajax&code=load_list_chapter&manga_id=' .. id .. '&page_num=' .. tostring(page) .. '&chap_id=0&keyword=') then
			x = CreateTXQuery(HTTP.Document)
			for v in x.XPath('//li[contains(@class, "wp-manga-chapter")]/a').Get() do
				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'):gsub('\\"', ''):gsub('\\/', '/'))
				MANGAINFO.ChapterNames.Add(x.XPathString('text()', v))
			end
		else
			break
		end
		page = page + 1
		if page > pages then
			break
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "page-chapter")]/img/@src', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end