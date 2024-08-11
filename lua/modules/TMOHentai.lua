----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '499c85a7b2e542609cb564a196dffb40'
	m.Name                     = 'TMOHentai'
	m.RootURL                  = 'https://tmohentai.com'
	m.Category                 = 'H-Sites'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/section/all?view=list&order=publication_date&order-dir=desc&type=all&page=1'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local next_url, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	while true do
		x.XPathHREFAll('//td[@class="text-left"]/a', LINKS, NAMES)
		next_url = x.XPathString('//ul[@class="pagination"]/li[last()]/a/@href')
		if HTTP.Terminated then break end
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

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="panel-heading"]/h3')
	MANGAINFO.CoverLink = x.XPathString('//img[@alt="cover"]/@src')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@class="content-property"]//a[contains(@href, "=artist")]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="content-property"]//a[contains(@href, "genders") or contains(@href, "tag")]')

	MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL):gsub('contents', 'reader') .. '/cascade'

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="content-image lazy"]/@data-original', TASK.PageLinks)

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end