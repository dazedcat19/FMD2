----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'cc9b87e0e2fe4da5b6e8eb7500c3f8c2'
	m.Name                     = 'NicoManga'
	m.RootURL                  = 'https://nicomanga.com'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga-list.html?s=name&st=ASC&p='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. 1) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[contains(@class, "pagination")]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('//script[contains(., "var mangaData")]/substring-before(substring-after(., "mangaData = "), "];")') .. ']')
	for v in x.XPath('json(*)()').Get() do
		LINKS.Add('manga-' .. v.GetProperty('slug').ToString() .. '.html')
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local slug, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//ul/h3')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="thumbnail img-fluid"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//b[contains(., "Author")]/following-sibling::a')
	MANGAINFO.Genres    = x.XPathStringAll('//b[contains(., "Genre")]/following-sibling::a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//b[contains(., "Status")]/following-sibling::a'), 'On going', 'Completed')
	MANGAINFO.Summary   = x.XPathString('//div[@class="summary-content"]')

	slug = x.XPathString('//div/@slug')
	x.ParseHTML(x.XPathString('//script[contains(., "var chapters")]/substring-before(substring-after(., "chapters = "), "];")') .. ']')
	for v in x.XPath('json(*)()').Get() do
		MANGAINFO.ChapterLinks.Add('read-' .. slug .. '-chapter-' .. v.GetProperty('chapter').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('name').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local id = nil
	local u = MaybeFillHost(MODULE.RootURL, URL) .. '.html'

	if not HTTP.GET(u) then return net_problem end

	id = CreateTXQuery(HTTP.Document).XPathString('//*[@id="chapter"]/@value')
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	if HTTP.GET(MODULE.RootURL .. '/app/manga/controllers/cont.imgsList.php?cid=' .. id) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//img/@data-src', TASK.PageLinks)
	end

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end