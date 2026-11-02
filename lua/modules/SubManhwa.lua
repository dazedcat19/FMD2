----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local DirectoryPages = '/biblioteca?sort=new&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPages .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//nav[@class="lib-pages"]/a[last()-1]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPages .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="lib-grid"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="sr-alt"]')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="sr-cover"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//dt[text()="Autor(es)"]/following-sibling::dd[1]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//dt[text()="Artist(s)"]/following-sibling::dd[1]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="sr-badges"]/span[2]'), 'En Curso', 'Completa')
	MANGAINFO.Summary   = x.XPathString('//div[@class="sr-summary"]/p')

	local genres = {}
	local seen = {}
	for genre in x.XPath('(//dt[text()="Categorías"]/following-sibling::dd[1]/a|//div[@class="sr-tags"]/a)').Get() do
		local key = genre.ToString()
		if not seen[key] then
			table.insert(genres, key)
			seen[key] = true
		end
	end
	MANGAINFO.Genres = table.concat(genres, ', ')

	local mid = x.XPathString('//input[@name="post_id"]/@value')
	local page = 0
	local pages = (math.ceil(x.XPathString('//h2[@class="lc-tit"]/small') / 200) - 1) or 0
	while true do
		u = MODULE.RootURL .. '/lista-capitulos/serie/' .. mid .. '?orden=asc&limit=200&offset=' .. (page * 200)
		if not HTTP.GET(u) then return net_problem end
		x = CreateTXQuery(require 'utils.json'.decode(HTTP.Document.ToString()).html)
		x.XPathHREFAll('//div[@class="lc-info"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		if page >= pages then break end
		page = page + 1
	end

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="all"]/img/(@data-src|@src)[not(contains(., "data:"))]', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '00001673c7b1c4f4ba22be78fd599a2d0'
	m.Name                     = 'SubManhwa'
	m.RootURL                  = 'https://submanhwa.com'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end