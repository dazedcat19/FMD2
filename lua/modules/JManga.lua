----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '34782f498b074065a297f7199bf779ca'
	m.Name                     = 'JManga'
	m.RootURL                  = 'https://jmanga.mx'
	m.Category                 = 'Raw'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/filter/'

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
		x.XPathHREFTitleAll('//h3[@class="manga-name"]/a', LINKS, NAMES)
		next_url = x.XPathString('//ul[@class="pagination"]//a[@title="Next"]/@href')
		if next_url == '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('(%d+)') or ''))
		if not HTTP.GET(u .. next_url) then return net_problem end
		x.ParseHTML(HTTP.Document)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="anisc-detail"]/h2')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="manga-poster"]/img/@data-src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="anisc-detail"]//div[@class="item item-title" and ./span[.="著者:"]]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="anisc-detail"]//div[@class="genres"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathStringAll('//div[@class="anisc-detail"]//div[@class="item item-title" and ./span[.="地位:"]]/span[2]'), 'On-hold|Ongoing', 'Canceled|Completed')
	MANGAINFO.Summary   = x.XPathString('//div[@class="anisc-detail"]//div[@class="description"]/normalize-space(.)')

	for v in x.XPath('//div[@class="chapters-list-ul"]//li').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('data-id'))
		MANGAINFO.ChapterNames.Add(x.XPathString('a/@title', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local x = nil
	local u = MODULE.RootURL .. '/json/chapter?mode=vertical&id=' .. URL:match('(%d+)')

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(require 'utils.json'.decode(HTTP.Document.ToString()).html)
	x.XPathStringAll('//img/@data-src', TASK.PageLinks)

	return no_error
end