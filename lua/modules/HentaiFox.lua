----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '58a2dec76ebf43a5a9e7dc9b453e52e9'
	m.Name                     = 'HentaiFox'
	m.RootURL                  = 'https://hentaifox.com'
	m.Category                 = 'H-Sites'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetImageURL            = 'GetImageURL'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a'))

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//h2[@class="g_title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local pages, x = nil
	local p = 1
	local u = MaybeFillHost(MODULE.RootURL, URL:gsub('(.*)pag/.*', '%1'):gsub('(.*)popular/.*', '%1'))
	if u:find(MODULE.RootURL .. '/page/') then return net_problem end

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="info"]/h1|//h1[@class="tag_info"]/span')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="cover"]/img/@src|(//div[@class="lc_galleries"]/div[@class="thumb"])[1]//img/@data-src')
	MANGAINFO.Artists   = x.XPathStringAll('//ul[@class="artists"]/li/a/text()')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="info"]/ul[not(@class="artists")]/li/a/text()')

	if u:find('/gallery/') then
		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	else
		pages = tonumber(x.XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1
		while true do
			x.XPathHREFAll('//h2[@class="g_title"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			p = p + 1
			if p > pages then
				break
			elseif HTTP.GET(u .. 'pag/' .. tostring(p)) then
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
 	local u = MaybeFillHost(MODULE.RootURL, URL)
 
 	if not HTTP.GET(u) then return net_problem end
 
 	TASK.PageNumber = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//span[@class="i_text pages"])[1]/substring-after(., ": ")')) or 0
 
 	return no_error
 end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
 	local u = MaybeFillHost(MODULE.RootURL, URL:gsub('/gallery/', '/g/')) .. (WORKID + 1) .. '/'
 
 	if not HTTP.GET(u) then return net_problem end
 
 	TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[@id="gimg"]/@data-src')
 
 	return true
 end