----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'f8d26ca921af4876b7ba84bd7e06fe82'
	m.Name                     = 'NHentai'
	m.RootURL                  = 'https://nhentai.net'
	m.Category                 = 'H-Sites'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local utf8 = require("utf8")
DirectoryPagination = '/?page='

local ext = {
	['p'] = '.png',
	['j'] = '.jpg',
	['g'] = '.gif',
	['w'] = '.webp'
}

local function decode_unicode(str)
	return str:gsub("\\u(%x%x%x%x)", function(hex) return utf8.char(tonumber(hex, 16)) end)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//a[@class="last"]/@href/substring-after(.,"=")'))

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@id="content"]/div[not(contains(@class, "popular"))]/div[@class="gallery"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local p, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL:gsub('(.*)?page.*', '%1'):gsub('(.*)popular.*', '%1'))
	if string.find(u, MODULE.RootURL .. '/?page=', 1, true) then return net_problem end

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title"]|//h1//span[@class="name"]|//input[@type="search"]/@value')
	MANGAINFO.CoverLink = x.XPathString('//div[@id="cover"]//img/@data-src|(//div[@class="container index-container"]//a[@class="cover"])[1]/img/@data-src')
	MANGAINFO.Artists   = x.XPathStringAll('//*[@class="tags"]/a[contains(@href, "artist")]/*[@class="name"]')
	MANGAINFO.Genres    = x.XPathStringAll('//*[@class="tags"]/a[not(contains(@href, "artist") or contains(@href, "search"))]/*[@class="name"]')

	if string.find(u, '/g/', 1, true) then
		MANGAINFO.ChapterLinks.Add(URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	else
		while true do
			for v in x.XPath('//div[@class="gallery"]/a').Get() do
				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(x.XPathString('div', v))
			end
			p = x.XPathString('//a[@class="next"]/@href')
			if (p ~= '') and HTTP.GET(MaybeFillHost(MODULE.RootURL, p)) then
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
	local i, pages, svr, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	svr = x.XPathString('//script[contains(., "media_server")]/substring-before(substring-after(., "media_server: "), ",")')
	x.ParseHTML(decode_unicode(GetBetween('parse("', '");', x.XPathString('//script[contains(., "_gallery")]'))))
	pages = x.XPath('json(*).images.pages()')
	for i = 1, pages.Count do
		TASK.PageLinks.Add('https://i' .. svr .. '.nhentai.net/galleries/' .. x.XPathString('json(*).media_id') .. '/' .. i .. ext[pages.Get(i).GetProperty('t').ToString()])
	end

	return no_error
end