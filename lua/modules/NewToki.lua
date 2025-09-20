----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '8b8a11bb9b0e4cd8b30c8577c762d19c'
	m.Name                     = 'NewToki'
	m.RootURL                  = 'https://newtoki468.com'
	m.Category                 = 'Raw'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.TotalDirectory           = #DirectoryPages
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPages = {'webtoon', 'comic'}

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local x = nil
	local u = MODULE.RootURL .. '/' .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. '/p' .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFAll('//div[@class="in-lable trans-bg-black"]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//ul[contains(@class, "pagination")]/li[last()]/a/@href'):match('/p(%d+)')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//meta[@name="subject"]/@content')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//meta[@property="og:image"]/@content'))
	MANGAINFO.Authors   = x.XPathString('//meta[@property="og:author"]/@content')
	MANGAINFO.Summary   = x.XPathString('//div[@class="view-title"]//div[@class="col-sm-8"]/div[2]')

	for v in x.XPath('//div[@class="wr-subject"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('text()', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local s, x = nil
	local crypto = require 'fmd.crypto'
	local u = crypto.EncodeURL(MaybeFillHost(MODULE.RootURL, URL))

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	s = x.XPathString('//script[contains(., "var html_data")]')
	x.ParseHTML(crypto.HexToStr(s:match("html_data%+%=\'.*\';"):gsub("[html_data+=.;%'\n]", "")))
	x.XPathStringAll('//img[@src="/img/loading-image.gif"]/@*[contains(name(), "data-")]', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end