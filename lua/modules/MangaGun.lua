----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '42c61065259d4003940c1930f1ed7c26'
	m.Name                     = 'MangaGun'
	m.RootURL                  = 'https://mangagun.net'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.FMReader'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	Template.GetDirectoryPageNumber()

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	local v, x = nil

	local u = MODULE.RootURL .. '/app/manga/controllers/cont.Listchapter.php?slug=' .. URL:match('-(.-).html')
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'):gsub('.html', ''))
		MANGAINFO.ChapterNames.Add(x.XPathString('li/div[1]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local cid = nil
	local u = MaybeFillHost(MODULE.RootURL, URL) .. '.html'

	if not HTTP.GET(u) then return net_problem end

	cid = CreateTXQuery(HTTP.Document).XPathString('(//input[@id="chapter"])[1]/@value')

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	u = MODULE.RootURL .. '/app/manga/controllers/cont.Showimage.php?cid=' .. cid

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img/@data-original', TASK.PageLinks)

	return no_error
end