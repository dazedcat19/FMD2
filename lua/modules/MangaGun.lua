----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '42c61065259d4003940c1930f1ed7c26'
	m.Name                     = 'NihonKuni'
	m.RootURL                  = 'https://nihonkuni.com'
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

	local u = MODULE.RootURL .. '/app/manga/controllers/cont.Listchapter.php?slug=' .. URL:match('-(.-).html')

	MANGAINFO.CoverLink = CreateTXQuery(HTTP.Document).XPathString('//img[contains(@class, "thumbnail")]/@data-original')

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'):gsub('.html', ''))
		MANGAINFO.ChapterNames.Add(x.XPathString('li/div[1]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL) .. '.html'
	HTTP.Reset()
	HTTP.Cookies.Values['smartlink_shown'] = 1

	if not HTTP.GET(u) then return net_problem end

	local s = HTTP.Document.ToString():gsub('\\/', '/'):gsub('\\n', ''):gsub('\\r', '')
	local x = CreateTXQuery(s)
	x.ParseHTML(x.XPathString('//script[contains(., "window.chapterImages")]/substring-before(substring-after(., "window.chapterImages="""), """")'))
	x.XPathStringAll('//img/@data-srcset', TASK.PageLinks)

	return no_error
end