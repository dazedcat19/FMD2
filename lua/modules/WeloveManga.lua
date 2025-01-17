----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '437660e89f824183901cf05c24e35eae'
	m.Name                     = 'WeloveManga'
	m.RootURL                  = 'https://welovemanga.one'
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

	local mid = HTTP.Document.ToString():match('var mIds = "(.-)";')
	local u = MODULE.RootURL .. '/app/manga/controllers/cont.Listchapter.php?mid=' .. mid
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
	local id = nil
	local u = MaybeFillHost(MODULE.RootURL, URL) .. '.html'

	if not HTTP.GET(u) then return net_problem end

	id = CreateTXQuery(HTTP.Document).XPathString('(//input[@id="chapter"])[1]/@value')

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	u = MODULE.RootURL .. '/app/manga/controllers/cont.listImg.php?cid=' .. id

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img/@data-srcset', TASK.PageLinks)

	return no_error
end