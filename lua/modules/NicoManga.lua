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
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.FMReader'
DirectoryPagination = '/manga-list.html?s=name&st=ASC&p='

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

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	local v, x = nil

	local u = MODULE.RootURL .. '/app/manga/controllers/cont.Listchapterapi.php?slug=' .. URL:match('-(.-).html')
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'):gsub('.html', ''))
		MANGAINFO.ChapterNames.Add(x.XPathString('li/div[1]/span[1]', v))
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
	u = MODULE.RootURL .. '/app/manga/controllers/cont.imgsList.php?cid=' .. id

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img/@data-src', TASK.PageLinks)

	return no_error
end