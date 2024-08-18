----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
local m = NewWebsiteModule()
	m.ID                       = '41294a121062494489rwra601c542efg'
	m.Name                     = 'ManwhasOnline'
	m.RootURL                  = 'https://manwhasonline.com'
	m.Category                 = 'H-Sites'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaEsp'
DirectoryPagination = '/doujin?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	if x.XPath('//figure[@class="relative overflow-hidden rounded-lg shadow-md"]/a').Count == 0 then return no_error end
	for v in x.XPath('//figure[@class="relative overflow-hidden rounded-lg shadow-md"]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('../div/div/h3', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "max-w-80 w-full")]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//div[(./i[@class="i-heroicons-user-20-solid text-green-400"])]/div')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="flex flex-wrap"]/a')
	MANGAINFO.Summary   = x.XPathString('//p[@class="text-gray-300 leading-relaxed"]')

	for v in x.XPath('//div[@class="w-full flex-between"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div[1]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end