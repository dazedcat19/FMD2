----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
local m = NewWebsiteModule()
	m.ID                       = 'f4157690473045319dfb9ed9ed8953be'
	m.Name                     = 'RavenSeries'
	m.RootURL                  = 'https://ravensword.lat'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaEsp'
DirectoryPagination = '/comics'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('proyectos = ', '}];', x.XPathString('//script[contains(., "proyectos")]')) .. '}]')
	for v in x.XPath('json(*)()').Get() do
		LINKS.Add('sr2/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('nombre').ToString())
	end

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
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="flex flex-wrap"]/a')
	MANGAINFO.Summary   = x.XPathString('//p[@class="font-light text-gray-400"]')

	for v in x.XPath('//div[@class="grid md:grid-cols-2 gap-4"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div[2]/div/div[@id="name"]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end
