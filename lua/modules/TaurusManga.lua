----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '35d1c04c0de345b1a52ec4ff1e6098b5'
	m.Name                     = 'TaurusManga'
	m.RootURL                  = 'https://lectortaurus.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="summary_image"]//img/@data-src')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres-content generos"]//a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="manga-status"]/span[2]'), 'En Curso', 'Completado', 'En Espera', 'Cancelada')
	MANGAINFO.Summary   = x.XPathString('//div[@class="summary__content"]/p')

	x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/div/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return true
end