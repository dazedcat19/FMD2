----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="lm4-poster-frame"]/img/@src')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "lm4-poster-status")]'), 'Publicando', 'Finalizado')

	local summary = x.XPathString('//p[@class="lm4-summary-full"]')
	if summary == '' then
		summary = x.XPathString('//p[@class="lm4-summary-short"]')
	end
	MANGAINFO.Summary = summary

	for v in x.XPath('//ul[@class="lm4-chapter-grid"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('.//div[@class="lm4-chapter-name"]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'f3098381220841f8b13928e682de4d7e'
	m.Name                     = 'LeeMiau'
	m.RootURL                  = 'https://leemiau.com'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end