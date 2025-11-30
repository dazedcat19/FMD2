----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '4d4affa9d7684180b64fe6c65f3f5b5d'
	m.Name                     = 'Drake Scans'
	m.RootURL                  = 'https://drakecomic.org'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
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
	MANGAINFO.Title     = x.XPathString('//h1[@class="entry-title"]')
	MANGAINFO.AltTitles = x.XPathString('//span[@class="alternative"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@itemprop="image"]//img/@src')
	MANGAINFO.Authors   = x.XPathString('(//div[@class="imptdt" and contains(., "Author")]/i)[1]')
	MANGAINFO.Artists   = x.XPathString('(//div[@class="imptdt" and contains(., "Artist")]/i)[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//span[@class="mgen"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('(//div[@class="imptdt" and contains(., "Status")]/i)[1]'))
	MANGAINFO.Summary   = x.XPathString('//div[@itemprop="description"]//p/string-join(text(), "\r\n")')

	for v in x.XPath('//div[@id="chapterlist"]//div[@class="chbox" and not(./span)]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[@class="chapternum"]/normalize-space(.)', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return true
end