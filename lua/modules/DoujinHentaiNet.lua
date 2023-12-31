----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaReaderOnline'
-- DirectoryParameters = '/'            --> Override template variable by uncommenting this line.
XPathTokenStatus    = 'Estado'
XPathTokenAuthors   = 'Autor(es)'
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
XPathTokenGenres    = 'Categorías'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('(//div[contains(@class, "post-title")]//h3)[1]'):gsub("Doujin Hentai: ", "") 
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//dt[text()="' .. XPathTokenStatus .. '"]/following-sibling::dd[1]/span'), 'En curso', 'Completa')
	MANGAINFO.Artists   = x.XPathString('//dt[text()="' .. XPathTokenArtists .. '"]/following-sibling::dd[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//dt[text()="' .. XPathTokenGenres .. '"]/following-sibling::dd[1]/a')

	local v; for v in x.XPath('//ul[contains(@class, "version-chap")]/li').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', v))
		--local name = x.XPathString('a/text()[normalize-space()]', v)
		MANGAINFO.ChapterNames.Add(x.XPathString('a/text()[normalize-space()]', v):gsub("Leer Manga ", ""))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()	

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID               = '5861673c7b1c4f4ba22be78fd599a2d9'
	m.Name             = 'DoujinHentaiNet'
	m.RootURL          = 'https://doujinhentai.net'
	m.Category         = 'Spanish'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetPageNumber  = 'GetPageNumber'
end
