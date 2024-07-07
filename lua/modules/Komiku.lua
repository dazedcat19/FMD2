----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5af0f26f0d034fb2b42ee65d7e4188ab'
	m.Name                     = 'Komiku'
	m.RootURL                  = 'https://komiku.id'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
DirectoryPagination = '/daftar-komik/'
XPathTokenAuthors   = 'Komikus'
-- XPathTokenArtists   = 'Artist'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="ls4j"]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//span[@itemprop="name"]'):gsub('Komik', '')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="ims"]/img/@src')
	MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="genre"]//a')
	MANGAINFO.Summary   = x.XPathString('//p[@class="desc"]')

	x.XPathHREFTitleAll('//td[@class="judulseries"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="Baca_Komik"]/img/@src', TASK.PageLinks)

	return no_error
end