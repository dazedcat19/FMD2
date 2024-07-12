----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '3ba200a78325443cae4922f95047c52e'
	m.Name                     = 'SirenKomik'
	m.RootURL                  = 'https://sirenkomik.my.id'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
-- DirectoryPagination = '/manga/list-mode/'
-- XPathTokenAuthors   = 'Author'
-- XPathTokenArtists   = 'Artist'

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
	Template.GetInfo()

	local v, x = nil

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="judul-komik"]')
	MANGAINFO.Authors   = x.XPathString('//div[@class="keterangan-komik" and contains(b, "' .. XPathTokenAuthors .. '")]/span')
	MANGAINFO.Artists   = x.XPathString('//div[@class="keterangan-komik" and contains(b, "' .. XPathTokenArtists .. '")]/span')
	MANGAINFO.Genres    = x.XPathStringAll('//span[@class="genre-komik"]/a')

	for v in x.XPath('//div[@class="episode"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[@class="nomer-chapter"]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end