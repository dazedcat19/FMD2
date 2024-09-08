----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '36b0f19e144a4642a2839ec9cb728c3a'
	m.Name                     = 'RagnarokScan'
	m.RootURL                  = 'https://ragnarokscanlation.ketonos.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'
-- XPathTokenAuthors = 'Author(s)'
-- XPathTokenArtists = 'Artist(s)'
-- XPathTokenGenres  = 'Genre(s)'
-- XPathTokenStatus  = 'Status'

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

	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	MANGAINFO.Summary   = CreateTXQuery(HTTP.Document).XPathString('//div[@class="manga-excerpt"]')

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end