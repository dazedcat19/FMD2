----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5b0c4c616c7f4a759d40d5b6924e535b'
	m.Name                     = 'SetsuScans'
	m.RootURL                  = 'https://setsuscans.com'
	m.Category                 = 'English-Scanlation'
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

	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title   = x.XPathString('//div[@id="manga-title"]/h1/text()')
	MANGAINFO.Summary = x.XPathString('//div[@class="post-content_item" and contains(., "Summary")]//p')

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end