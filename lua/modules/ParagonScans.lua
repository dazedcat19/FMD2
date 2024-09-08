----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'a09b6757649f4683b72d2e669d9fbffc'
	m.Name                     = 'ParagonScans'
	m.RootURL                  = 'https://paragonscans.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'
DirectoryPagination = '/mangax/page/'
-- XPathTokenAuthors = 'Author(s)'
-- XPathTokenArtists = 'Artist(s)'
-- XPathTokenGenres  = 'Genre(s)'
-- XPathTokenStatus  = 'Status'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. 1) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="wp-pagenavi"]/a[last()]/@href'):match('/(%d+)/?')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. '/?m_orderby=new-manga') then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[contains(@class, "post-title")]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end