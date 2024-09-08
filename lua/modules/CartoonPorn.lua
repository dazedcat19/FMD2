----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd2d4a20548a641e4b24ae4c49379ae1f'
	m.Name                     = 'CartoonPorn'
	m.RootURL                  = 'https://cartoonporn.to'
	m.Category                 = 'Adult'
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
DirectoryPagination = '/porn-comic/page/'
DirectoryParameters = '/?m_orderby=latest'
-- XPathTokenAuthors = 'Author(s)'
-- XPathTokenArtists = 'Artist(s)'
-- XPathTokenGenres  = 'Genre(s)'
-- XPathTokenStatus  = 'Status'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. "1") then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="wp-pagenavi"]/a[last()]/@href'):match('/(%d+)/?')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectoryParameters) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[contains(@class, "post-title")]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="post-title"]/p')
	MANGAINFO.Artists   = x.XPathStringAll('//span[contains(., "Artists")]/following-sibling::span/a/span[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//span[contains(., "Tags")]/following-sibling::span/a/span[1]')
	MANGAINFO.Summary   = x.XPathString('//div[contains(span, "Description")]//div')

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end