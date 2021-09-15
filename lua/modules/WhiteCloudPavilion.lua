----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaReaderOnline'
DirectoryParameters = '/manga/patreon/changeMangaList?type=text'
-- XPathTokenStatus    = 'Status'       --> Override template variable by uncommenting this line.
-- XPathTokenAuthors   = 'Author(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenGenres    = 'Categories'   --> Override template variable by uncommenting this line.

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

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
	m.ID                       = 'd7b50edfda2d4dc69eae3e1c1886000d'
	m.Name                     = 'WhiteCloudPavilion'
	m.RootURL                  = 'https://www.whitecloudpavilion.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end