----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.ReadMangaRU'
-- DirectoryPagination       = '/'            --> Override template variable by uncommenting this line.
-- DirectoryOffset           = 0              --> Override template variable by uncommenting this line.

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	return no_error
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	Template.GetDirectoryPageNumber()

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
	m.ID                       = 'fe152f2481a84befbfa01d7f4312edb9'
	m.Name                     = 'UsagiOne'
	m.RootURL                  = 'https://web.usagi.one'
	m.Category                 = 'Russian'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end
