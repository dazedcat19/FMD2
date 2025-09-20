----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'e4e226f0270440b7b906171082c8a02f'
	m.Name                     = 'ColoredManga'
	m.RootURL                  = 'https://coloredmanga.com'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/utils/mangaByIndex'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	
	return true
end