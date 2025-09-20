----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '58f8a130748443c285036e4c4bf49fe8'
	m.Name                     = 'Hentaidexy'
	m.RootURL                  = 'https://dexyscan.com'
	m.Category                 = 'H-Sites'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/mangas?limit=24&sort=-createdAt&page='

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