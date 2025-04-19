----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ccd18d88c32e4cf9bd8a7847dafd7f34'
	m.Name                     = 'Borat Scans'
	m.RootURL                  = 'https://boratscans.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return true
end