----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '009bf49bc17a4a2a8e1c79cce6867651'
	m.Name                     = 'Komiksin ID'
	m.RootURL                  = 'https://komiksin.id'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'

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

	return no_error
end
