----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.NovelCool'
StatusOngoing   = 'Ongoing'
StatusCompleted = 'Completed'

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

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return true
end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
	Template.GetImageURL()

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '1acdfb072dc744c694a481ef0f4da6c3'
	m.Name                     = 'NovelCoolFR'
	m.RootURL                  = 'https://fr.novelcool.com'
	m.Category                 = 'French'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetImageURL            = 'GetImageURL'
	m.TotalDirectory           = AlphaList:len()
end