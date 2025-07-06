----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'English'
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.SortedList               = true
	end
	AddWebsiteModule('694ff34a6ae4469fbdaecf8d3aebb6eb', 'KaliScanIo', 'https://kaliscan.io')
	AddWebsiteModule('d29ab69c096948aa83847fc164e4ebde', 'KaliScanMe', 'https://kaliscan.me')
	AddWebsiteModule('9b823543dc5e4adc988f1a50831bb297', 'KaliScanCom', 'https://kaliscan.com')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MadTheme'
UseLegacyApi = true

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	Template.GetDirectoryPageNumber()

	return no_error
end

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