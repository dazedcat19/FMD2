----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, url, cat)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = 'NiAddEN'
		m.RootURL                  = url
		m.Category                 = cat
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.OnGetImageURL            = 'GetImageURL'
		m.TotalDirectory           = #DirectoryPages
	end
	AddWebsiteModule('1230bae145b3452580164d00acc05e6f', 'https://www.niadd.com', 'English')
	AddWebsiteModule('c7677c10dd6649c39ed92ac53dd8b4cc', 'https://niadd.com')
	AddWebsiteModule('9df276cd1d794ccfb9ef135c8e8f8b0d', 'https://english.niadd.com')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.NiAdd'

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

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
	Template.GetImageURL()

	return true
end