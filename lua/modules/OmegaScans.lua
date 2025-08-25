----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '752cda75b5e24f6ab4256079c564eba2'
	m.Name                     = 'Omega Scans'
	m.RootURL                  = 'https://omegascans.org'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.HeanCMS'
API_URL = 'https://api.omegascans.org'

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
	Template.GetNameAndLinkOld()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfoOld()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = API_URL .. URL

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).chapter.chapter_data.images()').Get() do
		image = v.ToString()
		if not image:match('^https?://') then
			image = API_URL .. '/' .. image
		end
		TASK.PageLinks.Add(image)
	end

	return true
end