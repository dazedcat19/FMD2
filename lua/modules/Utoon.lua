----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2b8e4004d8bd434ca5d8b75da95499f9'
	m.Name                     = 'Utoon'
	m.RootURL                  = 'https://utoon.net'
	m.Category                 = 'English'
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
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not u:find('style=list', 1, true) then u = u:gsub('?style=paged', '') .. '?style=list' end

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('//div[contains(@class, "page-break")]/img').Get() do
		if not v.GetAttribute('src'):find('banner', 1, true) then
			TASK.PageLinks.Add(v.GetAttribute('src'))
		end
	end

	return true
end