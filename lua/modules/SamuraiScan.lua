----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5ad6ebfe79f04b3b954829180052cb9e'
	m.Name                     = 'Samurai Scan'
	m.RootURL                  = 'https://samurai.wordoco.com'
	m.Category                 = 'Spanish-Scanlation'
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
	local i = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not u:find('style=list') then u = u:gsub('?style=paged', '') .. '?style=list' end

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "page-break")]/img/@src', TASK.PageLinks)
	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub('http://', 'https://')
		i = i + 1
	end

	return true
end