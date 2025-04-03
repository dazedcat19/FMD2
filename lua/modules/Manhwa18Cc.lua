----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'aeff15717c094a8384668b3ea53db9bd'
	m.Name                     = 'Manhwa18Cc'
	m.RootURL                  = 'https://manhwa18.cc'
	m.Category                 = 'H-Sites'
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

	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//li[@class="a-h wleft"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "read-content")]/img/@data-src', TASK.PageLinks)

	return true
end