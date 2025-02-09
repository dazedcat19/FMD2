----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '68c51e4148ee471fa221cd2d05cb216a'
	m.Name                     = 'NineManga'
	m.RootURL                  = 'https://www.ninemanga.com'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetImageURL            = 'GetImageURL'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.NineManga'

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

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.AltTitles = x.XPathString('//li[contains(b, "Alternative(s):")]/substring-after(., "Alternative(s):")')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="message"]/li[contains(b, "Status")]/a[1]'))

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