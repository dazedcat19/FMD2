----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '0b3d848de717473fbb71c87a89921ba5'
	m.Name                     = 'LerHentais'
	m.RootURL                  = 'https://lerhentais.com'
	m.Category                 = 'Portuguese'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Hiper'
__st = '1df1ec97d7be092aab66180283e6f8206662bf59428fb8b1c8fb9d990dc0515f'
ChapterName = 'Capítulo '
Key = 'X-Lh-Nexus'
Value = 'c37-block-q24'

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

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return true
end