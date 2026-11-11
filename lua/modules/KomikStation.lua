----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
XPathTokenAuthors   = 'Penulis'
XPathTokenArtists   = 'Ilustrator'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'b5586745030a45bba05d0c360caa6d1a'
	m.Name                     = 'KomikStation'
	m.RootURL                  = 'https://komikstation.org'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end