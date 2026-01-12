----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '27c2c6db9ce24942a89a28aa6c6ed35d'
	m.Name                     = 'Azora Manga'
	m.RootURL                  = 'https://azoramoon.com'
	m.Category                 = 'Arabic-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnCheckSite              = 'CheckSite'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Iken'
API_URL = 'https://api.azoramoon.com'

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
	Template.GetPageNumber()

	return true
end

-- Verify the module's functionality by checking a specific manga and chapter.
function CheckSite()
    MANGACHECK.MangaURL     = '/series/the-little-tyrant-is-making-a-scene'
    MANGACHECK.MangaTitle   = 'The Little Tyrant Is Making a Scene!'
    MANGACHECK.ChapterURL   = '/series/the-little-tyrant-is-making-a-scene/chapter-26'
    MANGACHECK.ChapterTitle = 'Chapter 26'
end