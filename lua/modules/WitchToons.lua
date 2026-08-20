----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.NovelsHub'

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

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '491ab77dd809411184bfd6dcab482ec5'
	m.Name                     = 'WitchToons'
	m.RootURL                  = 'https://witchtoons.net'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showpaidchapters'] = 'Show paid chapters'
		},
		['id_ID'] = {
			['showpaidchapters'] = 'Tampilkan bab berbayar'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('showpaidchapters', lang.showpaidchapters, false)
end