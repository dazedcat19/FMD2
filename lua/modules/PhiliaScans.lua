----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c280ce32f36843fbba73dcc891e979af'
	m.Name                     = 'Philia Scans'
	m.RootURL                  = 'https://philiascans.org'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnLogin                  = 'Login'
	m.AccountSupport           = true

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['showpaidchapters'] = 'Show paid chapters'
		},
		['id_ID'] = {
			['showpaidchapters'] = 'Tampilkan bab berbayar'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionCheckBox('showpaidchapters', lang:get('showpaidchapters'), false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Iken'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Login account to the current website.
function Login()
	Template.Login()

	return no_error
end

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

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end