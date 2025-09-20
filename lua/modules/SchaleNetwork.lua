----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '7eafb0a70e59463fb4957950bd7831bd'
	m.Name                     = 'Schale Network'
	m.RootURL                  = 'https://niyaniya.moe'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.MaxTaskLimit             = 1
	m.MaxConnectionLimit       = 1
	m.SortedList               = true

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['datasaver'] = 'Data saver',
			['clearance'] = 'Clearance Value:'
		},
		['id_ID'] = {
			['datasaver'] = 'Penghemat data',
			['clearance'] = 'Nilai Clearance:'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionCheckBox('datasaver', lang:get('datasaver'), false)
	m.AddOptionEdit('clearance', lang:get('clearance'))
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.SchaleNetwork'
API_URL = 'https://api.schale.network/books'

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

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	Template.BeforeDownloadImage()

	return true
end