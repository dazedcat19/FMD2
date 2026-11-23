----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.InitManga'
DirectoryPages = '/manga'

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

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local rev = tonumber(require 'fmd.env'.Revision)
	if rev < 7025 then
		print('Require FMD2 revision 7025 and later')
		print('Revision ' .. rev .. ' detected')
		return false
	end
	Template.GetPageNumber()

	return true
end

-- Download, decrypt and/or descramble the image of the current page.
function DownloadImage()
	Template.DownloadImage()

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c61078ca77ba44ec8129d7a7a66c087d'
	m.Name                     = 'MangaDrama'
	m.RootURL                  = 'https://mangadrama.com'
	m.Category                 = 'Webcomics'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showpaid'] = 'Show paid chapters'
		},
		['id_ID'] = {
			['showpaid'] = 'Tampilkan bab berbayar'
		}
	}
	local lang = translations[slang] or translations['en']
	m.AddOptionCheckBox('showpaid', lang.showpaid, false)
end
