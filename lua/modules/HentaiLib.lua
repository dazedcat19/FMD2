----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                    = '17da8b4f514d418097801bfe9c88feab'
	m.Name                  = 'HentaiLib'
	m.RootURL               = 'https://hentailib.me'
	m.Category              = 'H-Sites'
	m.OnGetNameAndLink      = 'GetNameAndLink'
	m.OnGetInfo             = 'GetInfo'
	m.OnGetPageNumber       = 'GetPageNumber'
	m.OnBeforeDownloadImage = 'BeforeDownloadImage'
	m.SortedList            = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['auth'] = 'Authorization:',
			['showscangroup'] = 'Show scanlation group',
			['isvr'] = 'Image server:',
			['svr'] = 'Main\nSecondary\nCompress'
		},
		['ru_RU'] = {
			['auth'] = 'Авторизация:',
			['showscangroup'] = 'Показать группу сканлейт',
			['isvr'] = 'Сервер изображений:',
			['svr'] = 'Первый\nВторой\nСжатия'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('showscangroup', lang.showscangroup, false)
	m.AddOptionEdit('auth', lang.auth)
	m.AddOptionComboBox('svr', lang.isvr, lang.svr, 0)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.LibGroup'
SITE_ID = '4'

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
	Template.GetPageNumber()

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	Template.BeforeDownloadImage()

	return true
end