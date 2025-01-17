----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                    = 'f75dc361ebc44d55abef2d293efeb5e4'
	m.Name                  = 'SlashLib'
	m.RootURL               = 'https://slashlib.me'
	m.Category              = 'Russian'
	m.OnGetNameAndLink      = 'GetNameAndLink'
	m.OnGetInfo             = 'GetInfo'
	m.OnGetPageNumber       = 'GetPageNumber'
	m.SortedList            = true

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
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
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionComboBox('svr', lang:get('isvr'), lang:get('svr'), 0)
	m.AddOptionCheckBox('showscangroup', lang:get('showscangroup'), false)
	m.AddOptionEdit('auth', lang:get('auth'))
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.LibGroup'
SITE_ID = '2'

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

	return no_error
end