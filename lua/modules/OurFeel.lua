----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.GigaViewer'

GigaDirectoryList = {
	'2550668105997034151',
	'2550668105997034166',
	'2550912965173938828',
	'2550668105997034155',
	'2550668105997034159',
}

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

-- Download, decrypt and/or descramble image given the image URL.
function DownloadImage()
	Template.DownloadImage()

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID               = 'c92912baa34d49ce8e395bdf08d6874b'
	m.Name             = 'アワフィール (OUR FEEL)'
	m.RootURL          = 'https://ourfeel.jp'
	m.Category         = 'Raw'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetPageNumber  = 'GetPageNumber'
	m.OnDownloadImage  = 'DownloadImage'
	m.TotalDirectory   = 1
	Template.AddOptions(m)
end
