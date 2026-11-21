----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.GigaViewer'

GigaDirectoryList = {
	'2550912965889047616',
	'2550912965796689066',
	'2550912965796689067',
	'2550912965796689072',
	'2550912965796689074',
	'2550912965796689075',
	'2550912965796689080',
	'2550912965796689089',
	'2550912965796689092',
	'2550912965796689093',
	'2550912965796689101',
	'2550912965796689105',
	'2550912965796689108',
	'2550912965796689112',
	'2550912965796689114',
	'2550912965796689120',
	'2550912965796689123',
	'2550912965796689131',
	'2550912965796689144',
	'2550912965796689168',
	'2550912965796689175',
	'2550912965796689183',
	'2550912965796689148',
	'2550912965796689158',
	'2550912965796689163',
	'2550912965796689187',
	'2550912965796689195',
	'2550912965796689196',
	'2550912965796689199',
	'2550912965796689206',
	'2550912965796689212',
	'2550912965796689213',
	'2550912965796689214',
	'2550912965796689218',
	'2550912965796689248',
	'2550912965796689255',
	'2550912965796689266',
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
	m.ID               = '313535611bcf4f8d998440af7f7f3781'
	m.Name             = '一迅プラス (Ichijin Plus)'
	m.RootURL          = 'https://ichicomi.com'
	m.Category         = 'Raw'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetPageNumber  = 'GetPageNumber'
	m.OnDownloadImage  = 'DownloadImage'
	m.TotalDirectory   = 1
	Template.AddOptions(m)
end
