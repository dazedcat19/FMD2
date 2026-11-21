----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.GigaViewer'

GigaDirectoryList = {
	'12207421983452274168',
	'12207421983452274175',
	'12207421983452274180',
	'12207421983452274185',
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
	m.ID               = '614a8ac69acf4fbbbd2decaec1959540'
	m.Name             = 'まんがタイムSquare (Manga Time Square)'
	m.RootURL          = 'https://mangatime-square.com'
	m.Category         = 'Raw'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetPageNumber  = 'GetPageNumber'
	m.OnDownloadImage  = 'DownloadImage'
	m.TotalDirectory   = 1
	Template.AddOptions(m)
end
