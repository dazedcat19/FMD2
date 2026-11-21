----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.GigaViewer'

GigaDirectoryList = {
	'14079602755185553136',
	'14079602755185553138',
	'14079602755185553142',
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
	m.ID               = '942a3a29a8c047e2880ba9c22ad23d3a'
	m.Name             = 'コミック アース・スター (Comic Earth Star)'
	m.RootURL          = 'https://comic-earthstar.com'
	m.Category         = 'Raw'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetPageNumber  = 'GetPageNumber'
	m.OnDownloadImage  = 'DownloadImage'
	m.TotalDirectory   = 1
	Template.AddOptions(m)
end
