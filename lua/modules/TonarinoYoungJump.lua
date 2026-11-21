----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.GigaViewer'

GigaDirectoryList = {
	'3269632237330951493',
	'3269632237330951504',
	'3269632237330951514',
	'3269632237330951522',
	'10834108156636443058',
	'3269632237330951523',
	'3269632237330951526',
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
	m.ID               = '88fb782a9c234fde82b0de32f710ce3f'
	m.Name             = 'となりのヤングジャンプ (Tonari no Young Jump)'
	m.RootURL          = 'https://tonarinoyj.jp'
	m.Category         = 'Raw'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetPageNumber  = 'GetPageNumber'
	m.OnDownloadImage  = 'DownloadImage'
	m.TotalDirectory   = 1
	Template.AddOptions(m)
end
