----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.GigaViewer'

GigaDirectoryList = {
	'3269754496454696025',
	'3269754496454696029',
	'3269754496454696030',
	'3269754496454696040',
	'3269754496454696045',
	'3269754496454696049',
	'3269754496454696050',
	'3269754496576310217',
	'3269754496454696059',
	'3269754496400962241',
}

GigaSeriesPages = { '/series/yoru-sunday' }

GigaSeriesPageXPath = {
	item_xpath  = '//ul[contains(@class, "webry-series-list")]/li/a[contains(@class, "webry-series-item-link")]',
	title_xpath = 'h4[contains(@class, "series-title")]',
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
	m.ID               = '2ca5fae1c28f4c4db4ecc71377da5bac'
	m.Name             = 'サンデーうぇぶり (Sunday Webry)'
	m.RootURL          = 'https://www.sunday-webry.com'
	m.Category         = 'Raw'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetPageNumber  = 'GetPageNumber'
	m.OnDownloadImage  = 'DownloadImage'
	m.TotalDirectory   = 1
	Template.AddOptions(m)
end
