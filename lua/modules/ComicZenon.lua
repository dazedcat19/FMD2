----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.GigaViewer'

GigaIsUnavailable = function(product)
	return (product.purchase_info or {}).unavailable == true or product.status == nil
end

GigaDirectoryList = {
	'12207421983695504181',
	'12207421983695504182',
	'12207421983695504186',
	'12207421983695504189',
	'12207421983695504190',
	'12207421983695504198',
	'12207421983695504201',
	'10834108156697286365',
	'10834108156697286367',
	'10834108156697286369',
	'10834108156697286370',
	'14079602755177733270',
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
	m.ID               = 'e88913a02baa4581b3b94d9240c3c017'
	m.Name             = 'ゼノン編集部 (Comic Zenon)'
	m.RootURL          = 'https://comic-zenon.com'
	m.Category         = 'Raw'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetPageNumber  = 'GetPageNumber'
	m.OnDownloadImage  = 'DownloadImage'
	m.TotalDirectory   = 1
	Template.AddOptions(m)
end
