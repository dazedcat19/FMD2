----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '462c20a8842e44e4a6e1811fab1c78e2'
	m.Name                     = 'WeLoMa'
	m.RootURL                  = 'https://weloma.art'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.FMReader'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	Template.GetDirectoryPageNumber()

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	MANGAINFO.Summary = CreateTXQuery(HTTP.Document).XPathString('//div[@class="sContent"]')

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	HTTP.Reset()
	HTTP.Cookies.Values['unlock_chapter_guest'] = 1

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('//div[@id="chapter-images"]/img').Get() do
		TASK.PageLinks.Add(require 'fmd.crypto'.DecodeBase64(v.GetAttribute('data-img')))
	end

	return true
end