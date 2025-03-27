----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '491ab77dd809411184bfd6dcab482ec5'
	m.Name                     = 'Witch Scans'
	m.RootURL                  = 'https://witchscans.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'

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
	local s, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	s = require 'fmd.crypto'.DecodeBase64(x.XPathString('(//script[contains(@src, "base64") and not(@type)])[last()-5]/@src/substring-after(., ",")'))
	x.ParseHTML(GetBetween('run(', ');', s))
	x.XPathStringAll('json(*).sources()[1].images()', TASK.PageLinks)

	return true
end