----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '9f756fcbfa114ea4a9abb578004edf31'
	m.Name                     = 'SkyMangas'
	m.RootURL                  = 'https://skymangas.com'
	m.Category                 = 'Spanish'
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

-- Get info and chapter list for current manga.
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
	s = require 'fmd.crypto'.DecodeBase64(x.XPathString('//script[contains(@src, "dHNfcmVhZGVyLnJ1bih7")]/@src/substring-after(., ",")'))
	x.ParseHTML(GetBetween('run(', ');', s))
	x.XPathStringAll('json(*).sources()[1].images()', TASK.PageLinks)

	return true
end