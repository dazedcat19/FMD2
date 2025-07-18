----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '6425c06121494842b49bfb73112f8596'
	m.Name                     = 'Arcane Scans'
	m.RootURL                  = 'https://arcanescans.com'
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
	s = require 'fmd.crypto'.DecodeBase64(x.XPathString('//script[contains(@src, "dHNfcmVhZGVyLnJ1bih7")]/@src/substring-after(., ",")'))
	if s == '' then s = x.XPathString('//script[contains(., "ts_reader")]') end
	x.ParseHTML(GetBetween('run(', ')', s):gsub('!1', 'false'))
	x.XPathStringAll('json(*).sources()[1].images()', TASK.PageLinks)

	return true
end