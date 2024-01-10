----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ab34a56c83f54b19b57a9a92070fe899'
	m.Name                     = 'DatgarScanlation'
	m.RootURL                  = 'https://datgarscanlation.blogspot.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
DirectoryPagination = '/series/list-mode/'
-- XPathTokenAuthors   = 'Author'
-- XPathTokenArtists   = 'Artist'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

function urlencode(str)
    return string.gsub(str, "([^%w%-_%.~])", function(c)
        return string.format("%%%02X", string.byte(c))
    end)
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	json = require "utils.json"
	x = CreateTXQuery(HTTP.Document)

	if MANGAINFO.Title == '' then MANGAINFO.Title = x.XPathString('//h1') end
	if MANGAINFO.CoverLink == '' then MANGAINFO.CoverLink = x.XPathString('//a[@class="md-auto"]/img/@src') end
	if MANGAINFO.Summary == '' then MANGAINFO.Summary = x.XPathString('//p[@id="synopsis"]/text()') end
	if MANGAINFO.Genres == '' then MANGAINFO.Genres = x.XPathStringAll('//div[@class="mt-15"]/a') end

	local slug = x.XPathString('//div[@id="clwd"]/script'):match("clwd%.run%('([^']+)")
	if not slug then slug = x.XPathString('//div[@id="latest"]/script'):match("label = '([^']+)'") end
	
	local u = MODULE.RootURL .. "/feeds/posts/default/-/" .. urlencode(slug) .. "?alt=json&start-index=1&max-results=150000"

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	local y = json.decode(HTTP.Document.ToString())
	local entries = x.XPath('json(*).feed.entry()')

	for ic = 2, entries.Count do
		local t = y.feed.entry[ic].link[5].title  
		local c = y.feed.entry[ic].link[5].href  
		MANGAINFO.ChapterLinks.Add(c)
		MANGAINFO.ChapterNames.Add(t)
	end	
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()	

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()
	
	x = CreateTXQuery(HTTP.Document)
	if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@class="separator"]//img/@src', TASK.PageLinks) end

	return no_error
end
