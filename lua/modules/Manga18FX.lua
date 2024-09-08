----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '7f1b637a525940db9459f1cbf2f7cfc3'
	m.Name                     = 'Manga18FX'
	m.RootURL                  = 'https://manga18fx.com'
	m.Category                 = 'H-Sites'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'
-- XPathTokenAuthors = 'Author(s)'
-- XPathTokenArtists = 'Artist(s)'
-- XPathTokenGenres  = 'Genre(s)'
-- XPathTokenStatus  = 'Status'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local x = nil
	local dirurl = MODULE.RootURL
	local next_url

	if not HTTP.GET(dirurl) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	while true do
		x.XPathHREFTitleAll('//h3[@class="tt mycover"]/a', LINKS, NAMES)
		next_url = x.XPathString('//ul[@class="pagination"]/li[last()]/a/@href')
		if HTTP.Terminated then break end
		if next_url == '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('/page/(%d+)') or ''))
		if HTTP.GET(dirurl .. next_url) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Summary = x.XPathString('//div[@class="dsct"]')

	x.XPathHREFTitleAll('//ul[@class="row-content-chapter"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end