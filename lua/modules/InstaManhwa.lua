----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fb7d0d7517684937971b3b558a465fff'
	m.Name                     = 'InstaManhwa'
	m.RootURL                  = 'https://www.instamanhwa.com'
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
	local next_url, x = nil
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	while true do
		x.XPathHREFTitleAll('//h3[@class="h5"]/a', LINKS, NAMES)
		next_url = x.XPathString('//ul[contains(@class, "pagination")]/li[last()]/a/@href')
		if HTTP.Terminated then break end
		if next_url == '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('(%d+)') or ''))
		if HTTP.GET(u .. '/' .. next_url) then
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

	local id, token, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	id = x.XPathString('//div[contains(@id, "manga-chapters-holder")]/@data-id')
	token = x.XPathString('//meta[@name="csrf-token"]/@content')
	HTTP.Reset()
	HTTP.Headers.Values['Cache-Control'] = 'no-cache'
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
	HTTP.MimeType = 'application/x-www-form-urlencoded'
	q = '_token=' .. token .. '&action=manga_get_chapters&manga=' .. id
	if HTTP.POST(MODULE.RootURL .. '/ajax', q) then
		HTTP.Document.SaveToFile("insta.html")
		CreateTXQuery(HTTP.Document).XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end