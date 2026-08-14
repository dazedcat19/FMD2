----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '8f7396a288e947bba73556c9b0ec41d4'
	m.Name                     = 'ZinManga'
	m.RootURL                  = 'https://www.zinmanga.net'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'
DirectoryParameters = '?orderby=new-manga&post_type=wp-manga'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1 .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[contains(@class, "pagination")]/li[last()]/a/@href'):match('^(%d+)?')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLinkWithPagination()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Summary = x.XPathString('//h3[text()="Main Plot"]/following-sibling::p[1]')

	local slug = URL:match('/([^/]+)$')
	local page = 1
	local pages = nil
	while true do
		if not HTTP.GET(MODULE.RootURL .. '/api/comics/' .. slug .. '/chapters?per_page=100&order=asc&page=' .. page) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).data.chapters()').Get() do
			MANGAINFO.ChapterLinks.Add('manga/' .. slug .. '/' .. v.GetProperty('chapter_slug').ToString())
			MANGAINFO.ChapterNames.Add(v.GetProperty('chapter_name').ToString())
		end
		if not pages then
			pages = tonumber(x.XPathString('json(*).data.last_page')) or 1
		end
		page = page + 1
		if page > pages then
			break
		end
	end

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end