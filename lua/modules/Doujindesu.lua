----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ec1a1ad5301f414592f0ba0402024813'
	m.Name                     = 'Doujindesu'
	m.RootURL                  = 'https://doujindesu.tv'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
DirectoryPagination = '/manga/page/'
-- XPathTokenAuthors   = 'Author'
XPathTokenArtists   = 'Character'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFTitleAll('//*[@class="entry"]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//*[@class="pagination"]//li[last()-1]/a')) or 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title"]/text()[not(span)]')
	MANGAINFO.CoverLink = x.XPathString('//*[@class="thumbnail"]/a/img/@src')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="tags"]/a')
	MANGAINFO.Summary   = x.XPathString('//div[@class="pb-2"]/p[1]/text()')

	x.XPathHREFAll('//span[@class="lchx"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	local q = 'id=' .. CreateTXQuery(HTTP.Document).XPathString('//*[@id="reader"]/@data-id')
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
	HTTP.MimeType = 'application/x-www-form-urlencoded'
	if HTTP.POST(MODULE.RootURL .. '/themes/ajax/ch.php', q) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//img/@src', TASK.PageLinks)
	end

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end