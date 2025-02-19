----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '3593adad980d454abe489c42e7158032'
	m.Name                     = 'RizzFables'
	m.RootURL                  = 'https://rizzfables.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
DirectoryPagination = '/series'

----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

function decode_unicode(str)
	return str:gsub("\\u(%x%x%x%x)", function(hex) return require("utf8").char(tonumber(hex, 16)) end)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="bsx"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	MANGAINFO.Summary = decode_unicode(CreateTXQuery(HTTP.Document).XPathString('//div[@itemprop="description"]/substring-before(substring-after(., "var description = """), """ ;")'))

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="readerarea"]/img/@src', TASK.PageLinks)

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end