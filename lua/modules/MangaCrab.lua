----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '73cfa250c661470c81428d99cdb8a140'
	m.Name                     = 'MangaCrab'
	m.RootURL                  = 'https://mangacrab.topmanhuas.org'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'

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
	Template.GetNameAndLinkWithPagination()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="post-title"]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres-content"]//a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="manga-status"]/span[2]'), 'En Emisión', 'Finalizado', 'En Hiatus', 'Cancelada')
	MANGAINFO.Summary   = x.XPathString('//div[@class="summary__content"]/p')

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "page-break")]/img/@data-src-base64', TASK.PageLinks)

	return true
end