----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'f5192ff4ca8a412fa7ae2bc18f76e130'
	m.Name                     = 'MangaRaw1001'
	m.RootURL                  = 'https://mangaraw1001.cc'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local TemplateLiliana  = require 'templates.Liliana'
local TemplateWPComics = require 'templates.WPComics'
DirectoryPagination = '/all-manga/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	TemplateWPComics.GetDirectoryPageNumber()

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()]/a/@href'):match('/(%d+)$')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	TemplateWPComics.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	TemplateWPComics.GetInfo()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	TemplateLiliana.GetPageNumber()

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(require 'utils.json'.decode(HTTP.Document.ToString()).html)
	for i = 0, x.XPathCount('//img/@data-original') do
		TASK.PageLinks.Add(x.XPathString('//img[@data-index="' .. i .. '"]/@data-original'))
	end

	return no_error
end