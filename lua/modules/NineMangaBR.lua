----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2c4c27a8c4a245a79f2057fa3ea5b858'
	m.Name                     = 'NineMangaBR'
	m.RootURL                  = 'https://br.ninemanga.com'
	m.Category                 = 'Portuguese'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetImageURL            = 'GetImageURL'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.NineManga'

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

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.AltTitles = x.XPathString('//li[contains(b, "Alternativa:")]/substring-after(., "Alternativa:")')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="message"]/li[contains(b, "Status")]/a[1]'), 'Em tradução', 'Completo')

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
	Template.GetImageURL()

	return true
end