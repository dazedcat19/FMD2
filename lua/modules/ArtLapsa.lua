----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ff66216635184bdbb7630155b51764d1'
	m.Name                     = 'Art Lapsa'
	m.RootURL                  = 'https://artlapsa.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showpaidchapters'] = 'Show paid chapters'
		},
		['id_ID'] = {
			['showpaidchapters'] = 'Tampilkan bab berbayar'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('showpaidchapters', lang.showpaidchapters, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.KeyoApp'
local DirectoryPagination = '/latest'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//a[contains(@href, "/series/")]', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	MANGAINFO.CoverLink = CreateTXQuery(HTTP.Document).XPathString('//div[contains(@class, "bg-cover bg-center bg-white")]/@style ! substring-before(substring-after(., "("), ")")')

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(//div[contains(@x-data, "immersiveReader")]/@x-data ! substring-before(substring-after(., "immersiveReader("), ")"))')
	local base = x.XPathString('baseLink', json)
	for v in x.XPath('pages?*?path', json).Get() do
		TASK.PageLinks.Add(base .. v.ToString())
	end

	return no_error
end