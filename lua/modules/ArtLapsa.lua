----------------------------------------------------------------------------------------------------
-- Template Configuration
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

	local s = CreateTXQuery(HTTP.Document).XPathString('//div[contains(@x-data, "immersiveReader")]/@x-data'):gsub('\\u0022', '"'):gsub('\\/', '/'):gsub('\\', '')
	for v in s:gmatch('"path"%s*:%s*"([^"]+)"') do
		TASK.PageLinks.Add(v)
	end

	return true
end

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