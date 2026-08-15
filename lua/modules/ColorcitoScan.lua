----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api'
local DirectoryPagination = '/searchProject'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).response()').Get() do
		LINKS.Add('ver/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('/([^/]+)$')
	local u = MODULE.RootURL .. API_URL .. '/showProject/' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('parse-json(.)?response')
	MANGAINFO.Title     = x.XPathString('name', info)
	MANGAINFO.AltTitles = x.XPathString('alternativeName', info)
	MANGAINFO.CoverLink = x.XPathString('urlImg', info)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*?author?name, ", ")', info)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*?artist?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join(genders?*?gender?name, ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('state?estado', info), 'En emision', 'Finalizado')
	MANGAINFO.Summary   = x.XPathString('sinopsis', info)

	for v in x.XPath('lastChapters?*[patreon = 0]', info).Get() do
		MANGAINFO.ChapterLinks.Add(slug .. '/'.. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Cap. ' .. v.GetProperty('num').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. API_URL .. '/showProject' .. URL

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local pages = x.XPathString('json(*).response.pages.urlImg')
	x.ParseHTML(pages)
	x.XPathStringAll('json(*)()', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'Spanish'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
	end
	AddWebsiteModule('01709c0af5d445e1a521dd6d458894b8', 'Colorcito Scan', 'https://coloresito.site')
	AddWebsiteModule('5450026642ecrrd48apotd81e57f8e40', 'Colorcito Scan (Afiliados)', 'https://colorcitotoons.site')
end
