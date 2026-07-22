----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'a1f3c7d9e0b64a2c8d5f1e93b7a4c6d2'
	m.Name                     = 'Nexus Scanlation'
	m.RootURL                  = 'https://nexusscanlation.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.TotalDirectory           = #DirectoryPages
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.nexusscanlation.com/api/v1'
local DirectoryPages = { 'manga', 'manhwa', 'manhua' }
local PAGE_LIMIT = 50

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. '/catalog?tipo=' .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. '&orden=nuevo&page=' .. (URL + 1) .. '&limit=' .. PAGE_LIMIT

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('titulo').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = math.ceil(x.XPathString('json(*).meta.total') / PAGE_LIMIT) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('/([^/]+)$')
	local u = API_URL .. '/series/' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local serie = x.XPath('json(*).serie')
	MANGAINFO.Title     = x.XPathString('titulo', serie)
	MANGAINFO.AltTitles = x.XPathString('string-join(titulos_alt?*, ", ")', serie)
	MANGAINFO.CoverLink = x.XPathString('portada_url', serie)
	MANGAINFO.Authors   = x.XPathString('string-join(autores?*?nombre, ", ")', serie)
	MANGAINFO.Genres    = x.XPathString('string-join(generos?*?nombre, ", ")', serie)
	MANGAINFO.Summary   = x.XPathString('descripcion', serie)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('estado', serie), 'en_emision', 'finalizado', 'pausado')

	for v in x.XPath('json(*).capitulos()').Get() do
		MANGAINFO.ChapterLinks.Add(slug .. '/' .. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Capítulo ' .. v.GetProperty('numero').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local mangaslug, chapslug = URL:match('^/([^/]+)/([^/]+)$')
	local u = API_URL .. '/series/' .. mangaslug .. '/capitulos/' .. chapslug

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.paginas().url', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end
