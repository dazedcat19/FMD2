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

-- The site is a Next.js SPA; all data comes from a public JSON API on the api. subdomain.
local API_URL = 'https://api.nexusscanlation.com/api/v1'

-- The catalog is segmented by content type. Each becomes one directory that FMD2
-- iterates via MODULE.CurrentDirectoryIndex (0-based), same pattern as MangaGo's genres.
DirectoryPages = { 'manga', 'manhwa', 'manhua', 'novel', 'doujin' }
local PAGE_LIMIT = 24

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	-- FMD2 drives the loop: it calls this once per directory (content type) and,
	-- within each, once per page. URL holds the current 0-based page number.
	local tipo = DirectoryPages[MODULE.CurrentDirectoryIndex + 1]
	local page = tonumber(URL) + 1
	local u = API_URL .. '/catalog?tipo=' .. tipo .. '&orden=popular&page=' .. page .. '&limit=' .. PAGE_LIMIT

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		local slug = v.GetProperty('slug').ToString()
		if slug ~= '' then
			LINKS.Add('series/' .. slug)
			NAMES.Add(v.GetProperty('titulo').ToString())
		end
	end

	-- Tell FMD2 how many pages this content type has, so it re-calls us for each.
	local total = tonumber(x.XPathString('json(*).meta.total')) or 0
	UPDATELIST.CurrentDirectoryPageNumber = math.ceil(total / PAGE_LIMIT)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('series/(.+)$') or URL
	local u = API_URL .. '/series/' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local serie = x.XPath('json(*).serie')

	MANGAINFO.Title     = x.XPathString('titulo', serie)
	MANGAINFO.CoverLink = x.XPathString('portada_url', serie)
	MANGAINFO.Authors   = x.XPathString('autores', serie)
	MANGAINFO.Genres    = x.XPathString('string-join(generos()?nombre, ", ")', serie)
	MANGAINFO.Summary   = x.XPathString('descripcion', serie)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('estado', serie),
	                                           'en_emision|en_pausa',
	                                           'completado|cancelado')

	-- Chapters live in the sibling 'capitulos' array of the same response.
	-- They arrive newest-first and may skip numbers, so always use each chapter's own slug.
	for v in x.XPath('json(*).capitulos()').Get() do
		MANGAINFO.ChapterLinks.Add(slug .. '/capitulos/' .. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Capítulo ' .. v.GetProperty('numero').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local mangaslug, chapslug = URL:match('^(.-)/capitulos/(.-)$')
	if not mangaslug then return false end
	local u = API_URL .. '/series/' .. mangaslug .. '/capitulos/' .. chapslug

	if not HTTP.GET(u) then return false end

	-- Pages are in data.paginas, each with an 'url' (full CDN link) and an 'orden' index.
	-- The array already comes ordered by 'orden', so a direct extraction preserves reading order.
	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.paginas()?url', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'
	return true
end
