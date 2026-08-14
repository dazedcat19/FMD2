----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '97dcd409f781467eb2da4a8634e5a1d9'
	m.Name                     = 'CapibaraTraductor'
	m.RootURL                  = 'https://capibaratraductor.com'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/manga-custom?limit=1000&order=alphabetical&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*).data.maxPage')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).data.items()').Get() do
		LINKS.Add(v.GetProperty('organization').GetProperty('slug').ToString() .. '/manga/' .. v.GetProperty('manga').GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local site, slug = URL:match('/([^/]+)/manga/([^/]+)')
	local u = MODULE.RootURL .. '/api/manga-custom/' .. slug
	HTTP.Headers.Values['X-Organization'] = site

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('parse-json(.)?data')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.CoverLink = x.XPathString('imageUrl', info)
	MANGAINFO.Authors   = x.XPathString('string-join(manga?authors?*?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, manga?demography?name, manga?bookType?name), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('replace(status, "finished", "completed")', info))
	MANGAINFO.Summary   = x.XPathString('description', info)

	for v in x.XPath('chapters?*[not(isUnreleased=true)]', info).Get() do
		local number = v.GetProperty('number').ToString()
		local title = v.GetProperty('title').ToString()

		title = (title ~= '') and not title:find('Capítulo ' .. number, 1, true) and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(site .. '/' .. slug .. '/' .. number)
		MANGAINFO.ChapterNames.Add('Capítulo ' .. number .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local site, slug, cid = URL:match('^/([^/]+)/([^/]+)/([^/]+)$')
	local u = MODULE.RootURL .. '/api/manga-custom/' .. slug .. '/chapter/' .. cid .. '/pages'
	HTTP.Reset()
	HTTP.Headers.Values['X-Organization'] = site

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data().imageUrl', TASK.PageLinks)

	return true
end