----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '6ac92a5cdf034857a066c9e0145cef31'
	m.Name                     = 'KappaBeast'
	m.RootURL                  = 'https://kappabeast.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://strapi.kappabeast.com'
local DirectoryPagination = '/api/mangas?populate[media][populate]=*&populate[category][fields][0]=name&pagination[pageSize]=100&pagination[page]='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).meta.pagination.pageCount')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. '/api/mangas?filters[slug][$eq]=' .. URL:match('/([^/]+)$') .. '&populate[media][populate]=*&populate[category][fields][0]=name'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(*).data()')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('altTitle', info)
	MANGAINFO.CoverLink = API_URL .. x.XPathString('media?*?coverImage?formats?small?url', info)
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Artists   = x.XPathString('artist', info)
	MANGAINFO.Genres    = x.XPathString('string-join((category?*?name, type), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('manga_status', info))
	MANGAINFO.Summary   = x.XPathString('description', info)

	local page = 1
	local pages = nil
	local mid = x.XPathString('documentId', info)
	while true do
		if not HTTP.GET(API_URL .. '/api/chapters?filters[manga][documentId][$eq]=' .. mid .. '&populate[pages][populate]=*&populate=manga&sort[0]=number:asc&pagination[pageSize]=100&pagination[page]=' .. page) then return net_problem end

		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).data()').Get() do
			local number = v.GetProperty('number').ToString()
			local title = v.GetProperty('title').ToString()

			local chapter = 'Chapter ' .. number

			if title == '' then
				title = chapter
			elseif not title:find(chapter, 1, true) then
				title = chapter .. ' - ' .. title
			end

			MANGAINFO.ChapterLinks.Add(mid .. '/' .. number)
			MANGAINFO.ChapterNames.Add(title)
		end
		if not pages then
			pages = tonumber(x.XPathString('json(*).meta.pagination.pageCount')) or 1
		end
		page = page + 1
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local mid, cid = URL:match('^/([^/]+)/([^/]+)$')
	local u = API_URL .. '/api/chapters?filters[manga][documentId][$eq]=' .. mid .. '&filters[number][$eq]=' .. cid .. '&populate[pages][populate]=*&populate=manga&pagination[pageSize]=100'

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('json(*).data().htmlContent'))
	x.XPathStringAll('//a/@href', TASK.PageLinks)

	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub('/s%d+/', '/s0/')
	end

	return true
end