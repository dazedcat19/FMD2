----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '760d177b1f6d4763a08971c0c1b5572b'
	m.Name                     = 'Olympus Scanlation'
	m.RootURL                  = 'https://zonaolympus.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://dashboard.zonaolympus.com/api'
DirectoryPagination = '/series?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------


-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data.series.data()').Get() do
		LINKS.Add('series/comic-' .. x.XPathString('slug', v))
		NAMES.Add(x.XPathString('name', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('json(*).data.series.last_page')) or 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local next_page, v, x = nil
	local slug = URL:match('/series/comic%-(.-)$')
	local u = API_URL .. '/series/' .. slug

	if not HTTP.GET(u) then return net_problem end
	
	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).data.name')
	MANGAINFO.CoverLink = x.XPathString('json(*).data.cover')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).data.genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).data.status.name'), 'Activo', 'Finalizado')
	MANGAINFO.Summary   = x.XPathString('json(*).data.summary')

	u = u .. '/chapters?page=1'
	while u do
		if not HTTP.GET(u) then return net_problem end
		x = CreateTXQuery(HTTP.Document)

		for v in x.XPath('json(*).data()').Get() do
			MANGAINFO.ChapterLinks.Add(slug .. '/chapters/' .. x.XPathString('id', v))
			MANGAINFO.ChapterNames.Add('Capítulo ' .. x.XPathString('name', v))
		end  

		next_page = x.XPathString('json(*).links.next')
		u = next_page ~= 'null' and next_page or nil
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local v = nil
	local u = API_URL .. '/series' .. URL .. '?type=comic'

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).chapter.pages()').Get() do
		TASK.PageLinks.Add(v.ToString())
	end

	return no_error
end
