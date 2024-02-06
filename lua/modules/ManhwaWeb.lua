----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'K042a85566244b7e836679491ce67ot0'
	m.Name                     = 'ManhwaWeb'
	m.RootURL                  = 'https://manhwaweb.com'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------


API_URL = 'https://manhwawebbackend-production.up.railway.app'
DirectoryPagination = '/manhwa/library?buscar=&estado=&tipo=&erotico=&demografia=&tag=&order_item=alfabetico&order_dir=desc&generes=&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil, s = 0
	if not HTTP.GET(API_URL .. DirectoryPagination .. (URL + 1) ) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('series/comic-' .. x.XPathString('_id', v))
		NAMES.Add(x.XPathString('the_real_name', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = if x.XPathString('json(*).next') s++

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = API_URL .. '/manhwa/see/' .. URL:match('[^/]+$')
	
	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).name_esp')
	MANGAINFO.CoverLink = x.XPathString('json(*)._imagen')
	MANGAINFO.Genres    = x.XPathStringAll('json(*)._categoris()')
	MANGAINFO.Status    = x.XPathString('json(*).status_esp')
	MANGAINFO.Summary   = x.XPathString('json(*)._sinopsis')

	local v for v in x.XPath('json(*).chapters_esp()').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('link', v))
		MANGAINFO.ChapterNames.Add('Capítulo ' .. x.XPathString('chapter', v))
	end  

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(API_URL .. '/chapters/see/' .. URL:match('[^/]+$')) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*).chapter.img()').Get() do
		TASK.PageLinks.Add(v.ToString():gsub("cdn.statically.io/img/", ""))
	end

	return no_error
end
