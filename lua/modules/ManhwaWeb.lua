----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'K042a85566244b7e836679491ce67ot0'
	m.Name                     = 'ManhwaWeb'
	m.RootURL                  = 'https://www.manhwaweb.com'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://manhwawebbackend-production.up.railway.app'
local DirectoryPagination = '/manhwa/library?order_item=creacion&order_dir=desc&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local page = 1
	while true do
		for v in x.XPath('json(*).data()').Get() do
			if v.GetProperty('_tipo').ToString() ~= 'novela' then
				LINKS.Add('manhwa/' .. v.GetProperty('_id').ToString())
				NAMES.Add(v.GetProperty('the_real_name').ToString())
			end
		end
		local has_next = x.XPathString('json(*).next')
		if has_next ~= 'true' then break end
		page = page + 1
		UPDATELIST.UpdateStatusText('Loading page ' .. page)
		if not HTTP.GET(API_URL .. DirectoryPagination .. page) then break end
		x.ParseHTML(HTTP.Document)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. '/manhwa/see/' .. URL:match('[^/]+$')
	
	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(*)')
	MANGAINFO.Title     = x.XPathString('name_esp', info)
	MANGAINFO.AltTitles = x.XPathString('string-join((name_raw, _name), ", ")', info)
	MANGAINFO.CoverLink = x.XPathString('_imagen', info)
	MANGAINFO.Authors   = x.XPathString('string-join(_extras?autores?*, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((_categoris?*?*, concat(upper-case(substring(_demografi, 1, 1)), lower-case(substring(_demografi, 2))), concat(upper-case(substring(_tipo, 1, 1)), lower-case(substring(_tipo, 2)))), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('_status', info), 'publicandose', 'finalizado', 'pausado')
	MANGAINFO.Summary   = x.XPathString('_sinopsis', info)

	for v in x.XPath('chapters?*', info).Get() do
		MANGAINFO.ChapterLinks.Add(v.GetProperty('link').ToString())
		MANGAINFO.ChapterNames.Add('Capítulo ' .. v.GetProperty('chapter').ToString())
	end  

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/chapters/see/' .. URL:match('[^/]+$')

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).chapter.img()', TASK.PageLinks)

	return true
end
