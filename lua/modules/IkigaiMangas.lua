----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/series/?tipos[]=comic&tipos[]=manga&ordenar=created_at&pagina='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//nav[@aria-label="pagination"]/a[last()-1]/@aria-label'):match('%d+')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//ul[contains(@class, "gap-x-2 gap-y-6")]/li/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('.//h3', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="w-full rounded-t-box"]/@src')
	MANGAINFO.Genres    = x.XPathStringAll('//li/a[contains(@href, "generos[]")]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//a[contains(@href, "estados[]")]'), 'En Curso', 'Completa', 'Hiatus', 'Abandonada|Cancelada')
	MANGAINFO.Summary   = x.XPathString('//p[@class="line-clamp-3"]')

	local page = 1
	local pages = tonumber(x.XPathString('//nav[@aria-label="pagination"]/a[last()-1]/@aria-label'):match('%d+')) or 1
	while true do
		for v in x.XPath('//li[@class="w-full"]/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'):match('%d+'))
			MANGAINFO.ChapterNames.Add(x.XPathString('.//h3', v))
		end
		if page >= pages then break end
		page = page + 1
		if not HTTP.GET(MANGAINFO.URL .. '?pagina=' .. page) then break end
		x.ParseHTML(HTTP.Document)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. '/capitulo' .. URL .. '/'
	HTTP.Reset()
	HTTP.Headers.Values['X-Add-Nsfw-Cookie'] = 1

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "img")]/img/@src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ds42a85566244b7e836679491ce679e8'
	m.Name                     = 'Ikigai Mangas'
	m.RootURL                  = 'https://viralikigai.milkchoco.online'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end