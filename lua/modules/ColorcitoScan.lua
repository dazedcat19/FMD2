----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5450026642ecrrd48apotd81e57f8e40'
	m.Name                     = 'ColorcitoScan'
	m.RootURL                  = 'https://colorcitoscan.com'
	m.Category                 = 'Spanish'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryParameters = '/lista-de-mangas/'
API_URL = 'https://api.colorcitoscan.com'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local slug = URL:match('/ver/([^/]+)$')  -- Usa [^/]+ para capturar sin barras
	local u = API_URL .. '/serie/' .. slug

	if not HTTP.GET(u) then return net_problem end
    -- Obtener el contenido JSON como texto
    local jsonText = HTTP.Document.ToString()
    
    -- Sanitizar el JSON: reemplazar caracteres problemáticos
    jsonText = jsonText:gsub('<3»', '❤»')  -- Reemplazar <3» con emoji corazón
    jsonText = jsonText:gsub('<', '&lt;')   -- Escapar otros caracteres <
    jsonText = jsonText:gsub('>', '&gt;')   -- Escapar caracteres >
    
    -- Crear un nuevo documento con el JSON sanitizado
    local x = CreateTXQuery()
    x.ParseHTML(jsonText)  -- Parsear el texto JSON sanitizado
	MANGAINFO.Title     = x.XPathString('json(*).serie.name')
	MANGAINFO.CoverLink = x.XPathString('json(*).serie.urlImg')
	MANGAINFO.Authors   = x.XPathString('json(*).serie.author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).serie.genders().name')
	MANGAINFO.Status    = x.XPathString('json(*).serie.state.estado')
	MANGAINFO.Summary   = x.XPathString('json(*).serie.sinopsis')

	local v for v in x.XPath('json(*).serie.chapters()').Get() do
		MANGAINFO.ChapterLinks.Add(URL .. "/".. x.XPathString('slug', v))
		MANGAINFO.ChapterNames.Add("Cap. " .. x.XPathString('num', v))
	end

	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	local x = nil
	local u = MODULE.RootURL .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFAll('//div[contains(@class, "entry-content")]//li//a', LINKS, NAMES)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()

	if not HTTP.GET(MODULE.RootURL .. URL) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)
    x.XPathStringAll('//div[contains(@class, "max-w-4xl")]//img/@src', TASK.PageLinks)
    
    if TASK.PageLinks.Count == 0 then
        x.XPathStringAll('//img[contains(@src, "colorcito/serie/")]/@src', TASK.PageLinks)
    end
	
	return no_error
end
