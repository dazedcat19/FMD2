----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '792e61d922fb442ba3cb13f2f5f26c1d'
	m.Name                     = 'OrckuMangas'
	m.RootURL                  = 'https://orckumangas.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/biblioteca.php?genre=0&type=&status=&page='
-- XPathTokenAuthors   = 'Author'
-- XPathTokenArtists   = 'Artist'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
    local page = 1

    while true do
        local url = MODULE.RootURL .. DirectoryPagination .. page

        UPDATELIST.UpdateStatusText('Loading page ' .. page)

        if not HTTP.GET(url) then break end

        local x = CreateTXQuery(HTTP.Document)

        -- Extraer mangas
        local v
        for v in x.XPath('//div[contains(@class,"card")]/a').Get() do
            LINKS.Add(v.GetAttribute('href'))
            NAMES.Add(x.XPathString('.//div/h3', v))
        end

        -- Verificar si existe la siguiente página
        local next_exists = x.XPathString('//a[text()="' .. (page + 1) .. '"]')

        if next_exists == '' then break end

        page = page + 1
    end

    return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not HTTP.GET(u) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)

    MANGAINFO.Title     = x.XPathString('//h1/text()')
    MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[contains(@class,"cover")]/@src'))
    MANGAINFO.Authors   = x.XPathString('//div[span[contains(text(),"Autor:")]]/text()')
    MANGAINFO.Artist    = x.XPathString('//div[span[contains(text(),"Artista:")]]/text()')
    MANGAINFO.Genres    = x.XPathStringAll('//a[contains(@href,"genre=")]')
    MANGAINFO.Summary   = x.XPathString('//p[contains(@class,"text-gray-300")]')

    local v for v in x.XPath('//a[contains(@href, "capitulo.php")]').Get() do
        MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
        MANGAINFO.ChapterNames.Add(x.XPathString('.//span/text()', v)) 
    end

    return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end
	
	local x = CreateTXQuery(HTTP.Document)

    local v for v in x.XPath('//div[contains(@class, "image-container")]/img').Get() do
        TASK.PageLinks.Add(MODULE.RootURL .. '/' .. v.GetAttribute('src'))
    end	

	return no_error
end
