----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd6c13b84c38342578d57086b5ca599cc'
	m.Name                     = 'WolfManga'
	m.RootURL                  = 'https://www.wolfmanga.com'
	m.Category                 = 'Spanish'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
    m.OnDownloadImage          = 'DownloadImage'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryParameters = '/lista-de-mangas/'
API_URL = 'https://www.wolfmanga.com/_ex/series'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function GetNameAndLink()    
    local api = 'https://www.wolfmanga.com/_ex/series/?page=' .. (URL + 1)
    HTTP.MimeType = 'application/json'

    if not HTTP.GET(api) then return net_problem end

    local json = CreateTXQuery(HTTP.Document)

    for v in json.XPath('json(*)()').Get() do
        local slug  = json.XPathString('slug', v)
        local title = json.XPathString('title', v)

        if slug ~= '' and title ~= '' then
            LINKS.Add('/series/' .. slug)
            NAMES.Add(title)
        end
    end

    -- Paginación
    if json.XPathString('json(*).meta.has_next') == 'true' then
        UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1
    else
        UPDATELIST.CurrentDirectoryPageNumber = 0    
    end

    return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
    local slug = URL:match('/series/([^/]+)')
    local api = API_URL .. '/' .. slug

    if not HTTP.GET(api) then return net_problem end

    local json = CreateTXQuery(HTTP.Document)

    -- Info básica
    MANGAINFO.Title     = json.XPathString('json(*).title')
    MANGAINFO.AltTitles = json.XPathStringAll('json(*).subtitle')
    MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, json.XPathString('json(*).cover'))
    MANGAINFO.Authors   = json.XPathStringAll('json(*).authors')
	MANGAINFO.Artists   = json.XPathStringAll('json(*).artists')
    MANGAINFO.Summary   = json.XPathString('json(*).description')
    MANGAINFO.Genres    = json.XPathStringAll('json(*).genres().nombre')
    MANGAINFO.Status    = json.XPathString('json(*).serie.estado')
    
    -- Capítulos
    for v in json.XPath('json(*).chapters()').Get() do
        local cap_id    = json.XPathString('id', v)
        local cap_num   = json.XPathString('number', v)
        local cap_title = json.XPathString('title', v)
        local link      = '/series/' .. slug .. '/' .. cap_num .. '/' .. cap_id

        MANGAINFO.ChapterLinks.Add(link)
        MANGAINFO.ChapterNames.Add('Capítulo ' .. cap_num .. (cap_title ~= '' and ' - ' .. cap_title or ''))
    end

    MANGAINFO.ChapterLinks.Reverse()
    MANGAINFO.ChapterNames.Reverse()

    return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
    local slug, cap_num, cap_id = URL:match('/series/([^/]+)/([^/]+)/([^/]+)')
    local api = API_URL .. '/' .. slug

    if not HTTP.GET(api) then return net_problem end

    local json = CreateTXQuery(HTTP.Document)
    local chapter_found = false

    for v in json.XPath('json(*).chapters()').Get() do
        local id = json.XPathString('id', v)
        print('Checking chapter ID: ' .. id .. ' against ' .. cap_id)
        if id == cap_id then
            chapter_found = true

            for page in json.XPath('pages?*', v).Get() do
                local page_type = json.XPathString('type', page)
                local img_url

                if page_type == 'scrambled' then
                    img_url = json.XPathString('src', page)
                
                    local grid = tonumber(json.XPathString('scramble/grid', page)) or 4
                
                    local map_array = {}
                
                    for item in json.XPath('scramble/map?*', page).Get() do
                        map_array[#map_array + 1] = item.ToString()
                    end
                
                    local map_str = table.concat(map_array, ',')
                
                    TASK.PageLinks.Add(
                        MaybeFillHost(MODULE.RootURL, img_url)
                        .. '#grid=' .. grid
                        .. '&map=' .. map_str
                    )
                else
                    img_url = json.XPathString('.', page)
                    if not img_url:lower():match('%.svg[%?#]?') then
                        TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, img_url))
                    end 
                end
            end
            break
        end
    end

    if not chapter_found then return net_problem end

    return no_error
end

function DownloadImage()
    -- Primero, comprobar si la URL tiene parámetros de descramble
    local url_without_params, params = URL:match('^(.-)%?(.*)$')
    if not url_without_params then
        url_without_params = URL
        params = ''
    end

    -- Buscar el fragmento #grid=...&map=...
    local base_url, fragment = url_without_params:match('^(.-)#(.*)$')
    if not base_url then
        base_url = url_without_params
        fragment = ''
    end

    -- Descargar la imagen (usando la URL base, sin fragmento)
    if not HTTP.GET(base_url) then return false end

    -- Procesar el fragmento si existe
    if fragment ~= '' then
        local grid, map_str = fragment:match('grid=(%d+)&map=([^&]+)')
        if grid and map_str then
            grid = tonumber(grid)
            if grid and grid > 0 then
                -- Convertir map_str en tabla de enteros
                local map = {}
                for num in map_str:gmatch('(%d+)') do
                    map[#map+1] = tonumber(num)
                end
                local total_tiles = grid * grid
                if #map == total_tiles then
                    local puzzle = require 'fmd.imagepuzzle'.Create(grid, grid)
                    -- Asignar la matriz: para cada tile origen (src), indicar destino (map[src+1])
                    for src = 0, total_tiles - 1 do
                        puzzle.Matrix[src] = map[src + 1]
                    end
                    puzzle.DeScramble(HTTP.Document, HTTP.Document)
                end
            end
        end
    end

    return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end
