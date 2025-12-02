----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'Spanish'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
	end

	AddWebsiteModule('019addd69b6276a7845e96f33cd6795b', 'MandaloAsiNoma', 'https://mandaloasinoma.com')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

--local Template = require 'templates.Madara'
-- XPathTokenAuthors = 'Author(s)'
-- XPathTokenArtists = 'Artist(s)'
-- XPathTokenGenres  = 'Genre(s)'
-- XPathTokenStatus  = 'Status'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()


	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()

	local u = MaybeFillHost(MODULE.RootURL, URL)
	local pages 

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	
	MANGAINFO.Title 	= x.XPathStringAll('//div/h1/text()[normalize-space()]')
	MANGAINFO.CoverLink = x.XPathString('//main/div/div/div/div/div/img/@src')
	MANGAINFO.Summary   = x.XPathString('//main//p/text()')

	MANGAINFO.ChapterLinks.Add(u)
	MANGAINFO.ChapterNames.Add('Capítulo 1')

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
    local u = MaybeFillHost(MODULE.RootURL, URL)
    
    if not HTTP.GET(u) then
        return net_problem
    end
    
    -- Extraer todo el contenido
    local content = HTTP.Document.ToString()
    
    -- Buscar todas las URLs de i.ibb.co que terminen en .webp
    local images = {}
    
    -- Patrón simple: cualquier URL que comience con https://i.ibb.co/ y termine en .webp
    for img_url in content:gmatch('(https?://i%.ibb%.co/[^"%s]+%.webp)') do
        table.insert(images, img_url)
    end
    
    -- Si no encontramos, buscar en el formato JSON escapado
    if #images == 0 then
        for img_url in content:gmatch('"src":"(https?://i%.ibb%.co/[^"]+%.webp)"') do
            img_url = img_url:gsub('\\/', '/')
            table.insert(images, img_url)
        end
    end
    
    -- Eliminar duplicados
    local seen = {}
    local unique_images = {}
    for _, img_url in ipairs(images) do
        if not seen[img_url] then
            seen[img_url] = true
            table.insert(unique_images, img_url)
        end
    end
    
    -- Ordenar
    table.sort(unique_images, function(a, b)
        -- Intentar extraer números
        local num_a = a:match('/(%d+)%.webp$') or 
                     (a:match('/blob%.webp$') and 0) or
                     (a:match('/00100%.webp$') and 1) or
                     999
                     
        local num_b = b:match('/(%d+)%.webp$') or 
                     (b:match('/blob%.webp$') and 0) or
                     (b:match('/00100%.webp$') and 1) or
                     999
                     
        return tonumber(num_a) < tonumber(num_b)
    end)
    
    -- Añadir a PageLinks
    for _, img_url in ipairs(unique_images) do
        TASK.PageLinks.Add(img_url)
    end
    
    if TASK.PageLinks.Count == 0 then
        print("No se encontraron imágenes. Contenido guardado en debug.html")
        HTTP.Document.SaveToFile("debug.html")
        return net_problem
    end
    
    return no_error
end

