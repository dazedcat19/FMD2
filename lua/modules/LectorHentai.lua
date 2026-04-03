function Init()
    local m = NewWebsiteModule()
    m.ID                         = '4ac85000451741f990569381e0e10752'
    m.Name                       = 'LectorHentai'
    m.RootURL                    = 'https://lectorhentai.com'
    m.Category                   = 'Spanish'
    m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
    m.OnGetNameAndLink           = 'GetNameAndLink'
    m.OnGetInfo                  = 'GetInfo'
    m.OnGetPageNumber            = 'GetPageNumber'
    m.OnBeforeDownloadImage      = 'BeforeDownloadImage'
end

-- Número de páginas (aproximado hasta que no haya resultados)
function GetDirectoryPageNumber()
    PAGENUMBER = 9999 -- no hay paginación clara
    return no_error
end

-- Lista de mangas
function GetNameAndLink()
    if HTTP.GET(MODULE.RootURL .. '/tipo/all?lenguaje=all&page=' .. (URL + 1)) then
        local x = CreateTXQuery(HTTP.Document)

        x.XPathHREFTitleAll(
            '//div[@class="listupd"]//div[@class="bsx"]/a',
            LINKS, NAMES
        )

        return no_error
    else
        return net_problem
    end
end

-- Info del manga
function GetInfo()
    MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)

    if not HTTP.GET(MANGAINFO.URL) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)

    MANGAINFO.Title     = x.XPathString('//b[contains(.,"Título")]/following-sibling::span')
    MANGAINFO.CoverLink = 'https:' .. x.XPathString('//div[@class="bigcover"]//img/@src')
    MANGAINFO.Authors = x.XPathString('//b[contains(.,"Artista")]/following-sibling::span')
    MANGAINFO.Genres = x.XPathStringAll('//b[contains(.,"Generos")]/following-sibling::span/a')
    MANGAINFO.Summary = MANGAINFO.Title

    -- SOLO 1 capitulo
    local link = x.XPathString('//a[contains(@class,"leer")]/@href')

    if link ~= '' then
        MANGAINFO.ChapterLinks.Add(link)
        MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
    end

    return no_error

end

-- Extraer imágenes
function GetPageNumber()
    if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

    local html = HTTP.Document.ToString()

    -- Extraer JSON dentro de ts_reader.run(...)
    local json = html:match('ts_reader%.run%((.-)%)')

    if json then
        -- Extraer URLs de imágenes
        for img in json:gmatch('"//(.-)"') do
            TASK.PageLinks.Add('https://' .. img)
        end
    end

    return true

end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])
	return true
end
