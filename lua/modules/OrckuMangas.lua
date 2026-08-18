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
function GetLastPage(x)

    local last = 1

    for v in x.XPath('//div[contains(@class,"flex-wrap")]//a[contains(@href,"page=")]').Get() do

        local href = v.GetAttribute('href')

        local n = tonumber(href:match('[?&]page=(%d+)'))

        if n and n > last then
            last = n
        end

    end

    return last

end

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

	HTTP.Cookies.Values['orcku_mayor_edad'] = 1

	if not HTTP.GET(u) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)

    MANGAINFO.Title     = x.XPathString('//h1/text()')
    MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[contains(@class,"w-full")]/img[contains(@src,"uploads/covers")]/@src'))
    MANGAINFO.Authors   = x.XPathString('//div[span[contains(text(),"Autor:")]]/text()')
    MANGAINFO.Artist    = x.XPathString('//div[span[contains(text(),"Artista:")]]/text()')
    MANGAINFO.Genres    = x.XPathStringAll('//a[contains(@href,"genre=")]')
    MANGAINFO.Summary   = x.XPathString('//p[contains(@class,"text-gray-300")]')

	-- obtener total de páginas
	local totalPages = GetLastPage(x)

	-- recorrer páginas
	local page

	for page = 1, totalPages do

		local pageUrl

		if page > 1 then
			if u:find('?') then
				if u:find('page=') then
					pageUrl = u:gsub('page=%d+', 'page=' .. page)
				else
					pageUrl = u .. '&page=' .. page
				end
			else
				pageUrl = u .. '?page=' .. page
			end
		else
			pageUrl = u
		end

		HTTP.Reset()
		HTTP.Cookies.Values['orcku_mayor_edad'] = 1

		if HTTP.GET(pageUrl) then

			x.ParseHTML(HTTP.Document)

			for v in x.XPath('//a[contains(@href,"capitulo")]').Get() do
				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(x.XPathString('.//div[@class="cap-num"]/text()', v))
			end
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	HTTP.Reset()
	HTTP.Cookies.Values['orcku_mayor_edad'] = 1
	if not HTTP.GET(u) then return net_problem end
	
	local x = CreateTXQuery(HTTP.Document)

    local v for v in x.XPath('//div[contains(@class, "image-container")]/img').Get() do
        TASK.PageLinks.Add(MODULE.RootURL .. '/' .. v.GetAttribute('src'))
    end	

	return no_error
end
