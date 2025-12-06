----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'English'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.TotalDirectory           = AlphaList:len()
	end
	AddWebsiteModule('8212f7c50ebe478bb344d16e8ab20adc', 'MangaHereToday', 'http://mangahere.today')
	AddWebsiteModule('d1958f8b85cb494abe69deb151d1a89d', 'MangaNelos', 'http://manganelos.com')

	cat = 'Spanish'
	AddWebsiteModule('c67d163c51b24bc498e777e2b0d810d2', 'LeerCapitulo', 'https://www.leercapitulo.re')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

AlphaList = '#abcdefghijklmnopqrstuvwxyz'
DirectoryPagination = '/alpha/'
base64 = require "fmd.crypto"

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local s, i, x
	if MODULE.CurrentDirectoryIndex == 0 then
		s = '9'
	else
		i = MODULE.CurrentDirectoryIndex + 1
		s = AlphaList:sub(i, i)
	end
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. s .. '/?page=' .. (URL + 1)) then return net_problem end
	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFTitleAll('//div[@class="cate-manga"]//div[@class="media-body"]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//div[@class="pagination"]//li[last()-1]')) or 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title-manga"]'):gsub(' Manga$', '')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "cover-detail")]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//p[@class="description-update"]/span[contains(., "Author")]/following-sibling::text()[1]')
	MANGAINFO.Artists   = x.XPathString('//p[@class="description-update"]/span[contains(., "Artist")]/following-sibling::text()[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//p[@class="description-update"]/span[contains(., "Genre")]/following-sibling::a/text()')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[@class="description-update"]/span[contains(., "Status")]/following-sibling::text()[1]'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="manga-content"]/p')

	x.XPathHREFAll('//div[@class="total-chapter"]//div[@class="chapter-list"]//h4/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Helper: trim
local function trim(s) return (s:gsub("^%s*(.-)%s*$", "%1")) end

-- Helper: split by separator (returns table)
local function split(str, sep)
    local t = {}
    if str == nil or str == "" then return t end
    sep = sep or ","
    local pattern = "([^" .. sep .. "]*)"
    local i = 1
    local last = 1
    local s = str
    while true do
        local startpos, endpos = s:find(sep, last, true)
        if not startpos then
            local part = s:sub(last)
            table.insert(t, part)
            break
        else
            local part = s:sub(last, startpos-1)
            table.insert(t, part)
            last = endpos + 1
        end
    end
    return t
end

-- Reverse string
local function str_reverse(s)
    return s:reverse()
end

-- Main: recibe encodedText (string) y opcional metaContent (string).
-- Devuelve tabla con URLs.
local function extractLeerCapituloImagesFromStrings(encodedText, metaContent)
    if not encodedText or encodedText == "" then return {} end

    -- 1) sustitución de alfabetos
    local sourceAlphabet = "xXHbvV7snRpMFkrUPqlS4BzG3jg1aYC5WJ0wcZiLtoAyedQ8D2fTNOI9Eu6mhK"
    local targetAlphabet = "EzCIUe3plcrfxuv9hKOsVtkTA6ZjaXRQJ0wWqb5D8gm1nG7LoH2dFyNYB4PiMS"

    local substituted = encodedText:gsub(".", function(ch)
        local idx = sourceAlphabet:find(ch, 1, true)
        if idx then
            return targetAlphabet:sub(idx, idx)
        else
            return ch
        end
    end)

    -- 2) base64 decode
    local decodedBase64 = base64.DecodeBase64(substituted)
    if not decodedBase64 then return {} end

    -- 3) split por coma
    local L = split(decodedBase64, ",")
    for i = 1, #L do L[i] = trim(L[i]) end

    -- debug: conteos
    -- print("decoded length:", #L)

    -- si no tenemos metaContent, devolvemos L (no reordenamiento)
    if not metaContent or metaContent == "" then
        return L
    end

    -- 4) obtener secuencia de reordenamiento EXACTO como JS:
    local onlyDigits = metaContent:gsub("[^0-9]+", "-")
    -- split("") → chars
    local chars = {}
    for c in onlyDigits:gmatch(".") do table.insert(chars, c) end
    -- reverse chars
    local reversedChars = {}
    for i = #chars, 1, -1 do table.insert(reversedChars, chars[i]) end
    local reversed = table.concat(reversedChars)
    -- split("-")
    local e = split(reversed, "-")

    -- debug: muestra secuencia producida por JS
    -- for i, v in ipairs(e) do print("e["..i.."]='"..v.."'") end

    -- 5) reconstruir j según índices válidos
    local j = {}
    for _, indexStr in ipairs(e) do
        -- parseInt equivalent: tonumber ignores leading zeros correctly
        local idx = tonumber(indexStr)
        if idx ~= nil then
            local lua_idx = idx + 1         -- IMPORTANT: convert 0-based JS index -> 1-based Lua index
            if L[lua_idx] then
                table.insert(j, trim(L[lua_idx]))
                -- debug:
                -- print(("using idx JS=%s -> lua=%d : %s"):format(tostring(indexStr), lua_idx, trim(L[lua_idx])))
            else
                -- debug: índice válido numérico pero fuera de rango
                -- print(("index out of range JS=%s -> lua=%d (Lsize=%d)"):format(tostring(indexStr), lua_idx, #L))
            end
        else
            -- no es número (como cadenas vacías) -> ignorar (igual que parseInt en JS)
        end
    end

    -- Si j quedó vacío devolvemos L (comportamiento original)
    if #j > 0 then
        return j
    end

    return L
end

-- Reemplaza tu GetPageNumber con esto (usa x.XPathString para meta)
function GetPageNumber()
    if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end
    local x = CreateTXQuery(HTTP.Document)

	if MODULE.Name == 'LeerCapitulo' then
		-- extrae el texto ofuscado del <p id="array_data">
		local encoded = x.XPathString('//p[@id="array_data"]/text()')
		if not encoded or encoded == "" then
			-- intento alternativo sin /text()
			encoded = x.XPathString('//p[@id="array_data"]')
		end

		-- obtener contenido de la última meta (igual que en JS: [...document.querySelectorAll('meta')].pop().content)
		local metaContent = x.XPathString('//meta[last()]/@content')
		if not metaContent or metaContent == "" then
			-- intento alternativo: todas las metas como cadena y tomar la última
			metaContent = x.XPathString('(//meta)[last()]/@content')
		end

		-- decodificar y obtener tabla de imágenes
		local images = extractLeerCapituloImagesFromStrings(encoded, metaContent)

		-- añadir a TASK.PageLinks (si el sitio usa cdn.statically, se eliminó en tu ejemplo)
		for _, img in ipairs(images) do
			local fixed = img:gsub("cdn.statically.io/img/", "")
			TASK.PageLinks.Add(fixed)
		end	
	else
		local json = x.XPathString('//p[@id="arraydata"]')
		for i in json:gmatch('(.-),') do
			TASK.PageLinks.Add(i:gsub("cdn.statically.io/img/", ""))
		end	
	end

    return no_error
end
