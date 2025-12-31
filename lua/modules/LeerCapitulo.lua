----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c67d163c51b24bc498e777e2b0d810d2'
	m.Name                     = 'LeerCapitulo'
	m.RootURL                  = 'https://www.leercapitulo.co'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.TotalDirectory           = AlphaList:len()
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

AlphaList = '#abcdefghijklmnopqrstuvwxyz'
local DirectoryPagination = '/alpha/'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Trim
local function trim(s) return (s:gsub("^%s*(.-)%s*$", "%1")) end

-- Split by separator (returns table)
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

-- Main: recibe encodedText (string) y opcional metaContent (string).
-- Devuelve tabla con URLs.
local function extractLeerCapituloImagesFromStrings(encodedText, metaContent)
	if not encodedText or encodedText == "" then return {} end

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

	local decodedBase64 = require 'fmd.crypto'.DecodeBase64(substituted)
	if not decodedBase64 then return {} end

	local L = split(decodedBase64, ",")
	for i = 1, #L do L[i] = trim(L[i]) end

	if not metaContent or metaContent == "" then
		return L
	end

	local onlyDigits = metaContent:gsub("[^0-9]+", "-")

	local chars = {}
	for c in onlyDigits:gmatch(".") do table.insert(chars, c) end

	local reversedChars = {}
	for i = #chars, 1, -1 do table.insert(reversedChars, chars[i]) end
	local reversed = table.concat(reversedChars)
	local e = split(reversed, "-")

	local j = {}
	for _, indexStr in ipairs(e) do
		local idx = tonumber(indexStr)
		if idx ~= nil then
			local lua_idx = idx + 1
			if L[lua_idx] then
				table.insert(j, trim(L[lua_idx]))
			end
		end
	end

	if #j > 0 then
		return j
	end

	return L
end

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
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

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

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local encoded = x.XPathString('//p[@id="array_data"]/text()')
	if encoded == '' then
		encoded = x.XPathString('//p[@id="array_data"]')
	end

	local metaContent = x.XPathString('//meta[last()]/@content')
	if metaContent == '' then
		metaContent = x.XPathString('(//meta)[last()]/@content')
	end

	local images = extractLeerCapituloImagesFromStrings(encoded, metaContent)
	for _, img in ipairs(images) do
		local fixed = img:gsub("cdn.statically.io/img/", "")
		TASK.PageLinks.Add(fixed)
	end

    return true
end