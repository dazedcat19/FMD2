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

AlphaList = '0123456789abcdefghijklmnopqrstuvwxyz'
local DirectoryPagination = '/initial/'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function Trim(s) 
	if not s then return '' end
	return (s:gsub('^%s*(.-)%s*$', '%1')) 
end

local function Split(str, sep)
	local t = {}
	if str == nil or str == '' then return t end
	sep = sep or ','
	local last = 1
	while true do
		local startpos, endpos = str:find(sep, last, true)
		if not startpos then
			table.insert(t, str:sub(last))
			break
		end
		table.insert(t, str:sub(last, startpos - 1))
		last = endpos + 1
	end
	return t
end

-- Main image-URL decryption
local function ExtractImages(encoded, meta_content)
	if encoded == '' then return {} end

	local source_alphabet = 'xXHbvV7snRpMFkrUPqlS4BzG3jg1aYC5WJ0wcZiLtoAyedQ8D2fTNOI9Eu6mhK'
	local target_alphabet = 'EzCIUe3plcrfxuv9hKOsVtkTA6ZjaXRQJ0wWqb5D8gm1nG7LoH2dFyNYB4PiMS'

	local substituted = encoded:gsub('[A-Za-z0-9]', function(ch)
		local idx = source_alphabet:find(ch, 1, true)
		if idx then
			return target_alphabet:sub(idx, idx)
		end
		return ch
	end)

	local padding = (4 - #substituted % 4) % 4
	substituted = substituted .. string.rep('=', padding)

	local decoded = require 'fmd.crypto'.DecodeBase64(substituted)
	if not decoded then return {} end

	local url_list = Split(decoded, ',')
	for i = 1, #url_list do url_list[i] = Trim(url_list[i]) end

	if meta_content and meta_content ~= '' then
		local only_digits = meta_content:gsub('[^0-9]+', '-')
		local raw_order_list = Split(only_digits, '-')

		local order_list = {}
		for _, v in ipairs(raw_order_list) do
			if v ~= '' then
				table.insert(order_list, v)
			end
		end

		if #order_list > 0 then
			local use_reversed_string = false
			for _, v in ipairs(order_list) do
				if v == '01' then
					use_reversed_string = true
					break
				end
			end

			local sorted_urls = {}
			for _, index_str in ipairs(order_list) do
				local idx
				if use_reversed_string then
					idx = tonumber(index_str:reverse())
				else
					idx = tonumber(index_str)
				end
				if idx and url_list[idx + 1] then
					table.insert(sorted_urls, url_list[idx + 1])
				end
			end

			local reversed_urls = {}
			for i = #sorted_urls, 1, -1 do
				table.insert(reversed_urls, sorted_urls[i])
			end

			if #reversed_urls > 0 then
				return reversed_urls
			end
		end
	end

	return url_list
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local i = MODULE.CurrentDirectoryIndex + 1
	local s = AlphaList:sub(i, i)
	local u = MODULE.RootURL .. DirectoryPagination .. s .. '/?page=' .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	x.XPathHREFTitleAll('//div[@class="cate-manga"]//div[@class="media-body"]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//ul[@class="pagination"]/li[last()]/a')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title-manga"]')
	MANGAINFO.AltTitles = x.XPathString('//p[contains(@class, "description-update")]/span[contains(., "Títulos Alternativos")]/following-sibling::text()[1]')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[contains(@class, "cover-detail")]/img/@src'))
	MANGAINFO.Authors   = x.XPathString('//p[contains(@class, "description-update")]/span[contains(., "Author")]/following-sibling::text()[1]')
	MANGAINFO.Artists   = x.XPathString('//p[contains(@class, "description-update")]/span[contains(., "Artist")]/following-sibling::text()[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//p[contains(@class, "description-update")]//a[contains(@href, "/genre/")]/text()')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[contains(@class, "description-update")]/span[contains(., "Estado:")]/following-sibling::text()[1]'), 'Ongoing', 'Completed', 'Paused', 'Cancelled')
	MANGAINFO.Summary   = x.XPathString('//div[@class="manga-content"]/p')

	for v in x.XPath('//div[contains(@class, "chapter-list")]//a[contains(@class, "xanh")]').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(v.GetAttribute('title'))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)

	local encoded = x.XPathString('//p[@id="array_data"]')
	local meta_content = x.XPathString('//meta[@property="ad:check"]/@content')

	local images = ExtractImages(encoded, meta_content)
	for _, img in ipairs(images) do
		if img:sub(1, 2) == '//' then
			img = 'https:' .. img
		end
		local fixed = img:gsub('cdn.statically.io/img/', '')
		TASK.PageLinks.Add(fixed)
	end

	return true
end