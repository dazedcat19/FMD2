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
local DirectoryPagination = '/initial/'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function trim(s) 
	if not s then return '' end
	return (s:gsub('^%s*(.-)%s*$', '%1')) 
end

local function split(str, sep)
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
local function extractImages(encodedText, metaContent)
	if encodedText == '' then return {} end

	local sourceAlphabet = 'xXHbvV7snRpMFkrUPqlS4BzG3jg1aYC5WJ0wcZiLtoAyedQ8D2fTNOI9Eu6mhK'
	local targetAlphabet = 'EzCIUe3plcrfxuv9hKOsVtkTA6ZjaXRQJ0wWqb5D8gm1nG7LoH2dFyNYB4PiMS'

	local substituted = encodedText:gsub('[A-Za-z0-9]', function(ch)
		local idx = sourceAlphabet:find(ch, 1, true)
		if idx then
			return targetAlphabet:sub(idx, idx)
		end
		return ch
	end)

	local padding = (4 - #substituted % 4) % 4
	substituted = substituted .. string.rep('=', padding)

	local decoded = require 'fmd.crypto'.DecodeBase64(substituted)
	if not decoded then return {} end

	local urlList = split(decoded, ',')
	for i = 1, #urlList do urlList[i] = trim(urlList[i]) end

	if metaContent and metaContent ~= '' then
		local onlyDigits = metaContent:gsub('[^0-9]+', '-')
		local rawOrderList = split(onlyDigits, '-')

		local orderList = {}
		for _, v in ipairs(rawOrderList) do
			if v ~= '' then
				table.insert(orderList, v)
			end
		end

		if #orderList > 0 then
			local useReversedString = false
			for _, v in ipairs(orderList) do
				if v == '01' then
					useReversedString = true
					break
				end
			end

			local sortedUrls = {}
			for _, indexStr in ipairs(orderList) do
				local idx
				if useReversedString then
					idx = tonumber(indexStr:reverse())
				else
					idx = tonumber(indexStr)
				end
				if idx and urlList[idx + 1] then
					table.insert(sortedUrls, urlList[idx + 1])
				end
			end

			local reversedUrls = {}
			for i = #sortedUrls, 1, -1 do
				table.insert(reversedUrls, sortedUrls[i])
			end

			if #reversedUrls > 0 then
				return reversedUrls
			end
		end
	end

	return urlList
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
	x.XPathHREFTitleAll('//div[contains(@class, "cate-manga")]//div[contains(@class, "media-body")]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//ul[contains(@class, "pagination")]/li[last()-1]/a')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title-manga"]'):gsub(' Manga$', '')
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
	local metaContent = x.XPathString('//meta[@property="ad:check"]/@content')

	local images = extractImages(encoded, metaContent)
	for _, img in ipairs(images) do
		if img:sub(1, 2) == '//' then
			img = 'https:' .. img
		end
		local fixed = img:gsub('cdn.statically.io/img/', '')
		TASK.PageLinks.Add(fixed)
	end

	return true
end