----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '67e22e0c766c4c9c990e179350262b3c'
	m.Name                     = 'IMHentai'
	m.RootURL                  = 'https://imhentai.xxx'
	m.Category                 = 'H-Sites'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/?page='

----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

local ext = {
	['p'] = '.png',
	['b'] = '.bmp',
	['g'] = '.gif',
	['w'] = '.webp',
	['j'] = '.jpg'
}

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a'))

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//h2[@class="gallery_title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local pages, x = nil
	local p = 1
	local u = MaybeFillHost(MODULE.RootURL, URL:gsub('(.*)?page.*', '%1'):gsub('(.*)popular/.*', '%1'))
	if u:find(MODULE.RootURL .. '/?page=') then return net_problem end

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[contains(@class, "right_details")]/h1|//h1[@class="sub_title"]/span')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "left_cover")]//img/@data-src|(//div[@class="thumbs_container"]/div[@class="thumb"])[1]//img/@data-src')
	MANGAINFO.Artists   = x.XPathStringAll('//ul[@class="galleries_info"]/li[contains(., "Artists")]/a/text()')
	MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="galleries_info"]/li[not(contains(., "Artists"))]/a/text()')

	if u:find('/gallery/') then
		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	else
		pages = tonumber(x.XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1
		while true do
			x.XPathHREFAll('//h2[@class="gallery_title"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			p = p + 1
			if p > pages then
				break
			elseif HTTP.GET(u .. '?page=' .. tostring(p)) then
				x.ParseHTML(HTTP.Document)
			else
				break
			end
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local dir, i, i1, i2, id, json, pages, server, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	dir    = x.XPathString('//input[@id="load_dir"]/@value')
	id     = x.XPathString('//input[@id="load_id"]/@value')
	server = x.XPathString('//input[@id="load_server"]/@value')
	json   = GetBetween("parseJSON('{", "');", x.XPathString('//script[contains(., "var g_th")]'))
	json   = json:gsub('","', ';'):gsub('"}', ';'):gsub(':', ','):gsub('"', '')
	for i in json:gmatch('(.-);') do
		i1, i2 = i:match('(.-),(.-),.-,.-')
		TASK.PageLinks.Add('https://m' .. server .. '.imhentai.xxx/' .. dir .. '/' .. id .. '/' .. i1 .. ext[i2])
	end

	return no_error
end