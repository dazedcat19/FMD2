----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'dfdec22299bc4fc6adbad401eeca2211'
	m.Name                     = 'HentaiRead'
	m.RootURL                  = 'https://hentairead.com'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/hentai/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[contains(@class, "wp-pagenavi")]/a[@class="last"]/@href'):match('/(%d+)/')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[contains(@class, "manga-item__wrapper")]/div[3]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if URL:find('/page/', 1, true) or URL:find('sortby=', 1, true) then return net_problem end

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="manga-titles grow"]/h1|//div[@class="container"]//h1')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="manga-titles grow"]/h2')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "items-center gap-7")]/a/img/@src')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@class="flex flex-wrap items-center gap-2" and ./div="Artist:"]/a/span[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="flex flex-wrap items-center gap-2" and ./div="Tags:"]/a/span[1]') .. ', ' .. x.XPathString('//div[@class="flex flex-wrap items-center gap-2" and ./div="Category:"]/a/span[1]')

	local desc = {}

	local circle = {}
	local circle_nodes = x.XPath('//div[@class="flex flex-wrap items-center gap-2" and ./div="Circle:"]/a/span[1]')
	for i = 1, circle_nodes.Count do
		table.insert(circle, circle_nodes.Get(i).ToString())
	end

	local characters = {}
	local chara_nodes = x.XPath('//div[@class="flex flex-wrap items-center gap-2" and ./div="Character:"]/a/span[1]')
	for i = 1, chara_nodes.Count do
		table.insert(characters, chara_nodes.Get(i).ToString())
	end

	local scanlator = {}
	local scan_nodes = x.XPath('//div[@class="flex flex-wrap items-center gap-2" and ./div="Scanlator:"]/a/span[1]')
	for i = 1, scan_nodes.Count do
		table.insert(scanlator, scan_nodes.Get(i).ToString())
	end

	local parody = x.XPathString('//div[@class="flex flex-wrap items-center gap-2" and ./div="Parody:"]/a/span[1]')
	local convention = x.XPathString('//div[@class="flex flex-wrap items-center gap-2" and ./div="Convention:"]/a/span[1]')
	local year = x.XPathString('//div[@class="flex flex-wrap items-center gap-2" and ./div="Release Year:"]/a/span[1]')
	local pages = x.XPathString('//div[@class="flex flex-wrap items-center gap-2" and ./div="Pages:"]/div/span/span')
	local language = x.XPathString('//div[@class="flex flex-wrap items-center gap-2" and ./div="Language:"]/a/span[1]')

	if parody ~= '' then table.insert(desc, 'Parody: ' .. parody) end
	if convention ~= '' then table.insert(desc, 'Convention: ' .. convention) end
	if #circle > 0 then table.insert(desc, 'Circle: ' .. table.concat(circle, ', ')) end
	if #characters > 0 then table.insert(desc, 'Characters: ' .. table.concat(characters, ', ')) end
	if #scanlator > 0 then table.insert(desc, 'Scanlator: ' .. table.concat(scanlator, ', ')) end
	if year ~= '' then table.insert(desc, 'Release Year: ' .. year) end
	if pages ~= '' then table.insert(desc, 'Pages: ' .. pages) end
	if language ~= '' then table.insert(desc, 'Language: ' .. language) end
	MANGAINFO.Summary = table.concat(desc, '\r\n')

	if URL:find('/hentai/', 1, true) then
		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	else
		MANGAINFO.CoverLink = x.XPathString('(//div[@class="manga-item__img"])[1]/img/@src')
		local page = 1
		local pages = tonumber(x.XPathString('//span[@class="pages"]/substring-after(., "Page 1 of ")')) or 1
		while true do
			x.XPathHREFAll('//div[contains(@class, "manga-item__wrapper")]/div[3]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			page = page + 1
			if page > pages then
				break
			elseif HTTP.GET(MANGAINFO.URL .. '/page/' .. tostring(page)) then
				x.ParseHTML(HTTP.Document)
			else
				break
			end
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="lazy-listing"]/ul//img/@src/replace(., "https://hencover.xyz/preview/", "https://henread.xyz/")', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end