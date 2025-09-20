----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'f8d26ca921af4876b7ba84bd7e06fe82'
	m.Name                     = 'NHentai'
	m.RootURL                  = 'https://nhentai.net'
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

local DirectoryPagination = '/?page='

local ext = {
	['g'] = '.gif',
	['j'] = '.jpg',
	['p'] = '.png',
	['w'] = '.webp'
}

-- Seed random number generator once.
math.randomseed(os.time())

----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

-- Decode unicode escape sequences.
function DecodeUnicode(str)
	return str:gsub('\\u(%x%x%x%x)', function(hex) return require('utf8').char(tonumber(hex, 16)) end)
end

-- Parse CDN URLs.
function ParseCdnUrls(str)
	local urls = {}
	local cdn = str:match('image_cdn_urls:%s*%[([^%]]+)%]')
	if cdn then
		for url in cdn:gmatch('"([^"]+)"') do
			table.insert(urls, url)
		end
	end
	return urls
end

-- Get a random CDN URL from the provided list.
function GetRandomCdn(cdn_urls)
	local random_index = math.random(1, #cdn_urls)
	return cdn_urls[random_index]
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//a[@class="last"]/@href/substring-after(., "=")')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@id="content"]/div[not(contains(@class, "popular"))]/div[@class="gallery"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL:gsub('(.*)/popular.*', '/%1'))
	if URL:find('/?page=', 1, true) then return no_error end

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title"]|//h1//span[@class="name"]|//input[@type="search"]/@value')
	MANGAINFO.AltTitles = x.XPathString('//h2[@class="title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@id="cover"]//img/@data-src|(//div[@class="container index-container"]//a[@class="cover"])[1]/img/@data-src')
	MANGAINFO.Artists   = x.XPathStringAll('//div[./text()="Artists:"]/span/a/span[@class="name"]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[./text()="Tags:"]/span/a/span[@class="name"]') .. ', ' .. x.XPathString('//div[./text()="Categories:"]/span/a/span[@class="name"]')

	local desc = {}

	local parodies = {}
	local parody = x.XPath('//div[./text()="Parodies:"]/span/a/span[@class="name"]')
	for i = 1, parody.Count do
		table.insert(parodies, parody.Get(i).ToString())
	end

	local characters = {}
	local character = x.XPath('//div[./text()="Characters:"]/span/a/span[@class="name"]')
	for i = 1, character.Count do
		table.insert(characters, character.Get(i).ToString())
	end

	local languages = {}
	local language = x.XPath('//div[./text()="Languages:"]/span/a/span[@class="name" and not(text()="translated")]')
	for i = 1, language.Count do
		table.insert(languages, language.Get(i).ToString())
	end

	local pages = x.XPathString('//div[./text()="Pages:"]/span/a/span')

	if #parodies > 0 then table.insert(desc, 'Parodies: ' .. table.concat(parodies, ', ')) end
	if #characters > 0 then table.insert(desc, 'Characters: ' .. table.concat(characters, ', ')) end
	if #languages > 0 then table.insert(desc, 'Languages: ' .. table.concat(languages, ', ')) end
	if pages ~= '' then table.insert(desc, 'Pages: ' .. pages) end
	MANGAINFO.Summary = table.concat(desc, '\r\n')

	if URL:find('/g/', 1, true) then
		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	else
		local page = 1
		local pages = tonumber(x.XPathString('//a[@class="last"]/@href/substring-after(., "=")')) or 1
		while true do
			for v in x.XPath('//div[@class="gallery"]/a').Get() do
				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(x.XPathString('div', v))
			end
			page = page + 1
			if page > pages then
				break
			elseif HTTP.GET(u .. '?page=' .. tostring(page)) then
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
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local image_cdn_urls = x.XPathString('//script[contains(., "image_cdn_urls")]')
	local cdn_urls = ParseCdnUrls(image_cdn_urls)

	x.ParseHTML(DecodeUnicode(GetBetween('parse("', '");', x.XPathString('//script[contains(., "_gallery")]'))))
	local id = x.XPathString('json(*).media_id')
	local pages = x.XPath('json(*).images.pages()')
	for i = 1, pages.Count do
		local ext_type = pages.Get(i).GetProperty('t').ToString()
		local cdn = GetRandomCdn(cdn_urls)
		TASK.PageLinks.Add(string.format('https://' .. cdn .. '/galleries/' .. id .. '/' .. i .. ext[ext_type]))
	end

	return true
end