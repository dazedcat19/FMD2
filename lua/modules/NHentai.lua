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
	m.MaxTaskLimit             = 2
	m.MaxConnectionLimit       = 5
	m.SortedList               = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['pretty'] = 'Use pretty title'
		},
		['id_ID'] = {
			['pretty'] = 'Gunakan judul menarik'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('pretty', lang.pretty, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api/v2/galleries'
local DirectoryPagination = '?per_page=100&page='

local image_servers = {
	'i1.nhentai.net',
	'i2.nhentai.net',
	'i3.nhentai.net',
	'i4.nhentai.net'
}

local thumb_servers = {
	't1.nhentai.net',
	't2.nhentai.net',
	't3.nhentai.net',
	't4.nhentai.net'
}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Seed random number generator once.
math.randomseed(os.time())

-- Allow bursts requests per minute.
local function RateLimit(request)
    local limit = request
    local window = 60 -- seconds

    local now = os.time()

    local raw = MODULE.Storage['req_times']
    local times = {}

    if raw then
        for t in string.gmatch(raw, '(%d+)') do
            times[#times + 1] = tonumber(t)
        end
    end

    local i = 1
    while i <= #times do
        if now - times[i] >= window then
            table.remove(times, i)
        else
            i = i + 1
        end
    end

    if #times >= limit then
        local wait_time = window - (now - times[1])
        if wait_time > 0 then
            sleep(wait_time * 1000)
        end
        now = os.time()
    end

    times[#times + 1] = now

    MODULE.Storage['req_times'] = table.concat(times, ',')
end

-- Get a random server from the provided list.
local function GetRandomServer(server_list)
	return server_list[math.random(#server_list)]
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).num_pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	RateLimit(60)
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).result()').Get() do
		LINKS.Add('g/' .. v.GetProperty('id').ToString() .. '/')
		NAMES.Add(v.GetProperty('english_title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	if URL:find('/g/', 1, true) then
		RateLimit(45)
		local mid = URL:match('%d+')
		local u = MODULE.RootURL .. API_URL .. '/' .. mid

		if not HTTP.GET(u) then return net_problem end

		local s = HTTP.Document.ToString()
		MODULE.Storage[mid] = s
		local x = CreateTXQuery(s)
		local data = x.XPath('json(*)')
		MANGAINFO.Title     = MODULE.GetOption('pretty') and x.XPathString('title?pretty', data) or x.XPathString('title?english', data)
		MANGAINFO.AltTitles = x.XPathString('title?japanese', data)
		MANGAINFO.CoverLink = GetRandomServer(thumb_servers) .. '/' .. x.XPathString('cover?path', data)
		MANGAINFO.Artists   = x.XPathString('string-join(tags?*[type="artist"]?name, ", ")', data)
		MANGAINFO.Genres    = x.XPathString('string-join((tags?*[type="tag"]?name, tags?*[type="category"]?name), ", ")', data)

		local desc = {}

		local parodies = {}
		for parody in x.XPath('tags?*[type="parody"]?name', data).Get() do
			parodies[#parodies + 1] = parody.ToString()
		end

		local characters = {}
		for character in x.XPath('tags?*[type="character"]?name', data).Get() do
			characters[#characters + 1] = character.ToString()
		end

		local languages = {}
		for language in x.XPath('tags?*[type="language" and not(name="translated")]?name', data).Get() do
			languages[#languages + 1] = language.ToString()
		end

		local pages = x.XPathString('num_pages', data)

		if #parodies > 0 then table.insert(desc, 'Parodies: ' .. table.concat(parodies, ', ')) end
		if #characters > 0 then table.insert(desc, 'Characters: ' .. table.concat(characters, ', ')) end
		if #languages > 0 then table.insert(desc, 'Languages: ' .. table.concat(languages, ', ')) end
		if pages ~= '' then table.insert(desc, 'Pages: ' .. pages) end
		MANGAINFO.Summary = table.concat(desc, '\r\n')

		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	else
		local u = MaybeFillHost(MODULE.RootURL, URL:gsub('(.*)/popular.*', '/%1'))
		if URL:find('/?page=', 1, true) then return no_error end

		if not HTTP.GET(u) then return net_problem end

		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('(//h1//span[@class="name"], //title ! substring-before(.," - Search"))')
		MANGAINFO.CoverLink = x.XPathString('(//div[contains(@class, "container")]//a[@class="cover"])[1]/img/@src')

		local page = 1
		local pages = tonumber(x.XPathString('//a[@class="last"]/@href'):match('(%d+)$')) or 1
		while true do
			page = page + 1
			if page > pages then
				break
			end
			for v in x.XPath('//div[contains(@class, "gallery")]/a').Get() do
				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(x.XPathString('div', v))
			end
			if not HTTP.GET(u:find('search?', 1, true) and u .. '&page=' .. page or u .. '?page=' .. page) then break end
			x.ParseHTML(HTTP.Document)
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local mid = URL:match('%d+')
	local s = MODULE.Storage[mid] ~= '' and MODULE.Storage[mid] or (HTTP.GET(MODULE.RootURL .. API_URL .. '/' .. mid) and HTTP.Document)

	if not s then return false end

	for v in CreateTXQuery(s).XPath('json(*).pages().path').Get() do
		TASK.PageLinks.Add(GetRandomServer(image_servers) .. '/' .. v.ToString())
	end

	return true
end