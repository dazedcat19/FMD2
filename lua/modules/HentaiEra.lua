----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '38f5203c68834cf782620c34b6fef944'
	m.Name                     = 'HentaiEra'
	m.RootURL                  = 'https://hentaiera.com'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/?page='

local ext = {
	['j'] = '.jpg',
	['p'] = '.png',
	['b'] = '.bmp',
	['g'] = '.gif',
	['w'] = '.webp'
}
----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1

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
	if URL:find('/gallery/', 1, true) then
		local u = MaybeFillHost(MODULE.RootURL, URL)
		
		if not HTTP.GET(u) then return net_problem end

		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//div[contains(@class, "gallery_first")]/h1/text()')
		MANGAINFO.AltTitles = x.XPathString('//div[contains(@class, "gallery_first")]//p[contains(@class, "subtitle")]/text()')
		MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "gallery_first")]//div[contains(@class, "left_cover")]//img/@data-src')
		MANGAINFO.Artists   = x.XPathStringAll('(//span[./text()="Artists"]/following-sibling::div[contains(@class, "info_tags")]//span, //span[./text()="Groups"]/following-sibling::div[contains(@class, "info_tags")]//span)')
		MANGAINFO.Genres    = x.XPathStringAll('(//span[./text()="Tags"]/following-sibling::div[contains(@class, "info_tags")]//span, //span[./text()="Category"]/following-sibling::div[contains(@class, "info_tags")]//span)')
		
		local desc = {}
		local parodies = {}
		local characters = {}
		local languages = {}

		for parody in x.XPath('//span[./text()="Parodies"]/following-sibling::div[contains(@class, "info_tags")]//span').Get() do
			parodies[#parodies + 1] = parody.ToString()
		end

		for character in x.XPath('//span[./text()="Characters"]/following-sibling::div[contains(@class, "info_tags")]//span').Get() do
			characters[#characters + 1] = character.ToString()
		end

		for language in x.XPath('//span[./text()="Languages"]/following-sibling::div[contains(@class, "info_tags")]//span[not(contains(text(), "translated"))]').Get() do
			languages[#languages + 1] = language.ToString()
		end

		local pages = x.XPathString('//button[@id="pages_btn"]/text() ! substring-before(., " Pages")')

		if #parodies > 0 then table.insert(desc, 'Parodies: ' .. table.concat(parodies, ', ')) end
		if #characters > 0 then table.insert(desc, 'Characters: ' .. table.concat(characters, ', ')) end
		if #languages > 0 then table.insert(desc, 'Languages: ' .. table.concat(languages, ', ')) end
		if pages ~= '' then table.insert(desc, 'Pages: ' .. pages) end

		MANGAINFO.Summary = table.concat(desc, '\r\n')

		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	else
		local u = MaybeFillHost(MODULE.RootURL, URL:gsub('(.*)popular.*', '%1'))
		if URL:find('/?page=', 1, true) then return no_error end
		
		if not HTTP.GET(u) then return net_problem end

		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//div[@class="container"]/div[contains(@class, "galleries")]//span[contains(@class, "search_key")]/text()'):gsub('%"', "")
		MANGAINFO.CoverLink = x.XPathString('(//div[contains(@class, "thumbs_container")]//div[@class="inner_thumb"])[1]/a/img/@data-src')

		local page = 1
		local pages = tonumber(x.XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1
		while true do
		    for v in x.XPath('//div[@class="thumb"]').Get() do
				MANGAINFO.ChapterLinks.Add(x.XPathString('div/div[@class="inner_thumb"]/a/@href', v))
				MANGAINFO.ChapterNames.Add(x.XPathString('div/div[@class="g_text"]', v))
			end
			page = page + 1
			if page > pages then
				break
			end
			local separator = u:find('?', 1, true) and '&' or '?'
			if not HTTP.GET(u .. separator .. 'page=' .. page) then break end
			x.ParseHTML(HTTP.Document)
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local load_server = x.XPathString('//input[@id="load_server"]/@value')
	local load_dir    = x.XPathString('//input[@id="load_dir"]/@value')
	local load_id     = x.XPathString('//input[@id="load_id"]/@value')
	
	local old_server_url = 'https://hentaiera.com/downloads/' .. load_dir .. '/' .. load_id .. '.js'
    HTTP.GET(old_server_url)
    if HTTP.ResultCode == 200 then
        local body = HTTP.Document.ToString()
        for filename in body:gmatch('item":"(.-)%|') do
            TASK.PageLinks.Add('https://m' .. load_server ..'.hentaiera.com/' .. load_dir .. '/' .. load_id .. '/' .. filename)
        end
    else
        local json = GetBetween("parseJSON('", "')", x.XPathString('//script[contains(., "var g_th")]'))
        for page, ext_type in json:gmatch('"(%d+)":"(%a),') do
            TASK.PageLinks.Add('https://m' .. load_server  .. '.hentaiera.com/'  .. load_dir .. '/' .. load_id .. '/' .. page .. ext[ext_type])
        end
    end

	return true
end