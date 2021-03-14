function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                         = id
		m.Name                       = name
		m.RootURL                    = url
		m.Category                   = 'H-Sites'
		m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink           = 'GetNameAndLink'
		m.OnGetInfo                  = 'GetInfo'
		m.OnGetPageNumber            = 'GetPageNumber'
		m.OnBeforeDownloadImage      = 'BeforeDownloadImage'
		m.SortedList                 = true
		end
	AddWebsiteModule('58a2dec76ebf43a5a9e7dc9b453e52e9', 'HentaiFox', 'https://hentaifox.com')
	AddWebsiteModule('67e22e0c766c4c9c990e179350262b3c', 'IMHentai', 'https://imhentai.xxx')
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if MODULE.ID == '58a2dec76ebf43a5a9e7dc9b453e52e9' then -- HentaiFox
		if HTTP.GET(MODULE.RootURL .. '/pag/' .. (URL + 1) .. '/') then
			CreateTXQuery(HTTP.Document).XPathHREFAll('//h2[@class="g_title"]/a', LINKS, NAMES)
			return no_error
		else
			return net_problem
		end
	else
		if HTTP.GET(MODULE.RootURL .. '/?page=' .. (URL + 1)) then
			CreateTXQuery(HTTP.Document).XPathHREFAll('//h2[@class="gallery_title"]/a', LINKS, NAMES)
			return no_error
		else
			return net_problem
		end
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		if MODULE.ID == '58a2dec76ebf43a5a9e7dc9b453e52e9' then -- HentaiFox
			MANGAINFO.Title      = x.XPathString('//div[@class="info"]/h1')
			MANGAINFO.CoverLink  = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class,"cover"]/img/@src'))
			MANGAINFO.Artists    = x.XPathStringAll('//ul[@class="artists"]/li/a/text()[not(parent::span)]')
			MANGAINFO.Genres     = x.XPathStringAll('//div[@class="info"]/ul[not(@class="artists")]/li/a/text()[not(parent::span)]')
		else
			MANGAINFO.Title      = x.XPathString('//div[contains(@class,"right_details")]/h1')
			MANGAINFO.CoverLink  = x.XPathString('//div[contains(@class,"left_cover")]//img/@src')
			MANGAINFO.Artists    = x.XPathStringAll('//ul[@class="galleries_info"]/li[contains(.,"Artists")]/a/text()[not(parent::span)]')
			MANGAINFO.Genres     = x.XPathStringAll('//ul[@class="galleries_info"]/li[not(contains(.,"Artists"))]/a/text()[not(parent::span)]')
		end
		MANGAINFO.ChapterLinks.Add(URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		return no_error
	else
		return net_problem
	end
end

local ext = {
	['p'] = '.png',
	['j'] = '.jpg',
	['g'] = '.gif',
	['w'] = '.webp'
}

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		local dir       = x.XPathString('//*[@id="load_dir"]/@value')
		local id        = x.XPathString('//*[@id="load_id"]/@value')
		local server    = x.XPathString('//*[@id="load_server"]/@value')
		local json      = GetBetween("parseJSON('{", "');", x.XPathString('//script[contains(., "var g_th")]'))
		json = json:gsub('","', ';'):gsub('"}', ';'):gsub(':', ','):gsub('"', '')
		local i; for i in json:gmatch('(.-);') do
			i1, i2 = i:match('(.-),(.-),.-,.-')
			if MODULE.ID == '58a2dec76ebf43a5a9e7dc9b453e52e9' then -- HentaiFox
				TASK.PageLinks.Add('https://i.hentaifox.com/' .. dir .. '/' .. id .. '/' .. i1 .. ext[i2])
			else
				TASK.PageLinks.Add('https://m' .. server .. '.imhentai.xxx/' .. dir .. '/' .. id .. '/' .. i1 .. ext[i2])
			end
		end
		return true
	else
		return false
	end
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	return true
end
