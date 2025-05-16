function Init()
	local m = NewWebsiteModule()
	m.ID                         = 'd78d163c51b24bc487e777e2b0d811e3'
	m.Name                       = 'MangaTV'
	m.RootURL                    = 'https://mangatv.net'
	m.Category                   = 'Spanish'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.SortedList                 = true
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="navigation"]//li[last()]/a')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/page/' .. (URL + 1)) then
		CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//*[@id="main"]//div[contains(@class, "chapter-thumb")]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathStringAll('//h1[contains(@class, "entry-title")]/text()')
		MANGAINFO.Summary    = x.XPathString('//div[contains(@class, "wd-full")][4]//span/text()')
		MANGAINFO.Authors    = x.XPathString('//div[@class="uk-text-center"]/h2/a')
		MANGAINFO.CoverLink  = x.XPathString('//div[contains(@class, "thumb")]//img/@src')
		MANGAINFO.Genres     = x.XPathStringAll('//h1[contains(@class, "entry-title")]')

		local v
		for v in x.XPath('//div[contains(@class, "eplister")]//li').Get() do
			-- Extraer el enlace
			MANGAINFO.ChapterLinks.Add(x.XPathString('.//a[@class="dload"]/@href', v))
			
			-- Extraer número de capítulo y grupo
			local chapterNum = x.XPathString('.//span[@class="chapternum"][1]/text()', v)
			local titleGroup = x.XPathString('.//span[@class="chapternum"][2]/text()', v)
			
			-- Procesar el grupo de traducción
			local group = "Sin grupo"
			if titleGroup then
				group = titleGroup:match("|%s*(.+)$") or "Sin grupo"
			end
			
			MANGAINFO.ChapterNames.Add(string.format("%s [%s]", chapterNum:gsub("%s+", " "), group:gsub("%s+", " ")))
		end
		
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	local result = false
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local crypto = require "fmd.crypto"
		local x = CreateTXQuery(HTTP.Document)
		local s = x.XPathString('//script[contains(., "eval")]')
	
		for url in s:gmatch("|Ly9pbWc[^'|%s]+") do
			local decodedURL = crypto.DecodeBase64(url .. '==')
			if decodedURL then
				TASK.PageLinks.Add("https:" .. decodedURL)
			end
		end
		TASK.PageLinks.Reverse()
		result = true
	end
	return result
end
