function Init()
	local m = NewWebsiteModule()
	m.ID                         = 'c67d163c51b24bc498e777e2b0d810d2'
	m.Name                       = 'LeerCapitulo'
	m.RootURL                    = 'https://www.leercapitulo.com/'
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
		MANGAINFO.Title      = x.XPathStringAll('//div[@class="media-body"]/*[self::h1 or self::h2 or self::h3]/text()', '')
		MANGAINFO.Authors    = x.XPathString('//div[@class="uk-text-center"]/h2/a')
		MANGAINFO.CoverLink  = x.XPathString('//div[@class="media-manga-detail"]//div[@class="media-left-cover-detail")]/img/@src')
		MANGAINFO.Genres     = x.XPathStringAll('//div[contains(@class, "generos-tags")]/a')
		MANGAINFO.Summary    = x.XPathStringAll('//div[contains(@class,"manga-content")]/p')

		local v for v in x.XPath('//a[@class="xanh"]').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('text()', v))
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		local s = x.XPathString('//p[@id="arraydata"]')
		for v in s:gmatch("([^,]+)") do TASK.PageLinks.Add(v:gsub("cdn.statically.io/img/", "")) end
		return true
	else
		return false
	end
end
