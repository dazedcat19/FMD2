function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		if MANGAINFO.Title == '' then
			MANGAINFO.Title = x.XPathString('//div[@class="manga_series_data"]/h1')
		end
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="manga_series_image"]/img/@src'))
		MANGAINFO.Genres=x.XPathStringAll('//div[@class="series_sub_genre_list"]/a')
		MANGAINFO.Summary=x.XPathString('//div[@class="manga_series_description"]'):gsub('Manga Description', '')
		x.XPathHREFAll('//div[@class="manga_series_list"]/table/tbody/tr/td[1]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x=CreateTXQuery(HTTP.Document)
		x.XPathStringAll('//img[@id="gohere"]/@src', TASK.PageLinks)
	else
		return false
	end
	return true
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL .. '/Mangalist/All/'..(URL + 1)) then
		local x = CreateTXQuery(HTTP.Document)
		local v=x.XPath('//*[contains(@class, "list_item_content")]//div[contains(@class, "list_item")]')
		for i=1,v.Count do
			local v1=v.Get(i)
			local title = x.XPathString('.//*[contains(@class, "list_item_info")]/h3', v1)
			local link = x.XPathString('.//*[contains(@class, "list_item_info")]/h3/a/@href', v1)
			NAMES.Add(title)
			LINKS.Add(link)
		end
		return no_error
	else
		return net_problem
	end
end

function getdirectorypagenumber()
	if HTTP.GET(MODULE.RootURL .. '/Mangalist') then
		local x = CreateTXQuery(HTTP.Document)
		PAGENUMBER = tonumber(x.XPathString('//div[contains(@class,"pagination")]/a[contains(@class,"last_p")]/@href/substring-after(.,"All/")')) or 1
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m=NewWebsiteModule()
	m.ID = '1d6a75911a1b414f89e5cf95c9a7ae4e'
	m.Category='English'
	m.Name='MangaFreak'
	m.RootURL='https://w11.mangafreak.net'
	m.OnGetInfo='getinfo'
	m.OnGetPageNumber='getpagenumber'
	m.OnGetNameAndLink='getnameandlink'
	m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end
