local dirurl = '/manga/new';
local perpage = 20;

function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@id="cover"]/@src'))
		MANGAINFO.Title=x.XPathString('//*[@class="name_row"]/h1')
		MANGAINFO.Authors=x.XPathString('//*[@class="item" and contains(.,"Автор")]/following-sibling::*[1]')
		MANGAINFO.Genres=x.XPathString('//*[@class="item" and contains(.,"Тэги")]/following-sibling::*[1]')
		MANGAINFO.Status=MangaInfoStatusIfPos(x.XPathString('//*[@class="item" and contains(.,"Загружено")]/following-sibling::*[1]'), 'продолжается', '')
		MANGAINFO.Summary=x.XPathStringAll('//*[@id="description"]/text()')
		if MODULE.Name=='MangaChanRU' then x.XPathHREFAll('//table[@class="table_cha"]//div[@class="manga2"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MODULE.Name=='YaoiChanRU' then x.XPathHREFAll('//table[@class="table_cha"]//div[@class="manga"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MODULE.Name=='HentaiChanRU' then
			local v=x.XPath('//div[@id="manga_images"]/a')
			for i = 1, v.Count do
				v1 = v.Get(i)
				MANGAINFO.ChapterLinks.Add(v1.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(v1.GetAttribute('title'))
			end
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageNumber=0
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		local x=CreateTXQuery(HTTP.Document)
		local v=string.match(x.XPathString('//script[contains(., "var data")]/text()'), 'var data =(.-);')
		CreateTXQuery(v).XPathStringAll('json(*).fullimg()', TASK.PageLinks)
		return true
	else
		return false
	end
end

function getnameandlink()
	local s = MODULE.RootURL..dirurl
	if URL ~= '0' then s = s..'?offset='..tostring(tonumber(URL) * perpage) end
	if HTTP.GET(s) then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//*[@class="content_row"]//a[@class="title_link"]',LINKS,NAMES)
		return no_error
	else
		return net_problem
	end
end

function getdirectorypagenumber()
	PAGENUMBER=1
	if HTTP.GET(MODULE.RootURL .. dirurl) then
		x = CreateTXQuery(HTTP.Document)
		local s = tonumber(x.XPathString('//*[@id="pagination"]/a[last()]/substring-after(@href,"offset=")'))
		if s ~= '' then PAGENUMBER=s end
		if PAGENUMBER > 1 then PAGENUMBER=math.floor(PAGENUMBER / perpage) + 1 end
		return no_error
	else
		return net_problem
	end
end

function Init()
	function AddWebsiteModule(id, name, url, category)
		local m = NewWebsiteModule()
		m.ID = id
		m.Name = name
		m.RootURL = url
		m.Category = category
		m.LastUpdated='May 6, 2019'
		m.OnGetInfo='getinfo'
		m.OnGetPageNumber='getpagenumber'
		m.OnGetNameAndLink='getnameandlink'
		m.OnGetDirectoryPageNumber='getdirectorypagenumber'
	end
	AddWebsiteModule('adb5ad9b022e4b5c82586d10404c253e', 'MangaChanRU', 'http://manga-chan.me', 'Russian')

	local cat = 'H-Sites'
	AddWebsiteModule('82f401398ab94d04b03c0221e9fe2aa3', 'HentaiChanRU', 'http://h-chan.me', cat)
	AddWebsiteModule('bbc4794f4ffe4468a28b21a7e009366e', 'YaoiChanRU', 'http://yaoi-chan.me', cat)
end
