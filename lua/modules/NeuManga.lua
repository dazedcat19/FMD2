function GetInfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL,URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title=x.XPathString('//div[@class="series-title"]/h2')
		MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="series-thumb"]/img/@src'))
		MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="series-infoz block"]/span[@class="status Ongoing" or @class="status Completed"]'))
		MANGAINFO.Authors = x.XPathString('//ul[@class="series-infolist"]/li[contains(b, "Author")]/span')
		MANGAINFO.Artists = x.XPathString('//ul[@class="series-infolist"]/li[contains(b, "Artist")]/span')
		MANGAINFO.Genres = x.XPathStringAll('//div[@class="series-genres"]/a')
		MANGAINFO.Summary = x.XPathStringAll('//div[@class="series-synops"]/p')
		local v=x.XPath('//div[@class="series-chapter"]/ul[@class="series-chapterlist"]//a')
		for i=1, v.Count do
				local v1=v.Get(i)
				MANGAINFO.ChapterLinks.Add(v1.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(x.XPathString('span', v1))
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	TASK.PageNumber = 0
	local u = MaybeFillHost(MODULE.RootURL,URL)
	if HTTP.GET(u) then
		local x=CreateTXQuery(HTTP.Document)
		x.XPathStringAll('//div[@class="reader-area"]//img/@src', TASK.PageLinks)
		return true
	else
		return false
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL..'/manga-list') then
		x=CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//div[@class="container"]/div[@class="mangalist-blc"]//li/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID = '43b1be881e5c4661b984b84ba8f208a6'
	m.Category='Indonesian'
	m.Name='NeuManga'
	m.RootURL='https://neumanga.net'
	m.OnGetInfo='GetInfo'
	m.OnGetPageNumber='GetPageNumber'
	m.OnGetNameAndLink='GetNameAndLink'
end
