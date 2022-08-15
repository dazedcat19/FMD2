function getinfo()
	if URL:find('/episodes') then
		MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	else
		MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL..'/episodes')
	end
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title = x.XPathString('//div[@class="detail-title-bg"]/span')
		MANGAINFO.CoverLink=x.XPathString('//div[@class="detail-img"]/img/@src')
		MANGAINFO.Authors=x.XPathString('//div[@class="detail-top-left"]//div[@class="created-by"]')
		MANGAINFO.Genres=x.XPathString('//div[@class="detail-top-left"]//div[@class="top-comics-type]')
		local chapters = x.XPath('//div[@class="episodes-wrap-new"]/a')
		for i = 1, chapters.Count do
			local v1 = chapters.Get(i)
			MANGAINFO.ChapterLinks.Add(v1.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('.//div[@class="episode-title-new"]', v1))
		end
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="pictures"]//img/@data-original', TASK.PageLinks)

		if TASK.PageNumber == 1 then
			TASK.PageNumber = 0
			TASK.PageLinks.Clear()
			return false
		end

		return true
	else
		return false
	end
end

function getnameandlink()

	local dirurl = '/genre?page='
	if MODULE.Name == 'MangaToon' then
	 dirurl = '/en/genre?page='
	end

	if HTTP.GET(MODULE.RootURL..dirurl.. (URL + 1)) then
		local x=CreateTXQuery(HTTP.Document)
		CreateTXQuery(HTTP.Document).XPathHREFAll('//ul[contains(@class, "ret-search-list")]/li//h3/a',LINKS,NAMES)
		local v = x.XPath('//div[@class="items"]/a')
		for i = 1, v.Count do
			local v1 = v.Get(i)
			NAMES.Add(x.XPathString('.//div[@class="content-title"]', v1));
			LINKS.Add(v1.GetAttribute('href')..'/episodes');
		end
		p = tonumber(20)
		if p ~= nil then
			UPDATELIST.CurrentDirectoryPageNumber = p
		end
		return no_error
	else
		return net_problem
	end
end

function Init()
	function AddWebsiteModule(id, site, url, cat)
		local m=NewWebsiteModule()
		m.ID=id
		m.Category=cat
		m.Name=site
		m.RootURL=url
		m.LastUpdated='April 09, 2019'
		m.OnGetInfo='getinfo'
		m.OnGetPageNumber='getpagenumber'
		m.OnGetNameAndLink='getnameandlink'
	end
	AddWebsiteModule('e203bd33bcfe4d2b919e696fa5de6f63', 'MangaToon', 'https://mangatoon.mobi', 'English')
	AddWebsiteModule('9f34de1c70824222802b25e656086da8', 'MangaToonID', 'https://mangatoon.mobi/id', 'Indonesian')
	AddWebsiteModule('3bdf6d8182ed43ecb7102156a520add5', 'MangaToonVI', 'https://mangatoon.mobi/vi', 'Vietnamese')
	--AddWebsiteModule('fba3900561d2464b8a242b37a0f42f4a', 'MangaToonSP', 'https://mangatoon.mobi/es', 'Spanish')
	AddWebsiteModule('9fd7c7954eea4addaf2d283135c20222', 'MangaToonCN', 'https://mangatoon.mobi/cn', 'Webcomics')
end
