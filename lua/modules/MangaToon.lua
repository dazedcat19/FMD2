function Init()
	function AddWebsiteModule(id, name, url, cat)
		local m = NewWebsiteModule()
		m.ID                 = id
		m.Name               = name
		m.RootURL            = url
		m.Category           = cat
		m.OnGetNameAndLink   = 'GetNameAndLink'
		m.OnGetInfo          = 'GetInfo'
		m.OnGetPageNumber    = 'GetPageNumber'
	end
	AddWebsiteModule('e203bd33bcfe4d2b919e696fa5de6f63', 'MangaToon', 'https://mangatoon.mobi', 'English')
	AddWebsiteModule('9f34de1c70824222802b25e656086da8', 'MangaToonID', 'https://mangatoon.mobi/id', 'Indonesian')
	AddWebsiteModule('3bdf6d8182ed43ecb7102156a520add5', 'MangaToonVI', 'https://mangatoon.mobi/vi', 'Vietnamese')
	AddWebsiteModule('8b64d8a6fcd049999e36c63b6706df53', 'MangaToonSP', 'https://mangatoon.mobi/es', 'Spanish')
	AddWebsiteModule('4e2496188f384acbae47cfb295cd9ca1', 'MangaToonPT', 'https://mangatoon.mobi/pt', 'Portuguese')
	AddWebsiteModule('3cc350524dcb42dbb547e3198be028e6', 'MangaToonTH', 'https://mangatoon.mobi/th', 'Thailand')
	AddWebsiteModule('4df0307b95ec458485798908bf2b9f31', 'MangaToonFR', 'https://mangatoon.mobi/fr', 'France')
	AddWebsiteModule('cce6e62454b84b5e91ccd9f4b60c7be6', 'MangaToonAR', 'https://mangatoon.mobi/ar', 'Arabic')
	AddWebsiteModule('9fd7c7954eea4addaf2d283135c20222', 'MangaToonCN', 'https://mangatoon.mobi/cn', 'Webcomics')
end

function GetNameAndLink()
	local dirurl = '/genre/comic'
	if MODULE.ID == 'e203bd33bcfe4d2b919e696fa5de6f63' then -- English
		dirurl = '/en/genre/comic'
	end
	if not HTTP.GET(MODULE.RootURL .. dirurl) then return net_problem end
	local x = CreateTXQuery(HTTP.Document)
	local next_url
	while true do
		local v for v in x.XPath('//div[@class="items"]/a').Get() do
			LINKS.Add(v.GetAttribute('href'))
			NAMES.Add(x.XPathString('.//div[@class="content-title"]/span', v))
		end
		next_url = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="page"]/a[contains(., "Next Page")]/@href'))
		if HTTP.Terminated then break end
		if next_url == '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('page=(%d+)') or ''))
		if HTTP.GET(next_url) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end
	return no_error
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathString('//div[@class="detail-title-bg"]/span')
		MANGAINFO.CoverLink  = x.XPathString('//div[@class="detail-img"]/img[1]/@src')
		MANGAINFO.Authors    = x.XPathString('//div[@class="detail-author-name select-text"]/span/substring-after(., ":")')
		MANGAINFO.Genres     = x.XPathString('//div[@class="detail-tags-info select-text"]/span'):gsub('/', ', ')
		local status = x.XPathString('//div[@class="detail-status"]')
		if (status == 'on going') or (status == 'sedang berlangsung') or (status == 'Tiếp tục cập nhật') or (status == 'En proceso') or (status == 'Atualizando') or (status == 'เซเรียล') or (status == 'En cours') or (status == 'التسلسل') or (status == '连载') then
			status = 'ongoing'
		else
			status = 'completed'
		end
		MANGAINFO.Status     = MangaInfoStatusIfPos(status)
		MANGAINFO.Summary    = x.XPathString('//div[@class="detail-description select-text"]//p')
		local v for v in x.XPath('//div[@class="episodes-wrap-new"]/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('.//div[@class="episode-title-new"]', v))
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="pictures"]//img/@data-original', TASK.PageLinks)
		return true
	else
		return false
	end
end
