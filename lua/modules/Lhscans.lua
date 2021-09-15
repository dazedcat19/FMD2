function Init()
	local cat = 'Raw'
	local function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                         = id
		m.Category                   = cat
		m.Name                       = name
		m.RootURL                    = url
		m.TotalDirectory             = 1
		m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink           = 'GetNameAndLink'
		m.OnGetInfo                  = 'GetInfo'
		m.OnGetPageNumber            = 'GetPageNumber'
		m.OnBeforeDownloadImage      = 'BeforeDownloadImage'
		return m
	end
	AddWebsiteModule('4c089029492f43c98d9f27a23403247b', 'HanaScan', 'https://hanascan.com')
	AddWebsiteModule('010777f53bf2414fad039b9567c8a9ce', 'KissAway', 'https://kissaway.net')
	AddWebsiteModule('794187d0e92e4933bf63812438d69017', 'Manhwa18', 'https://manhwa18.com')
	local m = AddWebsiteModule('9054606f128e4914ae646032215915e5', 'WeLoveManga', 'https://welovemanga.net')
	m.AccountSupport = true
	m.OnLogin        = 'WeLoveMangaLogin'

	cat = 'English'
	AddWebsiteModule('80427d9a7b354f04a8f432b345f0f640', 'MangaWeek', 'https://mangaweek.com')
	AddWebsiteModule('694ff34a6ae4469fbdaecf8d3aebb6eb', 'ManhuaScan', 'https://manhuascan.com')
	AddWebsiteModule('3b7ab0c7342f4783910f7842ea05630b', 'EcchiScan', 'https://ecchiscan.com')
	AddWebsiteModule('f488bcb1911b4f21baa1ab65ef9ca61c', 'HeroScan', 'https://heroscan.com')
	AddWebsiteModule('154d7bdcaa964bcda16ca4dbc683cdb8', 'ManhwaSmut', 'https://manhwasmut.com')
	AddWebsiteModule('9b325c488f6f443281b39315d6fa72d0', 'Manhwa18Net', 'https://manhwa18.net')
	
	cat = 'English-Scanlation'
	AddWebsiteModule('7fb5fbed6d3a44fe923ecc7bf929e6cb', 'LHTranslationArchive', 'https://archive.lhtranslation.net')
	AddWebsiteModule('8313871a984b4b6c8de41860fc5ec96e', 'KSGroupScans', 'https://ksgroupscans.com')
end

function WeLoveMangaLogin()
	if MODULE.Account.Enabled == false then return false end
	MODULE.Account.Status = asChecking
	if HTTP.GET(MODULE.RootURL) then
		local x = CreateTXQuery(HTTP.Document)

		local isLoggedIn = x.XPathString('//form[contains(@id, "logout")]/@action')
		if isLoggedIn ~= nil then
			if HTTP.POST(MaybeFillHost(MODULE.RootURL, isLoggedIn)) then
				HTTP.GET(MODULE.RootURL)
				x = CreateTXQuery(HTTP.Document)
			end
		end

		local crypto = require 'fmd.crypto'
		local token = x.XPathString('//form[contains(@class, "signin")]/@action')
		local url = MaybeFillHost(MODULE.RootURL, token)
		local payload = 'email=' .. crypto.EncodeURLElement(MODULE.Account.Username) ..
						'&password=' .. crypto.EncodeURLElement(MODULE.Account.Password) ..
						'&isRemember=1'

		HTTP.Reset()
		if HTTP.POST(url, payload) then
			if (HTTP.ResultCode == 200 and HTTP.Document.ToString() == '...') then
				MODULE.Account.Status = asValid
				return true
			else
				MODULE.Account.Status = asInvalid
				return false
			end
		else
			MODULE.Account.Status = asUnknown
			return false
		end
	else
		MODULE.Account.Status = asUnknown
		return false
	end
end

function GetDirectoryPageNumber()
	if MODULE.ID == '9054606f128e4914ae646032215915e5' or MODULE.ID == '8313871a984b4b6c8de41860fc5ec96e' then -- WeLoveManga, KSGroupScans
		if HTTP.GET(MODULE.RootURL .. '/manga-list.html?page=1&sort=name&sort_type=ASC') then
			PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[contains(@class, "pagination")]/li[last()-1]')) or 1
			return no_error
		else
			return net_problem
		end
	end
end

function GetNameAndLink()
	if MODULE.ID == '9054606f128e4914ae646032215915e5' or MODULE.ID == '8313871a984b4b6c8de41860fc5ec96e' then -- WeLoveManga, KSGroupScans
		if HTTP.GET(MODULE.RootURL .. '/manga-list.html?page=' .. (URL + 1) .. '&sort=name&sort_type=ASC') then
			local x = CreateTXQuery(HTTP.Document)
			local v for v in x.XPath('//div[@class="row-last-update"]//div[contains(@class, "series-title")]/a').Get() do
				NAMES.Add(Trim(SeparateLeft(v.ToString(), '- Raw')))
				LINKS.Add(v.GetAttribute('href'))
			end
			return no_error
		else
			return net_problem
		end
	else
		if HTTP.GET(MODULE.RootURL .. '/manga-list.html?listType=allABC') then
			local x = CreateTXQuery(HTTP.Document)
			if MODULE.ID == '694ff34a6ae4469fbdaecf8d3aebb6eb' then -- manhuascan
				x.XPathHREFAll('//div[@id="Character"]//a', LINKS, NAMES)
			else
				local v for v in x.XPath('//span[@manga-slug]//a').Get() do
					NAMES.Add(Trim(SeparateLeft(v.ToString(), '- Raw')))
					LINKS.Add(v.GetAttribute('href'))
				end
			end
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
		MANGAINFO.Title      = Trim(SeparateLeft(x.XPathString('//div[@class="container"]//li[3]//span'), '- Raw'))
		if MANGAINFO.Title   == '' then
			MANGAINFO.Title = Trim(x.XPathString('//div[@class="container manga"]//li[3]/a/@title'):gsub('- RAW', ''):gsub('%(MANGA%)', ''))
		end
		MANGAINFO.CoverLink  = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="thumbnail"]/@src'))
		MANGAINFO.Status     = MangaInfoStatusIfPos(x.XPathString('//ul[@class="manga-info"]/li[contains(., "Status")]//a'))
		MANGAINFO.Authors    = x.XPathString('//ul[@class="manga-info"]/li[contains(., "Author")]//a')
		MANGAINFO.Genres     = x.XPathStringAll('//ul[@class="manga-info"]/li[contains(., "Genre")]//a')
		MANGAINFO.Summary    = x.XPathString('string-join(//div[./h3="Description"]//p, "\r\n")')
		if MANGAINFO.Summary == '' then
			MANGAINFO.Summary = x.XPathString('//div[@class="detail"]/div[@class="content"]')
		end

		x.XPathHREFAll('//div[@id="tab-chapper"]//table/tbody/tr/td/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		if MANGAINFO.ChapterLinks.Count == 0 then
			x.XPathHREFAll('//div[@id="list-chapters"]//a[@class="chapter"]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		end
		if MANGAINFO.ChapterLinks.Count == 0 then
			x.XPathHREFTitleAll('//ul[contains(@class, "list-chapters")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		end
		for i = 0, MANGAINFO.ChapterLinks.Count - 1 do
			MANGAINFO.ChapterLinks[i] = MODULE.RootURL .. '/' .. MANGAINFO.ChapterLinks[i]
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		HTTP.Reset()
		HTTP.Headers.Values['Referer'] = MANGAINFO.URL
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(u) then
		local x = CreateTXQuery(HTTP.Document)
		if MODULE.ID == 'f488bcb1911b4f21baa1ab65ef9ca61c' then -- HeroScan
			x.XPathStringAll('//img[contains(@class, "chapter-img")]/@data-original', TASK.PageLinks)
		elseif MODULE.ID == '010777f53bf2414fad039b9567c8a9ce' or MODULE.ID == '694ff34a6ae4469fbdaecf8d3aebb6eb' then -- KissAway, ManhuaScan
			local v for v in x.XPath('//img[contains(@class, "chapter-img")]/@data-aload').Get() do
				TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
			end
		else
			x.XPathStringAll('//img[contains(@class, "chapter-img")]/@data-src', TASK.PageLinks)
			if TASK.PageLinks.count == 0 then x.XPathStringAll('//img[contains(@class, "chapter-img")]/src', TASK.PageLinks) end
		end
	else
		return false
	end
	return true
end

function BeforeDownloadImage()
	if MODULE.ID == 'f488bcb1911b4f21baa1ab65ef9ca61c' and URL.find(URL, "isekaiscan") ~= nil then
		HTTP.Headers.Values['Referer'] = ''
	else
		HTTP.Headers.Values['Referer'] = ' ' .. MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])
	end
	return true
end
