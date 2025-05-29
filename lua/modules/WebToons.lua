local langs = {
	["en"] = "English",
	["id"] = "Indonesian",
	["zh-hant"] = "Chinese",
	["th"] = "Thai"
}

function getinfo()
	MANGAINFO.URL = MANGAINFO.URL:gsub('(.*)&page=.*', '%1')
	HTTP.Headers.Values['sec-ch-ua-platform'] = '"Android"'
	HTTP.Headers.Values['sec-ch-ua-platform-version'] = '"6.0"'
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title = x.XPathString('//meta[@property="og:title"]/@content')
		MANGAINFO.CoverLink=x.XPathString('//meta[@name="twitter:image"]/@content')
		MANGAINFO.Authors=x.XPathString('//div[contains(@class, "detail_info")]/p[@class="author"]/text()'):gsub("[^%a%d ,]", '')
		MANGAINFO.Genres=x.XPathString('//div[@class="info"]/h2')
		if MANGAINFO.Genres == '' then
			MANGAINFO.Genres=x.XPathString('//div[contains(@class, "detail_info")]/p[@class="genre"]')
		end
		MANGAINFO.Summary=x.XPathString('//p[@class="summary"]')
		local v
		for v in x.XPath('//ul[@id="_episodeList"]/li[@class="_episodeItem"]/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('.//p/span[@class="ellipsis"]', v))
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
	URL = URL:gsub('(.*)&page=.*', '%1')
	HTTP.Cookies.Values['ageGatePass'] = 'True'
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="_imageList"]/img[@class="_images"]/@data-URL', TASK.PageLinks)
		return true
	else
		return false
	end
end

function getnameandlink()
	local selectedLang = MODULE.GetOption('lualang')
	local l = langs
	if selectedLang > 0 then
		l = {[findlang(selectedLang)] = ""}
	end
	local key, dirurl, v
	local x = CreateTXQuery()
	for key, _ in pairs(l) do
		dirurl = key..'/genre'
		if HTTP.GET(MODULE.RootURL .. dirurl) then
			x.ParseHTML(HTTP.Document)
			for v in x.XPath('//div[@class="card_wrap genre"]/ul/li/a').Get() do
				NAMES.Add(x.XPathString('.//div[@class="Info"]//p[@class="subj"]', v) .. ' [' .. key .. ']')
				LINKS.Add(v.GetAttribute('href'))
			end
		else
			return net_problem
		end
	end

	if MODULE.GetOption('luaincludechallengetitles') then
		for key, _ in pairs(l) do
			dirurl = key..'/challenge/list?genreTab=ALL&sortOrder=UPDATE'
			if HTTP.GET(MODULE.RootURL..dirurl) then
				x.ParseHTML(HTTP.Document)
				local v, p
				while true do
					for v in x.XPath('//div[@class="challenge_cont_area"]/div[contains(@class,"challenge_lst")]/ul/li/a[contains(@class,"challenge_item")]').Get() do
						NAMES.Add(x.XPathString('./p[@class="subj"]', v)..' ['..key..']');
						LINKS.Add(v.GetAttribute('href'));
					end
					p = x.XPathString('//div[@class="paginate"]/a[@href="#"]/following-sibling::a/@href')
					if (p ~= '') and HTTP.GET(MaybeFillHost(MODULE.RootURL, p)) then
						x.ParseHTML(HTTP.Document)
					else
						break
					end
				end
			else
				return net_problem
			end
		end
	end

	return no_error
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = URL
	return true
end

function getlang(lang)
	if langs[lang] ~= nil then
		return langs[lang]
	else
		return 'Unknown'
	end
end

function getlanglist()
	local t = {}
	for k, v in pairs(langs) do table.insert(t, v); end
	table.sort(t)
	return t
end

function findlang(lang)
	local t = getlanglist()
	for i, v in ipairs(t) do
		if i == lang then
			lang = v
			break
		end
	end
	for k, v in pairs(langs) do
		if v == lang then return k; end
	end
	return nil
end

function Init()
	local m = NewWebsiteModule()
	m.ID                    = '18f636ec7fdf47fabe95d940ad0b548f'
	m.Category              ='English'
	m.Name                  ='WebToons'
	m.RootURL               ='https://www.webtoons.com/'
	m.OnGetInfo             ='getinfo'
	m.OnGetPageNumber       ='getpagenumber'
	m.OnGetNameAndLink      ='getnameandlink'
	m.OnBeforeDownloadImage = 'BeforeDownloadImage'
	m.AddServerCookies('webtoons.com', 'ageGatePass=True; max-age=31556952')

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['includechallengetitles'] = 'Include manga titles from WebToons Challenge (takes very very long to create manga list!)',
			['lang'] = 'Language:'
		},
		['id_ID'] = {
			['includechallengetitles'] = 'Sertakan judul komik dari WebToons Challenge (perlu waktu yang sangat lama untuk membuat daftar komik!)',
			['lang'] = 'Bahasa:'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionCheckBox('luaincludechallengetitles', lang:get('includechallengetitles'), false)

	local items = 'All'
	local t = getlanglist()
	for k, v in ipairs(t) do items = items .. '\r\n' .. v; end
	m.AddOptionComboBox('lualang', lang:get('lang'), items, 2)
end
