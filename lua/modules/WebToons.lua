----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '18f636ec7fdf47fabe95d940ad0b548f'
	m.Category                 = 'English'
	m.Name                     = 'WebToons'
	m.RootURL                  = 'https://www.webtoons.com/'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
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
	local t = GetLangList()
	for k, v in ipairs(t) do items = items .. '\r\n' .. v; end
	m.AddOptionComboBox('lualang', lang:get('lang'), items, 2)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local langs = {
	["en"] = "English",
	["id"] = "Indonesian",
	["zh-hant"] = "Chinese",
	["th"] = "Thai"
}

----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

function GetLang(lang)
	if langs[lang] ~= nil then
		return langs[lang]
	else
		return 'Unknown'
	end
end

function GetLangList()
	local t = {}
	for k, v in pairs(langs) do table.insert(t, v); end
	table.sort(t)
	return t
end

function FindLang(lang)
	local t = GetLangList()
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

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local selectedLang = MODULE.GetOption('lualang')
	local l = langs
	if selectedLang > 0 then
		l = {[FindLang(selectedLang)] = ""}
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

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//meta[@property="og:title"]/@content')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Authors   = x.XPathString('//div[@class="author_area"]/text()'):gsub('[^%a%d ,]', '')
	MANGAINFO.Genres    = x.XPathString('//h2[contains(@class, "genre")]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[@class="day_info"]'), 'UP')
	MANGAINFO.Summary   = x.XPathString('//p[@class="summary"]')

	u = 'https://m.webtoons.com/api/v1/webtoon/' .. URL:match('title_no=(%d+)') .. '/episodes?pageSize=99999'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).result.episodeList()').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetProperty('viewerLink').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('episodeTitle').ToString())
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	HTTP.Cookies.Values['ageGatePass'] = 'True'

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="_imageList"]/img[@class="_images"]/@data-URL', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end