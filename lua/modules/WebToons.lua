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

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['includechallengetitles'] = 'Include manga titles from WebToons Challenge (takes very very long to create manga list!)',
			['lang'] = 'Language:'
		},
		['id_ID'] = {
			['includechallengetitles'] = 'Sertakan judul komik dari WebToons Challenge (perlu waktu yang sangat lama untuk membuat daftar komik!)',
			['lang'] = 'Bahasa:'
		}
	}
	local lang = translations[slang] or translations.en
	local items = table.concat(GetLangList(), '\r\n')
	m.AddOptionComboBox('lualang', lang.lang, items, 3)
	m.AddOptionCheckBox('luaincludechallengetitles', lang.includechallengetitles, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Langs = {
	{  nil, 'All' },
	{ 'en', 'English' },
	{ 'fr', 'French' },
	{ 'id', 'Indonesian' },
	{ 'zh-hant', 'Chinese' },
	{ 'th', 'Thai' }
}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Return language names in defined order
function GetLangList()
	local t = {}
	for _, v in ipairs(Langs) do
		table.insert(t, v[2])
	end
	return t
end

-- Return language key by index
local function FindLang(lang)
	return Langs[lang + 1][1]
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local selectedLang = MODULE.GetOption('lualang')
	local l = langs
	if selectedLang > 0 then
		l = {[FindLang(selectedLang)] = ''}
	end
	local key, dirurl, v
	local x = CreateTXQuery()
	for key, _ in pairs(l) do
		dirurl = key .. '/genre'
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
			dirurl = key .. '/challenge/list?genreTab=ALL&sortOrder=UPDATE'
			if HTTP.GET(MODULE.RootURL..dirurl) then
				x.ParseHTML(HTTP.Document)
				while true do
					for v in x.XPath('//div[@class="challenge_cont_area"]/div[contains(@class,"challenge_lst")]/ul/li/a[contains(@class,"challenge_item")]').Get() do
						NAMES.Add(x.XPathString('./p[@class="subj"]', v)..' ['..key..']');
						LINKS.Add(v.GetAttribute('href'));
					end
					local p = x.XPathString('//div[@class="paginate"]/a[@href="#"]/following-sibling::a/@href')
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
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//meta[@property="og:title"]/@content')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Authors   = x.XPathString('//div[@class="author_area"]/text()'):gsub('[^%a%d ,]', '')
	if MANGAINFO.Authors == '' then MANGAINFO.Authors = x.XPathStringAll('//div[@class="author_area"]/a') end
	MANGAINFO.Genres    = x.XPathStringAll('//*[self::h2 or self::p][contains(@class, "genre")]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[@class="day_info"]'), 'UP')
	MANGAINFO.Summary   = x.XPathString('//p[contains(@class, "summary")]')

	local lang = x.XPathString('//script[contains(., "contentLang")]'):match("contentLang: '(.-)'")

	if URL:find('/canvas/') then s = 'canvas' else s = 'webtoon' end

	if not HTTP.GET('https://m.webtoons.com/api/v1/' .. s .. '/' .. URL:match('title_no=(%d+)') .. '/episodes?pageSize=99999' .. '&readingLanguageCode=' .. lang) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).result.episodeList()').Get() do
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
