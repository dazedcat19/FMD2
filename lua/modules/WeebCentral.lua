----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'abbde9b6468f45939e5603416d73ac47'
	m.Name                     = 'Weeb Central'
	m.RootURL                  = 'https://weebcentral.com'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.MaxTaskLimit             = 2
	m.MaxConnectionLimit       = 4
	m.SortedList               = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['detect_official'] = 'Separately mark the official chapter so it will be detected when new ones are available'
		},
		['id_ID'] = {
			['detect_official'] = 'Tandai bab resmi secara terpisah supaya bisa terdeteksi saat yang baru tersedia'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('detect_official', lang.detect_official, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/search/data?limit=32&sort=Recently+Added&order=Descending&official=Any&display_mode=Minimal+Display&offset='
local DirectoryPageLimit = 32

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 0
	PAGENUMBER = 10100

	if not HTTP.GET(u .. PAGENUMBER) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local s = x.XPathString('//button/@hx-get')
	while s ~= '' do
		PAGENUMBER = tonumber(s:match('offset=(%d+)')) or PAGENUMBER + DirectoryPageLimit
		if not HTTP.GET(u .. PAGENUMBER) then return net_problem end
		s = CreateTXQuery(HTTP.Document).XPathString('//button/@hx-get')
	end
	PAGENUMBER = math.ceil(PAGENUMBER / DirectoryPageLimit)

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (DirectoryPageLimit * URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//article/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('h2', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('(//h1)[1]')
	MANGAINFO.AltTitles = x.XPathStringAll('//li[./strong="Associated Name(s)"]//li')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Authors   = x.XPathStringAll('//li[contains(., "Author")]/span/a')
	MANGAINFO.Genres    = x.XPathStringAll('//li[contains(., "Tags")]/span/a|//li[contains(., "Type")]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//li[contains(., "Status")]/a'))
	MANGAINFO.Summary   = x.XPathString('//li[contains(., "Description")]/p')

	local official = x.XPathString('//li[./strong[contains(., "Official Translation")]]/a')
	if official:find('Yes', 1, true) then MANGAINFO.Summary = 'Official Translation\r\n \r\n' .. MANGAINFO.Summary end

	if not HTTP.GET(MANGAINFO.URL:gsub('(/series/[^/]+)/[^/]+$', '%1') .. '/full-chapter-list') then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@class="flex items-center"]/a').Get() do
		local official = MODULE.GetOption('detect_official') and x.XPathString('span[1]/svg/@stroke', v) == '#d8b4fe' and '/official' or ''
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href') .. official)
		MANGAINFO.ChapterNames.Add(x.XPathString('span[2]/span[1]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL):gsub('/official$', '') .. '/images?reading_style=long_strip'

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img/@src', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end