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
	m.OnCheckSite				= 'CheckSite'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/search/data?limit=32&sort=Recently+Added&order=Descending&official=Any&display_mode=Minimal+Display&offset='
local DirectoryPageLimit = 32

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------
function CheckSite()
    MANGACHECK.MangaURL     = "/series/01J76XYGGM22WZP7T4TKA4ZFAF/Kagurabachi"
    MANGACHECK.MangaTitle   = 'Kagurabachi'
    MANGACHECK.ChapterURL   = "/chapters/01K4JBY7YN9XEZ35F5GECH6ZWP"
    MANGACHECK.ChapterTitle = 'Chapter 92.5'
end
-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 0
	PAGENUMBER = 9700

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
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[2]/span[1]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL) .. '/images?reading_style=long_strip'

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img/@src', TASK.PageLinks)

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end