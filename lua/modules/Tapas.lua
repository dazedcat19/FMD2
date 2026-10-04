----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://story-api.tapas.io'
local DirectoryPages = { 'COMIC', 'MATURE_COMIC' }

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. '/cosmos/api/v1/landing/genre?category_type=' .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. '&sort_option=NEWEST_SERIES&subtab_id=17&size=200&page=' .. URL

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data.items()').Get() do
		LINKS.Add('series/' .. v.GetProperty('seriesId').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = math.ceil(x.XPathString('json(*).meta.pagination.totalCount') / x.XPathString('json(*).meta.pagination.size')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not u:find('/info$') then
		HTTP.Cookies.Values['birthDate'] = '2000-01-01'
		HTTP.Cookies.Values['adjustedBirthDate'] = '2000-01-01'
		if not HTTP.GET(u) then return net_problem end
		u = MaybeFillHost(MODULE.RootURL, CreateTXQuery(HTTP.Document).XPathString('//a[@class="nav-button ga-tracking"]/@href'))
	end

	HTTP.Reset()
	HTTP.Cookies.Values['birthDate'] = '2000-01-01'
	HTTP.Cookies.Values['adjustedBirthDate'] = '2000-01-01'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//a[@class="title"]')
	MANGAINFO.CoverLink = x.XPathString('//a[@class="thumb js-thumbnail"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//ul[@class="row-body creator-section"]//a[@class="name js-fb-tracking"]')
	MANGAINFO.Genres    = x.XPathString('//div[@class="info-detail__row"]/a')
	MANGAINFO.Summary   = x.XPathString('//span[@id="description__body"]')

	local mid = x.XPathString('//meta[@property="al:android:url"]/@content'):match('%d+')
	local show_paid_chapters = MODULE.GetOption('showpaidchapters')
	local page = 0
	while true do
		u = MODULE.RootURL .. '/series/' .. mid .. '/episodes?page=' .. page .. '&sort=OLDEST'

		if not HTTP.GET(u) then return net_problem end

		x = CreateTXQuery(HTTP.Document)
		local xpath = 'json(*).data.episodes()'
		if not show_paid_chapters then
			xpath = xpath .. '[not(free=false)]'
		end

		for v in x.XPath(xpath).Get() do
			MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
			MANGAINFO.ChapterNames.Add(v.GetProperty('title').ToString())
		end

		if x.XPathString('json(*).data.pagination.has_next') == 'false' then break end
		page = page + 1
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	HTTP.Reset()
	HTTP.Cookies.Values['birthDate'] = '2000-01-01'
	HTTP.Cookies.Values['adjustedBirthDate'] = '2000-01-01'
	local u = MODULE.RootURL .. '/episode' .. URL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="content__img js-lazy"]/@data-src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '9f2d722f3e90432ba913d5bdddc048b4'
	m.Name                     = 'Tapas'
	m.RootURL                  = 'https://tapas.io'
	m.Category                 = 'Webcomics'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.TotalDirectory           = #DirectoryPages
	m.SortedList               = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showpaidchapters'] = 'Show paid chapters'
		},
		['id_ID'] = {
			['showpaidchapters'] = 'Tampilkan bab berbayar'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('showpaidchapters', lang.showpaidchapters, false)
end