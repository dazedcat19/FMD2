----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	Template.GetDirectoryPageNumber()

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLinkWithPagination()

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="agrid"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1/text()')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "wp-post-image")]/@src')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[span="Géneros"]/a, upper-case(substring(//span[contains(@class, "manga-title-badges")], 1, 1)) || lower-case(substring(//span[contains(@class, "manga-title-badges")], 2)))')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[span[1]="Estado"]/span[2]'))
	MANGAINFO.Summary   = x.XPathString('//div[@id="mbee38d1"]/p')

	local page, pages = 1
	while true do
		HTTP.Reset()
		HTTP.Headers.Values['Content-Length'] = 0
		HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'

		if not HTTP.POST(MANGAINFO.URL .. 'ajax/chapters/?t=' .. page) then return net_problem end

		x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#") and not(@class="reward_ads")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		if not pages then
			pages = tonumber(x.XPathString('//div[@class="pagination"]/span[not(contains(normalize-space(.), ">>"))][last()]/a')) or 1
		end
		if page >= pages then break end
		page = page + 1
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="wp-manga-chapter-img"]/@src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '44d1b00d78a54277b9a2af68c7980be2'
	m.Name                     = 'Dragon Translation'
	m.RootURL                  = 'https://dragontranslation.org'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end