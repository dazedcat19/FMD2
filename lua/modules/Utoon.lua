----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2b8e4004d8bd434ca5d8b75da95499f9'
	m.Name                     = 'Utoon'
	m.RootURL                  = 'https://utoon.net'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/manga/page/'
local DirectoryParameters = '/?orderby=new'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1 .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="pager"]/a[last()-1]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="agrid"]/a', LINKS, NAMES)
	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@class="agrid"]/a[not(div[contains(., "(Novel)")])]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('div[@class="ac-t"]', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathStringAll('//div[@class="halt-list"]/span')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="poster"]//@src')
	MANGAINFO.Authors   = x.XPathString('//div[span="Author"]/span[2]')
	MANGAINFO.Artists   = x.XPathString('//div[span="Artist"]/span[2]')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="genres"]/a, //div[span="Type"]/span[2])')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[span="Status"]/span[2]'))
	MANGAINFO.Summary   = x.XPathString('//div[@id="syn"]')

	local page = 1
	local pages = nil
	while true do
		HTTP.Reset()
		HTTP.Headers.Values['Content-Length'] = 0
		HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'

		if not HTTP.POST(MANGAINFO.URL .. 'ajax/chapters/?t=' .. page) then return net_problem end

		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)

		if not pages then
			pages = tonumber(x.XPathString('//div[@class="pagination"]/span[not(contains(normalize-space(.), ">>"))][last()]/a')) or 1
		end
		page = page + 1
		if page > pages then
			break
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "page-break")]/img/@src', TASK.PageLinks)

	return true
end