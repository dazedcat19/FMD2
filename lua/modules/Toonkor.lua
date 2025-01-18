----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2a7d69a1e1d24f90851b4e2598cffdcd'
	m.Name                     = 'Toonkor'
	m.RootURL                  = 'https://toonkor473.com'
	m.Category                 = 'Raw'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.TotalDirectory           = #DirectoryPages
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPages = {'%EC%9B%B9%ED%88%B0', '%EC%9B%B9%ED%88%B0/%EC%99%84%EA%B2%B0', '%EB%A7%9D%EA%B0%80',
	'%EB%8B%A8%ED%96%89%EB%B3%B8', '%EB%8B%A8%ED%96%89%EB%B3%B8/%EC%99%84%EA%B2%B0',
	'%ED%8F%AC%ED%86%A0%ED%88%B0', '%ED%8F%AC%ED%86%A0%ED%88%B0/%EC%99%84%EA%B2%B0'}

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/' .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1]

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="section-item-title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//table[@class="bt_view1"]//td[@class="bt_title"]')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//table[@class="bt_view1"]//td[@class="bt_thumb"]/a/img/@src'))
	MANGAINFO.Authors   = x.XPathString('//table[@class="bt_view1"]//span[contains(., "작가")]/following-sibling::span[1]')
	MANGAINFO.Summary   = x.XPathString('//table[@class="bt_view1"]//td[@class="bt_over"]')

	for v in x.XPath('//table[@class="web_list"]/tbody//tr/td[@class="content__title"]').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('data-role'))
		MANGAINFO.ChapterNames.Add(v.ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local i, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(require 'fmd.crypto'.DecodeBase64(GetBetween("toon_img = '", "';", x.XPathString('//script[contains(., "toon_img")]'))))
	for i in x.XPath('//img/@src').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, i.ToString()))
	end

	return no_error
end