----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'df01551e1739407a98669e37318842b0'
	m.Name                     = 'SoftKomik'
	m.RootURL                  = 'https://softkomik.com'
	m.Category                 = 'Indonesian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://v2.softkomik.com/komik'
local CDN_URL = 'https://image.softkomik.com/softkomik/'
local DirectoryPagination = '/komik/list?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end
	
	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(//script[@id="__NEXT_DATA__"]).props.pageProps.data.maxPage')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(//script[@id="__NEXT_DATA__"]).props.pageProps.data.data()').Get() do
		LINKS.Add(v.GetProperty('title_slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. URL

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.AltTitles = x.XPathString('json(*).title_alt')
	MANGAINFO.CoverLink = 'https://softkomik.com/_next/image?url=https://cover.softkomik.com/softkomik-cover/' .. x.XPathString('json(*).gambar') .. '&w=256&q=100'
	MANGAINFO.Authors   = x.XPathString('json(*).author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).Genre()') .. ', ' .. x.XPathString('json(*).type'):gsub("^%l", string.upper)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).status'))
	MANGAINFO.Summary   = x.XPathString('json(*).sinopsis')

	if not HTTP.GET(u .. '/chapter?limit=9999999') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).chapter()').Get() do
		MANGAINFO.ChapterLinks.Add(URL .. '/chapter/' .. v.GetProperty('chapter').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. v.GetProperty('chapter').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(//script[@id="__NEXT_DATA__"]).props.pageProps.data.data.imageSrc()').Get() do
		TASK.PageLinks.Add(CDN_URL .. v.ToString())
	end

	return true
end