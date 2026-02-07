----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fb34a56c83f54b19b57a9a92070fe899'
	m.Name                     = 'Flame Comics'
	m.RootURL                  = 'https://flamecomics.xyz'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/browse'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(//script[@id="__NEXT_DATA__"]).props.pageProps.series()').Get() do
		LINKS.Add('series/' .. v.GetProperty('series_id').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(//script[@id="__NEXT_DATA__"]).props.pageProps.series')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('string-join(altTitles?*, ", ")', json)
	MANGAINFO.CoverLink = MODULE.RootURL .. x.XPathString('//img[@alt="Cover"]/@src')
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Genres    = x.XPathString('string-join(tags?*, ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json), 'Ongoing', 'Completed', 'Hiatus', 'Cancelled|Dropped')
	MANGAINFO.Summary   = x.XPathString('description', json)

	local mid = x.XPathString('series_id', json)

	for v in x.XPath('json(//script[@id="__NEXT_DATA__"]).props.pageProps.chapters()').Get() do
		local chapter = math.floor(v.GetProperty('chapter').ToString())
		local title   = v.GetProperty('title').ToString()

		title = (title ~= 'null' and title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add('series/' .. mid .. '/' .. v.GetProperty('token').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. chapter .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('//div[@class="m_6d731127 mantine-Stack-root"]/img[@alt!=""]/@src').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
	end

	return no_error
end