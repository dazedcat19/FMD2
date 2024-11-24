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

DirectoryPagination = '/browse'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(//script[@id="__NEXT_DATA__"]).props.pageProps.series()').Get() do
		LINKS.Add('series/' .. v.GetProperty('series_id').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local json, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	json = x.XPath('json(//script[@id="__NEXT_DATA__"]).props.pageProps.series')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.CoverLink = MODULE.RootURL .. x.XPathString('//img[@alt="Cover"]/@src')
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Genres    = x.XPathString('tags', json):gsub('%[', ''):gsub('%]', ''):gsub('"', '')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json), 'Ongoing|Hiatus', 'Completed|Dropped')
	MANGAINFO.Summary   = x.XPathString('description', json)

	for v in x.XPath('json(//script[@id="__NEXT_DATA__"]).props.pageProps.chapters()').Get() do
		MANGAINFO.ChapterLinks.Add('series/' .. v.GetProperty('series_id').ToString() .. '/' .. v.GetProperty('token').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('chapter').ToString():gsub('%.0', ''))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local i = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	for i in CreateTXQuery(HTTP.Document).XPath('//div[@class="m_6d731127 mantine-Stack-root"]/img/@src').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, i.ToString()))
	end

	return no_error
end