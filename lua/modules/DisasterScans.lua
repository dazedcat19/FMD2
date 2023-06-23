----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'a24656d5e72544469f656e490ffc2591'
	m.Name                     = 'DisasterScans'
	m.RootURL                  = 'https://disasterscans.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/comics'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. '?page=' .. (URL + 1)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPathString('//div[contains(@class, "comicCards_cardGrid")]//a') == '' then return no_error end
	x.XPathHREFAll('//div[contains(@class, "comicCards_cardGrid")]//a', LINKS, NAMES)
	-- UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		local info = x.XPath('json(//script[@type="application/json"]).props.pageProps.comic')
		MANGAINFO.Title     = x.XPathString('ComicTitle', info)
		MANGAINFO.Summary   = x.XPathString('Description', info)
		MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "comicDetails_coverImage")]/@src')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('Status', info))
		MANGAINFO.Genres    = x.XPathStringAll('Genres()', info)
		MANGAINFO.Authors   = x.XPathString('Author', info)
		MANGAINFO.Artists   = x.XPathString('Artist', info)

		local chapters = x.XPath('json(//script[@type="application/json"]).props.pageProps.chapters()')
		for ic = 1, chapters.Count do
			local chapter_name = x.XPathString('ChapterName', chapters.Get(ic))
			local chapter_num  = x.XPathString('chapterNumber', chapters.Get(ic))
			local chapter_id   = x.XPathString('chapterID', chapters.Get(ic))

			MANGAINFO.ChapterLinks.Add(URL .. '/' .. chapter_id .. '-chapter-' .. chapter_num)
			if chapter_name ~= '' then chapter_name = ' - ' .. chapter_name end
			if chapter_num ~= ''  then chapter_num = string.format('Chapter %s', chapter_num) end
			MANGAINFO.ChapterNames.Add(chapter_num .. chapter_name)
		end
		MANGAINFO.ChapterLinks.Reverse()
		MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "container")]/img/@src', TASK.PageLinks)

	return no_error
end
