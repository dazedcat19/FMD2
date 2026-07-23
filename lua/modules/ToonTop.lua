----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'b2e4d8c0f1a75b3d9e6c2f04a8b5d7e3'
	m.Name                     = 'ToonTop'
	m.RootURL                  = 'https://toontop.io'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

-- ToonTop is a Next.js site: page state is serialized in <script id="__NEXT_DATA__">.
-- The series page only embeds the 50 newest chapters (its pager and sort control both work
-- client-side on those), so the full list is pulled from the site's public API instead.
local NEXT_DATA = 'json(//script[@id="__NEXT_DATA__"])'
local API_URL   = 'https://api.toontop.io'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/latest?page=' .. (tonumber(URL) + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath(NEXT_DATA .. '.props.pageProps.items()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	UPDATELIST.CurrentDirectoryPageNumber =
		tonumber(x.XPathString(NEXT_DATA .. '.props.pageProps.pagination.total_pages')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local manga = x.XPath(NEXT_DATA .. '.props.pageProps.initialManga')

	MANGAINFO.Title     = x.XPathString('name', manga)
	MANGAINFO.AltTitles = x.XPathString('altName', manga)
	MANGAINFO.CoverLink = x.XPathString('cover', manga)
	MANGAINFO.Authors   = x.XPathString('string-join(authors()?name, ", ")', manga)
	MANGAINFO.Artists   = x.XPathString('string-join(artists()?name, ", ")', manga)
	MANGAINFO.Genres    = x.XPathString('string-join(genres()?name, ", ")', manga)
	MANGAINFO.Summary   = x.XPathString('summary', manga)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', manga),
	                                           'ONGOING|RELEASING',
	                                           'COMPLETED|FINISHED',
	                                           'HIATUS',
	                                           'CANCELLED|DROPPED')

	-- The embedded payload only carries the 50 newest chapters, so pull the full list from the
	-- API instead. Use each chapter's own 'url' and 'name': labels and slugs can disagree
	-- (e.g. "Chapter 101" is number 102) and decimal chapters exist (e.g. chapter-55-5).
	-- Grab the embedded ones first: the API call replaces HTTP.Document, and with it 'x'.
	local links, names = {}, {}
	for v in x.XPath(NEXT_DATA .. '.props.pageProps.initialManga.chapters()').Get() do
		links[#links + 1] = v.GetProperty('url').ToString()
		names[#names + 1] = v.GetProperty('name').ToString()
	end

	local mid = x.XPathString('id', manga)

	if mid ~= '' and HTTP.GET(API_URL .. '/titles/' .. mid .. '/chapters') then
		for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.chapters()').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetProperty('url').ToString())
			MANGAINFO.ChapterNames.Add(v.GetProperty('name').ToString())
		end
	end

	-- Fall back to the chapters embedded in the page if the API call yielded nothing.
	if MANGAINFO.ChapterLinks.Count == 0 then
		for i = 1, #links do
			MANGAINFO.ChapterLinks.Add(links[i])
			MANGAINFO.ChapterNames.Add(names[i])
		end
	end

	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	-- The cover CDN (rx.toontop.io) sits behind Cloudflare hotlink protection and rejects
	-- requests without a Referer. FMD2 fetches the cover after this function returns, so leave
	-- the header set for it — OnBeforeDownloadImage only covers chapter pages.
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	-- Reader images live in __NEXT_DATA__ as an ordered "images" array.
	CreateTXQuery(HTTP.Document).XPathStringAll(
		NEXT_DATA .. '.props.pageProps.initialChapter.images()', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'
	return true
end
