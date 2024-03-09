----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'a51ebfb8979045d589cd867c48a095c0'
	m.Name                     = 'ManhwaFreak'
	m.RootURL                  = 'https://freakcomic.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------


DirectoryPagination = '/manga/?order=new'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="lastest-serie"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="info"]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//p[contains(., "Author(s)")]/following-sibling::p')
	MANGAINFO.Artists   = x.XPathString('//p[contains(., "Artist(s)")]/following-sibling::p')
	MANGAINFO.Genres    = x.XPathString('//p[contains(., "Genre(s)")]/following-sibling::p')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[contains(., "Status")]/following-sibling::p'))
	MANGAINFO.Summary   = x.XPathString('//div[@id="summary"]/p')

	for v in x.XPath('//div[@class="chapter-li"]/a').Get() do
		-- Skip chapters which are locked
		local locked = x.XPathString('div/svg/path/@d', v)
		if locked == nil or locked == '' then
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('div/div/p[1]', v))
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('run(', ', "<script', x.XPathString('//script[contains(., "ts_reader")]')))
	for v in x.XPath('json(*).sources()[1].images()').Get() do
		if string.find(v.ToString(), 'ajax') == nil then
			TASK.PageLinks.Add(v.ToString())
		end
	end

	return no_error
end
