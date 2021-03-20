----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '770b018081a74cb1ae983233d318ff87'
	m.Name                     = 'ComicFx'
	m.RootURL                  = 'https://comicfx.net'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaReaderOnline'
-- DirectoryParameters = '/'            --> Override template variable by uncommenting this line.
-- XPathTokenStatus    = 'Status'       --> Override template variable by uncommenting this line.
-- XPathTokenAuthors   = 'Author(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenGenres    = 'Categories'   --> Override template variable by uncommenting this line.

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('(//div[contains(@class, "container")]//h2)[1]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="thumb"]/img/@src')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//b[text()="Status:"]/following-sibling::i'))
	MANGAINFO.Authors   = x.XPathStringAll('//b[text()="Author:"]/following-sibling::a')
	MANGAINFO.Artists   = x.XPathStringAll('//b[text()="Artists:"]/following-sibling::a')
	MANGAINFO.Genres    = x.XPathStringAll('//b[text()="Genres:"]/following-sibling::a')
	MANGAINFO.Summary   = x.XPathString('//div[@class="sinopsis"]/p')

	for v in x.XPath('//div[@class="chaplist"]/ul/li/span').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('normalize-space(.)', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML('[' .. GetBetween('var pages = [', '];', x.XPathString('//script[contains(., "var pages")]')) .. ']')
	x.XPathStringAll('json(*)().page_image', TASK.PageLinks)
	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub("i%d.wp.com/", "")
		i = i + 1
	end

	return no_error
end
