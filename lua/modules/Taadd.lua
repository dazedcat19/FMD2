----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                         = '05ebc869b7e0466690041551612fee1c'
	m.Name                       = 'Taadd'
	m.RootURL                    = 'https://www.taadd.com'
	m.Category                   = 'English'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.OnGetImageURL              = 'GetImageURL'
	m.TotalDirectory             = AlphaList:len()
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

AlphaList = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'
DirectoryPagination = '/category/%s_views_'
DirectorySuffix     = '.html'
MangaInfoParameters = '?waring=1'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function GetNameAndLink()
	local s, j, x = nil
	local i = 1
	if MODULE.CurrentDirectoryIndex == 0 then
		s = '0-9'
	else
		i = MODULE.CurrentDirectoryIndex + 1
		s = AlphaList:sub(i, i)
	end
	local u = MODULE.RootURL .. DirectoryPagination:format(s) .. (URL + 1) .. DirectorySuffix

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFTitleAll('//div[@class="intro"]/h2/a', LINKS, NAMES)
	j = tonumber(x.XPathString('(//span[@class="pagetor"])[1]//a[last()-1]')) or 1
	if j > i then i = j end
	UPDATELIST.CurrentDirectoryPageNumber = i

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not u:find(MangaInfoParameters) then u = u .. MangaInfoParameters end

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//title/substring-before(., " - Read ")')
	MANGAINFO.AltTitles = x.XPathString('//td[contains(., "Alternative")]/substring-after(text(), "Alternative:")')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('(//td)[1]/a/img/@src'))
	MANGAINFO.Authors   = x.XPathStringAll('//td[contains(., "Author")]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//td[contains(., "Categories")]/a')
	MANGAINFO.Summary   = x.XPathString('//td[contains(b, "Manga Summary")]/substring-after(., "Manga Summary")')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//td[contains(., "Status")]/a'), 'Updated', 'Completed')

	x.XPathHREFAll('//div[@class="chapter_list"]//td[1]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('(//select[@id="page"])[last()]/option/@value', TASK.PageContainerLinks)
	TASK.PageNumber = TASK.PageContainerLinks.Count

	return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
	local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])

	if not HTTP.GET(u) then return net_problem end

	TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[@id="comicpic"]/@src')

	return true
end