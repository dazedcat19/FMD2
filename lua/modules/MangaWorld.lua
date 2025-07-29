----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '86c14ab2b8774fa8a8461e07c3688a27'
	m.Name                     = 'MangaWorld'
	m.RootURL                  = 'https://www.mangaworld.nz'
	m.Category                 = 'Italian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/archive?sort=newest&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//script[contains(., "totalPages")]/substring-before(substring-after(., "totalPages"":"), ",""v")')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="comics-grid"]/div/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//div[./span="Titoli alternativi:"]/text()')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "thumb")]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[./span[contains(., "Autor")]]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//div[./span[contains(., "Artist")]]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[./span="Generi:"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[./span="Stato:"]/a'), 'In corso', 'Finito', 'In pausa', 'Cancellato')
	MANGAINFO.Summary   = x.XPathString('//div[@id="noidungm"]')

	for v in x.XPath('//div[contains(@class, "volume-element")]').Get() do
        for w in x.XPath('div//div[@class="chapter"]', v).Get() do
            MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', w))
            MANGAINFO.ChapterNames.Add(x.XPathString('div/p', v) .. ' - ' .. x.XPathString('a/span', w))
        end
    end
	if MANGAINFO.ChapterLinks.Count == 0 then
		for v in x.XPath('//div[@class="chapter pl-2"]/a').Get() do
            MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
            MANGAINFO.ChapterNames.Add(x.XPathString('span', v))
        end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not u:find('style=list', 1, true) then u = u:gsub('?style=pages', '') .. '?style=list' end

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="page-image img-fluid"]/@src', TASK.PageLinks)

	return true
end
