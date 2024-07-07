----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '86c14ab2b8774fa8a8461e07c3688a27'
	m.Name                     = 'MangaWorld'
	m.RootURL                  = 'https://www.mangaworld.ac'
	m.Category                 = 'Italian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/archive?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. '1') then return net_problem end

	PAGENUMBER = tonumber(GetBetween('totalPages":', ',"v', CreateTXQuery(HTTP.Document).XPathString('//script[contains(., "totalPages")]'))) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. (URL + 1)) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="comics-grid"]/div/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "thumb")]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//span[contains(., "Autore")]/following-sibling::a')
	MANGAINFO.Artists   = x.XPathStringAll('//span[contains(., "Artista")]/following-sibling::a')
	MANGAINFO.Genres    = x.XPathStringAll('//span[contains(., "Generi")]/following-sibling::a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(., "Stato")]/following-sibling::a'), 'In corso', 'Finito')
	MANGAINFO.Summary   = x.XPathString('//div[@id="noidungm"]')

	local v for v in x.XPath('//div[contains(@class, "volume-element")]').Get() do
        local w for w in x.XPath('div//div[@class="chapter"]', v).Get() do
            MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', w))
            MANGAINFO.ChapterNames.Add(x.XPathString('div//p', v) .. ' - ' .. x.XPathString('a/span', w))
        end
    end
	if MANGAINFO.ChapterLinks.Count == 0 then
		local v for v in x.XPath('//div[@class="chapter pl-2"]/a').Get() do
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
	if string.find(u, 'style=list', 1, true) == nil then
		u = string.gsub(u, '?style=pages', '') .. '?style=list'
	end
	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="page-image img-fluid"]/@src', TASK.PageLinks)

	return no_error
end
