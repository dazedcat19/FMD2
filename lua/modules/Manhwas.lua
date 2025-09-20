----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, name, url, cat)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = cat
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
	end
	AddWebsiteModule('187417a02af74223a390dce786bb2a9f', 'ManhwasMen', 'https://manhwas.men', 'Raw')
	AddWebsiteModule('c8e02b7aaac1412180db86374fc799a8', 'ManhwasNet', 'https://manhwas.net', 'Spanish')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = {
	['187417a02af74223a390dce786bb2a9f'] = '/manga-list?page=',
	['c8e02b7aaac1412180db86374fc799a8'] = '/biblioteca?page='
}

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination[MODULE.ID] .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination[MODULE.ID] .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//article[@class="anime"]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('h3', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="thumb"]//img/@src')
	MANGAINFO.Genres    = x.XPathStringAll('//p[@class="genres"]//a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="meta"]/span[2]'), 'Publicándose|Ongoing', 'Finalizado|Complete', 'Pausado', 'Cancelado')
	MANGAINFO.Summary   = x.XPathString('string-join(//p[@class="sinopsis"]/text(), "\r\n")')

	for v in x.XPath('//ul[@class="episodes-list list-unstyled"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('.//p', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="chapter_imgs"]/img/@src', TASK.PageLinks)

	return true
end