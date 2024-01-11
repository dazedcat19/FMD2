----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'adaa36e7f591470685788d0e8ae31230'
	m.Name                     = 'Nartag'
	m.RootURL                  = 'https://nartag.com'
	m.Category                 = 'Webcomics'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------


DirectoryPagination = '/biblioteca?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFAll('//h4[@class="manga__title"]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//ul[@class="pagination"]/a[last()]')) or 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="manga__title"]/h2')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="manga__cover"]/img/@data-src')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="categories__list"]//a') .. ', ' .. x.XPathString('//div[@class="manga__demography shonen"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="manga__status"]/span'), 'En emisión', 'Finalizado')
	MANGAINFO.Summary   = x.XPathString('//div[@class="manga__description"]/p')

	for v in x.XPath('//div[@class="chapter__item"]').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('div[2]/a/@href', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('div[1]/h4', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="reader__item"]/img/@data-src', TASK.PageLinks)

	return no_error
end