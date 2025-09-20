----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'c280ce32f36843fbba73dcc891e979af'
	m.Name                     = 'Philia Scans'
	m.RootURL                  = 'https://philiascans.org'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/page/'
DirectoryParameters = '/?post_type=wp-manga&sort=recently_added'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()]/a/@href'):match('paged=(%d+)$')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="info"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local x, v = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="serie-info"]/h1')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="serie-info"]/h6')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="main-cover"]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//div[@class="stat-details" and ./span="Author"]/span[2]')
	MANGAINFO.Artists   = x.XPathString('//div[@class="stat-details" and ./span="Artist"]/span[2]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genre-list"]//a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="stat-details" and ./span="Status"]/span[2]'), 'Releasing')
	MANGAINFO.Summary   = x.XPathString('//div[@class="modal-content p-4"]/p')

	for v in x.XPath('//div[@id="free-list"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('.//zebi/normalize-space(.)', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="ch-images"]//img/@src', TASK.PageLinks)

	return true
end