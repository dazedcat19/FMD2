----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fb042c961d06479582edb2fa582e3a41'
	m.Name                     = 'ReaperScans'
	m.RootURL                  = 'https://reaperscans.com'
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
	if x.XPathString('//li//a[1]') == '' then return no_error end
	x.XPathHREFAll('//li//a[1]', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//*[contains(@class, "container")]//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[@aria-label="card"]//img/@src')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//section/div[@aria-label="card"]//div[./dt="Release Status"]/dd'))
	MANGAINFO.Summary   = x.XPathString('//section/div[@aria-label="card"]//p[1]')

	local pages = tonumber(x.XPathString('//nav//span[last()-1]/button')) or 1
	for page = 1, pages, 1 do
		local v for v in x.XPath('//li[contains(@*, "comic-chapter-list")]/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('(.//p)[1]', v))
		end

		if page > 1 then
			HTTP.GET(MaybeFillHost(MODULE.RootURL, URL .. '?page=' .. tostring(page)))
			x.ParseHTML(HTTP.Document)
			HTTP.Headers.Values['Referer'] = MANGAINFO.URL
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[contains(@class, "max-w-full")]/@src', TASK.PageLinks)

	return no_error
end
