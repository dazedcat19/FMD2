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

local DirectoryPagination = '/all-mangas?m_orderby=new-manga&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//span[@class="page-current"]'):match('%d+$')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//a[@class="manga-card"]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('.//h3', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local s = HTTP.Document.ToString():gsub('\\"', '"'):gsub('\\\\', '\\'):gsub('"%]%)</script><script>self%.__next_f%.push%(%[1,"', '')
	local x = CreateTXQuery(s)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="detail-cover"]//img/@src'))
	MANGAINFO.Authors   = x.XPathString('//li[span="Author"]//a')
	MANGAINFO.Artists   = x.XPathString('//li[span="Artist"]//a')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="detail-genres"]/a, upper-case(substring(//div[span="Type"]/span[2], 1, 1)) || lower-case(substring(//div[span="Type"]/span[2], 2)))')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[span="Status"]/span[2]'), 'On Going', 'Completed', 'On Hold', 'Canceled')
	MANGAINFO.Summary   = x.XPathString('//div[@class="synopsis-text"]/p')

	local slug = URL:match('/([^/]+)$')
	for v in x.XPath('parse-json(//script[contains(., "langChapters")]/substring-before(substring-after(., "langChapters"":"), ",""hasVolumes"))()[coinPrice=0]').Get() do
		local number = v.GetProperty('number').ToString()
		local title = v.GetProperty('title').ToString()

		title = (title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add('series/' .. slug .. '/' .. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. number .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('//div[contains(@class, "image-with-skeleton")]//img/@src').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
	end

	return true
end