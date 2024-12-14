----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '7103ae6839ea46ec80cdfc2c4b37c803'
	m.Name                     = 'Asura Scans'
	m.RootURL                  = 'https://asuracomic.net'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/series?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	if x.XPath('//div[contains(@class, "md:grid-cols-5")]/a').Count == 0 then return no_error end
	for v in x.XPath('//div[contains(@class, "md:grid-cols-5")]/a').Get() do
		LINKS.Add(v.GetAttribute('href'):gsub('-(%w+)$', '-'))
		NAMES.Add(x.XPathString('div/div/div[2]/span[1]', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//span[@class="text-xl font-bold"]')
	MANGAINFO.CoverLink = x.XPathString('(//img[@alt="poster"])[1]/@src')
	MANGAINFO.Authors   = x.XPathString('//h3[contains(., "Author")]/following-sibling::h3')
	MANGAINFO.Artists   = x.XPathString('//h3[contains(., "Artist")]/following-sibling::h3')
	MANGAINFO.Genres    = x.XPathStringAll('//h3[contains(., "Genres")]/following-sibling::div/button')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//h3[contains(., "Status")]/following-sibling::h3'))
	MANGAINFO.Summary   = x.XPathString('//span[@class="font-medium text-sm text-[#A2A2A2]"]')

	for v in x.XPath('//div[contains(@class, "group")]/a').Get() do
		MANGAINFO.ChapterLinks.Add('series/' .. v.GetAttribute('href'):gsub('-(%w+)/chapter', '-/chapter'))
		MANGAINFO.ChapterNames.Add(x.XPathString('string(.//h3[contains(@class, "text-sm text-white font-medium")])', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local i, img = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	img = GetBetween("push(", "])", CreateTXQuery(HTTP.Document).XPathString('(//script[contains(., "published_at")])[last()]'):gsub('\\"', '"')) .. ']'
	for i in img:gmatch('"url":"(.-)"') do
		TASK.PageLinks.Add(i)
	end

	return no_error
end
