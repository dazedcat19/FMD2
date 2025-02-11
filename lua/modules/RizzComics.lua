----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '3593adad980d454abe489c42e7158032'
	m.Name                     = 'Realm Oasis'
	m.RootURL                  = 'https://realmoasis.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
DirectoryPagination = '/comics'

----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

function FilterSeriesURL(original_url)
	return original_url:gsub("(/%d+/)([^/]+)", function(prefix, random_part)
		return '/123456789012345/' .. random_part:gsub("^....s....(%d%d%d%d%d).+", "aaaasaaaa%1aaaaaaaaaaaaaa")
	end)
end

function FilterChapterURL(original_url)
	return original_url:gsub("(/%d+/)([^/]+)", function(prefix, random_part)
		return '/123456789012345/' .. random_part:gsub("^....c....(%d%d%d%d%d)....(%d%d%d%d%d%d).+", "aaaacaaaa%1aaaa%2aaaa")
	end)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@class="bsx"]/a').Get() do
		LINKS.Add(FilterSeriesURL(v.GetAttribute('href')))
		NAMES.Add(v.GetAttribute('title'))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="entry-title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@itemprop="image"]//img/@src')
	MANGAINFO.Authors   = x.XPathString('(//div[@class="imptdt" and contains(., "Author")]/i)[1]')
	MANGAINFO.Artists   = x.XPathString('(//div[@class="imptdt" and contains(., "Artist")]/i)[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//span[@class="mgen"]/a')
	MANGAINFO.Summary   = x.XPathString('//div[@itemprop="description"]/substring-before(substring-after(., "var description = """), """;")')

	for v in x.XPath('//div[@id="chapterlist"]//div[@class="eph-num"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(FilterChapterURL(v.GetAttribute('href')))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[@class="chapternum"]/normalize-space(.)', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="readerarea"]/img/@src', TASK.PageLinks)

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end