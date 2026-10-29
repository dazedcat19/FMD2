----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local DirectoryPages = '/series/page/%s/?sort=newest'
local crypto = require 'fmd.crypto'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local path = URL == '0' and '/series/?sort=newest' or DirectoryPages:format(URL + 1)
	local u = MODULE.RootURL .. '/api/discovery?path=' .. crypto.EncodeURLElement(path)
	HTTP.Headers.Values['Origin'] = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).items()').Get() do
		LINKS.Add(v.GetProperty('path').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	if x.XPathString('json(*).hasMore') ~= 'true' then return no_error end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('replace(//p[@class="alternative-title"], " / ", ", ")')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="detail-cover"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[dt="Author"]//a[not(contains(@href, "/updating/"))]')
	MANGAINFO.Artists   = x.XPathStringAll('//div[dt="Artist"]//a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[dt="Genre"]//a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="status-badge"]'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="rich-content synopsis"]')

	local slug = URL:match('^/manhua/([^/]+)')
	local page = 1
	while true do
		HTTP.Reset()
		HTTP.Headers.Values['Origin'] = MODULE.RootURL
	
		u = MODULE.RootURL .. '/api/reader?resource=chapters&path=' .. crypto.EncodeURLElement('/manhua/' .. slug .. '/?tab=chapters&chapter_page=' .. page)

		if not HTTP.GET(u) then return net_problem end

		x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).items()').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetProperty('path').ToString())
			MANGAINFO.ChapterNames.Add(v.GetProperty('title').ToString())
		end
		if x.XPathString('json(*).hasMore') ~= 'true' then break end
		page = page + 1
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. '/api/reader?resource=content&path=' .. crypto.EncodeURLElement(URL)

	HTTP.Reset()
	HTTP.Headers.Values['Origin'] = MODULE.RootURL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).images().url', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '6e4b199622604f6fb77c95457dd1145e'
	m.Name                     = 'Manhua Top'
	m.RootURL                  = 'https://manhuatop.org'
	m.Category                 = 'Webcomics'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end