----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

AlphaList = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http headers for making a request.
local function SetRequestHeaders()
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
end

local function ChapterStableId(title, occurrence)
	local key = title
	if occurrence > 1 then
		key = string.format('%s\x01%d', title, occurrence)
	end
	local hash = require 'fmd.crypto'.MD5(key)
	hash = hash:gsub('.', function(c) return string.format('%02x', c:byte()) end)
	return hash:lower():sub(-10)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local i, s
	if MODULE.CurrentDirectoryIndex == 0 then
		s = '0-9'
	else
		i = MODULE.CurrentDirectoryIndex + 1
		s = AlphaList:sub(i, i)
	end
	local u = MODULE.RootURL .. '/category/' .. s .. '_' .. (URL + 1) .. '.html?sort=name'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	x.XPathHREFAll('//div[@class="manga-part-inner-info"]/a[1]', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//script[contains(., "all_pages")] ! substring-before(substring-after(., "all_pages = """), """")')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL):gsub('(.-/manga/.-)/.-(%.html)', '%1%2'):gsub('(.-/original/.-)/.-(%.html)', '%1%2')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//td[@class="bookside-general-type"]//div[./span="' .. XPathTokenAltTitles .. ':"]/text()')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="bookside-img-box"]//img[@itemprop="image"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//td[@class="bookside-general-type"]//div[@itemprop="author" and span="' .. XPathTokenAuthors .. ':"]/a/span')
	MANGAINFO.Artists   = x.XPathStringAll('//td[@class="bookside-general-type"]//div[@itemprop="author" and span="' .. XPathTokenArtists .. ':"]/a/span')
	MANGAINFO.Genres    = x.XPathStringAll('//td[@class="bookside-general-type"]//span[@itemprop="genre"]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="book-status"]'), StatusOngoing, StatusCompleted)
	MANGAINFO.Summary   = x.XPathString('//section[contains(@class, "detail-synopsis")]/text()[not(a)]')

	u = MANGAINFO.URL:gsub('(.-/manga/.-)/.-', '%1'):gsub('(.-/original/.-)/.-', '%1'):gsub('%.html', '') .. '/chapters.html'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	local seen = {}
	for v in x.XPath('//ul[contains(@class, "chapter-list")]/a').Get() do
		local href = v.GetAttribute('href')
		local title = x.XPathString('li/div/span[@class="chp-title"]', v)

		if not href:find('/chapter/', 1, true) then
			seen[title] = (seen[title] or 0) + 1
			local id = ChapterStableId(title, seen[title])
			MODULE.Storage[id] = href
			href = id
		end

		MANGAINFO.ChapterLinks.Add(href)
		MANGAINFO.ChapterNames.Add(title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	if not MODULE.Storage[URL:match('[^/]+$')]:find('/chapter/', 1, true) then
		local u = MODULE.Storage[URL:match('[^/]+$')]

		SetRequestHeaders()
		if not HTTP.GET(u) then return false end
	
		u = HTTP.Document.ToString():match('document%.location%s*=%s*["\'](.-)["\']')

		SetRequestHeaders()
		if not HTTP.GET(u) then return false end

		CreateTXQuery(HTTP.Document).XPathStringAll('json("[" || //script[contains(., "all_imgs_url")] ! substring-before(substring-after(., "all_imgs_url: ["), "]") || "]")()', TASK.PageLinks)
	else
		local u = MaybeFillHost(MODULE.RootURL, URL)

		if not HTTP.GET(u) then return false end

		CreateTXQuery(HTTP.Document).XPathStringAll('(//select[@class="sl-page"])[last()]/option/@value', TASK.PageContainerLinks)
		TASK.PageNumber = TASK.PageContainerLinks.Count
	end

	return true
end

-- Extract/Build/Repair image urls before downloading them.
function _M.GetImageURL()
	local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID]:gsub('^https?://[^/]+', ''))

	if not HTTP.GET(u) then return false end

	TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[contains(@class, "manga_pic")]/@src')

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M