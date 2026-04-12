----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function md5_hex(s)
	local raw = require 'fmd.crypto'.MD5(s)
	return (raw:gsub('.', function(c)
		return string.format('%02x', string.byte(c))
	end))
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local crypto = require 'fmd.crypto'
	local u = MODULE.RootURL .. '/wp-json/manga/v1/load'
	local s = '{"page":"1","sort":"release_desc"}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
	local page = 1
	while true do
		local series = x.XPath('json(*)()')
		for v in series.Get() do
			LINKS.Add(v.GetProperty('url').ToString())
			NAMES.Add(v.GetProperty('title').ToString())
		end
		UPDATELIST.UpdateStatusText('Loading page ' .. page)
		if series.Count == 0 then break end
		page = page + 1
		HTTP.Reset()
		HTTP.MimeType = 'application/json'
		if not HTTP.POST(u, '{"page":"'.. page .. '","sort":"release_desc"}') then break end
		x.ParseHTML(crypto.HTMLEncode(HTTP.Document.ToString()))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[contains(@class, "flex-1 text-left")]/h1')
	MANGAINFO.AltTitles = x.XPathString('//div[contains(@class, "flex-1 text-left")]/p')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@alt, "Cover background")]/@src')
	MANGAINFO.Authors   = x.XPathString('//div[contains(@class, "flex-1 text-left")]//div[div="Author & Artist"]/div[1]')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[contains(@class, "flex-1 text-left")]//a[contains(@href, "/tag/")], //span[.//path[contains(@d, "M9 4.804A7.968")]])')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[contains(@class, "flex-1 text-left")]//span[.//path[@stroke-linecap="round"]]/text()'))
	MANGAINFO.Summary   = x.XPathString('//div[@id="description-content-tab"]/string-join(p, "\r\n")')

	local utc = os.date('!*t')
	local formatted = string.format('%04d%02d%02d%02d',
		utc.year, utc.month, utc.day, utc.hour
	)

	local mid = x.XPathString('//body/@data-manga-id')
	local timestamp = os.time()
	local token = md5_hex(timestamp .. 'mng_ch_' .. formatted):sub(1, 16)

	if not HTTP.GET(MODULE.RootURL .. '/auth/manga-chapters?manga_id=' .. mid .. '&offset=0&limit=9999&order=ASC&_t=' .. token .. '&_ts=' .. timestamp) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).chapters()').Get() do
		local chapter = v.GetProperty('chapter').ToString()
		local title   = v.GetProperty('title').ToString()

		title = (title ~= 'N/A' and title ~= '—' and title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(v.GetProperty('url').ToString())
		MANGAINFO.ChapterNames.Add('Chapter ' .. chapter .. title)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MODULE.RootURL .. '/auth/chapter-content?chapter_id=' .. URL:match('(%d+)$')

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).images()', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M