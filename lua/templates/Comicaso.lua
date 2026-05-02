----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. '/komik/?mjv2_api=home_batch&mjv2_limit=10000'

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.items()').Get() do
		LINKS.Add('komik/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local slug = URL:gsub('/$', ''):match('([^/]+)$')
	local u = MODULE.RootURL .. '/wp-content/static/manga/' .. slug .. '.json'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(*)')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('alternative', info)
	MANGAINFO.CoverLink = x.XPathString('thumbnail', info)
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Artists   = x.XPathString('artist', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*, concat(upper-case(substring(type, 1, 1)), lower-case(substring(type, 2)))), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info), 'on-going', 'end')
	MANGAINFO.Summary   = x.XPathString('synopsis', info)

	local chapters = {}
	for v in x.XPath('chapters?*', info).Get() do
		table.insert(chapters, {
			slug = v.GetProperty('slug').ToString(),
			title = v.GetProperty('title').ToString()
		})
	end

	table.sort(chapters, function(a, b) return (tonumber(a.slug:match('(%d+)')) or 0) < (tonumber(b.slug:match('(%d+)')) or 0) end)

	for _, chapter in ipairs(chapters) do
		MANGAINFO.ChapterLinks.Add('komik/' .. slug .. '/' .. chapter.slug)
		MANGAINFO.ChapterNames.Add(chapter.title)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	HTTP.Reset()
	local u = MaybeFillHost(MODULE.RootURL, URL) .. '/'
	local s = '{"urls":["' .. u .. '"]}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(MODULE.RootURL .. '/wp-json/mp/v1/chapter', s) then return false end

	local body = HTTP.Document.ToString()
	local token = body:match('":"([a-f0-9]+)"')
	local expire = body:match('"expire":(%d+)')

	if not HTTP.GET(u .. '?t=' .. token .. '&e=' .. expire) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//figure/img[@class="mjv2-page-image"]/@src', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M