----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/wp-admin/admin-ajax.php?action=advanced_search'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination
	local s = 'orderby=updated&page=' .. (URL + 1)
	HTTP.MimeType = 'application/x-www-form-urlencoded'

	if not HTTP.POST(u, s) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local series = x.XPath('//div[contains(@class, "flex justify-center")]/a')
	if series.Count == 0 then return no_error end

	for v in series.Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('h1', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local json = require 'utils.json'
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.AltTitles = x.XPathString('//div[contains(@class, "text-sm text-text")]')
	MANGAINFO.Summary   = x.XPathString('string-join(//div[@data-show="false" and @itemprop="description"]//text(), "\r\n")')

	local mid = x.XPathString('//div[@id="chapter-list"]/@hx-get/substring-before(substring-after(., "manga_id="), "&")')

	if not HTTP.GET(MODULE.RootURL .. '/wp-json/wp/v2/manga/' .. mid .. '?_embed') then return net_problem end

	local x = json.decode(HTTP.Document.ToString())
	MANGAINFO.Title     = x.title.rendered
	MANGAINFO.CoverLink = x._embedded['wp:featuredmedia'][1].source_url

	local authors = {}
	local artists = {}
	local genres = {}
	local type = ''
	local status = ''

	for _, group in ipairs(x._embedded['wp:term'] or {}) do
		local first = group[1]
		if first then
			local taxonomy = first.taxonomy
			if taxonomy == 'series-author' then
				for _, v in ipairs(group) do
					authors[#authors + 1] = v.name
				end
			elseif taxonomy == 'artist' then
				for _, v in ipairs(group) do
					artists[#artists + 1] = v.name
				end
			elseif taxonomy == 'genre' then
				for _, v in ipairs(group) do
					genres[#genres + 1] = v.name
				end
			elseif taxonomy == 'type' then
				type = first.name
			elseif taxonomy == 'status' then
				status = first.name
			end
		end
	end

	if type ~= '' then
		type = ', '.. type
	end

	MANGAINFO.Authors = table.concat(authors, ', ')
	MANGAINFO.Artists = table.concat(artists, ', ')
	MANGAINFO.Genres  = table.concat(genres, ', ') .. type
	MANGAINFO.Status  = MangaInfoStatusIfPos(status)

	if not HTTP.GET(MODULE.RootURL .. '/wp-admin/admin-ajax.php?action=get_chapters&manga_id=' .. mid) then return net_problem end

	local chapters = json.decode(HTTP.Document.ToString())
	for _, chapter in ipairs(chapters.data) do
		MANGAINFO.ChapterLinks.Add(chapter.url)
		MANGAINFO.ChapterNames.Add(chapter.title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//section[@data-image-data]/img/@src', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M