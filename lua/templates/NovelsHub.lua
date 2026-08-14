----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/series?sort=newest&page='
local NextJs = require 'utils.nextjs'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function FindSeriesData()
	local roots = NextJs.GetRootObjects(HTTP.Document.ToString())
	for _, root in ipairs(roots) do
		local data = NextJs.FindObject(root, function(v)
			return type(v) == 'table'
				and v.series
				and v.chapters
		end)
		if data then
			return data
		end
	end
	return nil
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1
	HTTP.Cookies.Values['content-mode'] = 'comics'

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(HTTP.Document.ToString():match('\\"initialTotal\\":(%d+)')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)
	HTTP.Cookies.Values['content-mode'] = 'comics'

	if not HTTP.GET(u) then return net_problem end

	local s = HTTP.Document.ToString():gsub('\\"', '"'):match('"initialSeries":(.-)]}],') .. ']}]'
	local data = require 'utils.json'.decode(s)
	for _, v in ipairs(data) do
		LINKS.Add('series/comic/' .. v.urlSlug)
		NAMES.Add(v.title)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local data = FindSeriesData()
	if not data then return no_error end

	local manga = data.series
	MANGAINFO.Title     = manga.title
	MANGAINFO.AltTitles = table.concat(manga.aliases or {}, ', ')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, manga.coverImage)
	MANGAINFO.Status    = MangaInfoStatusIfPos(manga.status, 'ONGOING', 'COMPLETED', 'HIATUS', 'DISCONTINUED|DROPPED')
	MANGAINFO.Summary   = CreateTXQuery(manga.description).XPathString('string-join(//text(), "\r\n")')

	local genres = {}

	for _, genre in ipairs(manga.genres or {}) do
		table.insert(genres, genre.name)
	end

	if manga.type then
		local capitalized = manga.type:sub(1, 1):upper() .. manga.type:sub(2):lower()
		table.insert(genres, capitalized)
	end

	MANGAINFO.Genres = table.concat(genres, ', ')

	local page = 1
	local pages = tonumber(data.totalPages) or 1
	local show_paid_chapters = MODULE.GetOption('showpaidchapters')
	while true do
		local chapters = data.chapters
		for i = 1, #chapters do
			local ch = chapters[i]

			if show_paid_chapters or ch.hasAccess then
				local title = ch.title
				local number = ch.number

				if not title:find('-', 1, true) then
					title = 'Chapter ' .. number
				end

				MANGAINFO.ChapterLinks.Add(MANGAINFO.URL .. '/chapter/' .. number)
				MANGAINFO.ChapterNames.Add(title)
			end
		end
		if page >= pages then break end
		page = page + 1
		if not HTTP.GET(MANGAINFO.URL .. '?page=' .. page) then break end
		data = FindSeriesData()
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local roots = NextJs.GetRootObjects(HTTP.Document.ToString())
	for _, root in ipairs(roots) do
		local chapter = NextJs.FindKey(root, 'chapter')
		if chapter then
			for i = 1, #chapter.pages do
				TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, chapter.pages[i].imageUrl))
			end
		end
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M