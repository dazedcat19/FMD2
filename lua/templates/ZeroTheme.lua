----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/api/search?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).comics.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local v = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).comics.data()').Get() do
		LINKS.Add('manga/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local AltTitles, chapter, json, title, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('//div[@id="app"]/@data-page'))
	json = x.XPath('json(*).props.comic_infos')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.CoverLink = x.XPathString('json(*).props.settings.url_cdn') .. '/' .. x.XPathString('cover', json)
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Genres    = x.XPathStringAll('json(*).props.comic_infos.genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('statuses?*/name', json), 'Em andamento', 'Concluído')
	MANGAINFO.Summary   = x.XPathString('description', json)

	AltTitles = x.XPathString('alternative_name', json)
	if AltTitles ~= 'null' then MANGAINFO.AltTitles = AltTitles end

	for v in x.XPath('json(*).props.comic_infos.chapters()').Get() do
		chapter = v.GetProperty('chapter_number').ToString()
		title   = v.GetProperty('chapter_title').ToString()

		title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

		MANGAINFO.ChapterLinks.Add('chapter/' .. v.GetProperty('chapter_path').ToString())
		MANGAINFO.ChapterNames.Add('Capítulo ' .. chapter .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local CDN_URL, i, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('//div[@id="app"]/@data-page'))
	CDN_URL = x.XPathString('json(*).props.settings.url_cdn')

	for i in x.XPath('json(*).props.chapter.chapter.pages()').Get() do
		TASK.PageLinks.Add(CDN_URL .. '/' .. i.GetProperty('page_path').ToString())
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M