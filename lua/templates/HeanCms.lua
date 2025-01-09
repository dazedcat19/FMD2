----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = ''
CDN_URL = ''
DirectoryPagination = '/query?series_type=Comic&order=desc&orderBy=created_at&perPage=100&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).meta.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local v = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1)
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. v.GetProperty('series_slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local id, pages, thumbnail, title, slug, v, x = nil
	local page = 1
	local u = API_URL .. '/series/' .. URL:match('/series/(.-)$')
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.Authors   = x.XPathString('json(*).author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).status'), 'Hiatus|Ongoing', 'Completed|Dropped')
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	thumbnail = x.XPathString('json(*).thumbnail')
	if string.find(thumbnail, 'media', 1, true) == nil then
		thumbnail = CDN_URL .. '/' .. thumbnail
	end
	MANGAINFO.CoverLink = thumbnail

	id = x.XPathString('json(*).id')
	slug = '/' .. x.XPathString('json(*).series_slug') .. '/'
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	while true do
		if not HTTP.GET(API_URL .. '/chapter/query?page=' .. tostring(page) .. '&perPage=100&series_id=' .. id) then return net_problem end
		x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).data()').Get() do
			title = v.GetProperty('chapter_title').ToString()
			title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

			if MODULE.GetOption('showpaidchapters') then
				MANGAINFO.ChapterLinks.Add('chapter' .. slug .. v.GetProperty('chapter_slug').ToString())
				MANGAINFO.ChapterNames.Add(v.GetProperty('chapter_name').ToString() .. title)
			else
				if v.GetProperty('price').ToString() == '0' then
					MANGAINFO.ChapterLinks.Add('chapter' .. slug .. v.GetProperty('chapter_slug').ToString())
					MANGAINFO.ChapterNames.Add(v.GetProperty('chapter_name').ToString() .. title)
				end
			end
		end
		page = page + 1
		pages = tonumber(x.XPathString('json(*).meta.last_page')) or 1
		if page > pages then
			break
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local image, i, x = nil
	local u = API_URL .. URL
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for i in x.XPath('json(*).chapter.chapter_data.images()').Get() do
		image = i.ToString()
		if string.find(image, 'media', 1, true) == nil then
			image = CDN_URL .. '/' .. image
		end
		TASK.PageLinks.Add(image)
	end
	if TASK.PageLinks.Count == 0 then
		for i in x.XPath('json(*).chapter.chapter_data.files()').Get() do
			TASK.PageLinks.Add(i.GetProperty('url').ToString())
		end
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M