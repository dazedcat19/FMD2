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
	if not HTTP.GET(API_URL .. DirectoryPagination .. 1) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).meta.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local v, x = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. x.XPathString('series_slug', v))
		NAMES.Add(x.XPathString('title', v))
	end

	return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local id, pages, thumbnail, title, slug, v, x = nil
	local page = 1
	local u = API_URL .. '/series/' .. URL:match('/series/(.-)$')

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.Authors   = x.XPathString('json(*).author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name')
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	thumbnail = x.XPathString('json(*).thumbnail')
	if string.find(thumbnail, 'media', 1, true) == nil then
		thumbnail = CDN_URL .. '/' .. thumbnail
	end
	MANGAINFO.CoverLink = thumbnail

	status = x.XPathString('json(*).status')
	if (status == 'Ongoing') or (status == 'Hiatus') then
		status = 'Ongoing'
	else
		status = 'Completed'
	end
	MANGAINFO.Status = MangaInfoStatusIfPos(status)

	id = x.XPathString('json(*).id')
	slug = '/' .. x.XPathString('json(*).series_slug') .. '/'
	while true do
		if HTTP.GET(API_URL .. '/chapter/query?page=' .. tostring(page) .. '&perPage=100&series_id=' .. id) then
			x = CreateTXQuery(HTTP.Document)
			pages = tonumber(x.XPathString('json(*).meta.last_page')) or 1
			for v in x.XPath('json(*).data()').Get() do
				title = x.XPathString('chapter_title', v)
				title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

				MANGAINFO.ChapterLinks.Add('chapter' .. slug .. x.XPathString('chapter_slug', v))
				MANGAINFO.ChapterNames.Add(x.XPathString('chapter_name', v) .. title)
			end
		else
			break
		end
		page = page + 1
		if page > pages then
			break
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local v, x = nil
	if not HTTP.GET(API_URL .. URL) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).chapter.chapter_data.images()').Get() do
		image = v.ToString()
		if string.find(image, 'media', 1, true) == nil then
			image = CDN_URL .. '/' .. image
		end
		TASK.PageLinks.Add(image)
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
