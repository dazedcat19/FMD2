----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/api/get_all_series/'
UserAgent = 'FreeMangaDownloader'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local v, w, x = nil
	local u = MODULE.RootURL .. DirectoryPagination
	HTTP.UserAgent = UserAgent

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	for v in x.XPath('jn:keys(json(*))').Get() do
		for w in x.XPath('json(*)."' .. v.ToString() .. '".slug').Get() do
			NAMES.Add(v.ToString())
			LINKS.Add('manga/' .. w.ToString() .. '/')
		end
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local chapters, group, group_id, id, json, title, url, v, vol, w, x = nil
	local slug = URL:match('manga/([%w-]+)/')
	local optgroup = MODULE.GetOption('showgroup')
	local u = MODULE.RootURL .. '/api/series/' .. slug
	HTTP.UserAgent = UserAgent

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	json = x.XPath('json(*)')

	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('cover', json))
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Summary   = x.XPathString('description', json)

	chapters = [[
	for $k in jn:keys(chapters)
	return jn:object(object(("chapter_id", $k)), (chapters)($k))
	]]

	for v in x.XPath(chapters, json).Get() do
		id = x.XPathString('chapter_id', v)
		for w in x.XPath('jn:keys(groups)', v).Get() do
			vol   = x.XPathString('volume', v)
			id    = x.XPathString('chapter_id', v)
			title = x.XPathString('title', v)

			vol = vol ~= 'null' and vol ~= '0' and string.format('Vol. %s ', vol) or ''
			id = id ~= 'null' and string.format('Ch. %s ', id) or ''
			title = title ~= 'null' and title ~= '' and string.format('- %s', title) or ''

			group_id = w.ToString()
			group = ' [' .. x.XPathString('(groups)(' .. group_id .. ')', json) .. ']'
			if not optgroup then group = '' end

			url = 'read/manga/' .. slug .. '/' .. x.XPathString('chapter_id', v):gsub('%.', '-') .. '/#' .. group_id
			title = vol .. id .. title .. group
			MANGAINFO.ChapterLinks.Add(url)
			MANGAINFO.ChapterNames.Add(title)
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local folder, json, v, x = nil
	local ch = URL:match('/([%d%-]+)/#%d-$'):gsub('-', '.')
	local group_id = URL:match('/#(%d+)$')
	local slug = URL:match('manga/([%w-]+)/')
	local u = MODULE.RootURL .. '/api/series/' .. slug
	HTTP.UserAgent = UserAgent

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	json = x.XPath('(json(*).chapters)("' .. ch .. '")')
	folder = x.XPathString('folder', json)

	for v in x.XPath('jn:members((groups)(' .. group_id .. '))', json).Get() do
		TASK.PageLinks.Add(MODULE.RootURL .. '/media/manga/' .. slug .. '/chapters/' .. folder .. '/' .. group_id .. '/' .. v.ToString())
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	HTTP.UserAgent = UserAgent

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M