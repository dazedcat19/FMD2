----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/api/query?perPage=9999'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).posts()').Get() do
		LINKS.Add('series/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('postTitle').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local json, title, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML('{"post"' .. GetBetween('{"post"', '}]]', x.XPathString('//script[contains(., "postId")]'):gsub('\\"', '\"'):gsub('\\\\', '\\')) .. '}')
	json = x.XPath('json(*).post')
	MANGAINFO.Title     = x.XPathString('postTitle', json)
	MANGAINFO.CoverLink = x.XPathString('featuredImage', json)
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Genres    = x.XPathStringAll('json(*).genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('seriesStatus', json), 'COMING_SOON|HIATUS|MASS_RELEASED|ONGOING', 'CANCELLED|COMPLETED|DROPPED')
	MANGAINFO.Summary   = x.XPathString('postContent', json)

	if HTTP.GET(MODULE.RootURL .. '/api/chapters?postId=' .. x.XPathString('id', json) .. '&skip=0&take=9999&order=asc') then
		x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).post.chapters()').Get() do
			title = v.GetProperty('title').ToString()
			title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

			MANGAINFO.ChapterLinks.Add('series/' .. x.XPathString('slug', json) .. '/' .. v.GetProperty('slug').ToString())
			MANGAINFO.ChapterNames.Add('Chapter ' .. v.GetProperty('number').ToString() .. title)
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//section[contains(@class, "w-full flex")]//img/@src', TASK.PageLinks)

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M