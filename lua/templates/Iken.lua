----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = ''
ChapterName = 'Chapter '
DirectoryPagination = '/api/query?perPage=9999'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Sign in to the current website.
function _M.Login()
	local s, x = nil
	local login_url = MODULE.RootURL .. '/auth/signin'
	if MODULE.Account.Enabled == false then return false end
	s = '[{"email":"' .. MODULE.Account.Username ..
	'","password":"' .. MODULE.Account.Password .. '"}]'
	MODULE.Account.Status = asChecking
	if HTTP.POST(login_url, s) then
		if (HTTP.ResultCode == 200) and (HTTP.Cookies.Values['auth_session'] ~= '') then
			MODULE.Account.Status = asValid
			return true
		else
			MODULE.Account.Status = asInvalid
			return false
		end
	else
		MODULE.Account.Status = asUnknown
		return false
	end
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local v, x = nil
	if API_URL ~= '' then API_URL = API_URL else API_URL = MODULE.RootURL end
	local u = API_URL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).posts()').Get() do
		if v.GetProperty('isNovel').ToString() ~= 'true' then
			LINKS.Add('series/' .. v.GetProperty('slug').ToString())
			NAMES.Add(v.GetProperty('postTitle').ToString())
		end
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local demographic, json, title, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML('{"post"' .. GetBetween('{"post"', '}]]', x.XPathString('//script[contains(., "postId")]'):gsub('\\"', '\"'):gsub('\\\\', '\\')) .. '}')
	json = x.XPath('json(*).post')
	MANGAINFO.Title     = x.XPathString('postTitle', json)
	MANGAINFO.AltTitles = x.XPathString('alternativeTitles', json)
	MANGAINFO.CoverLink = x.XPathString('featuredImage', json)
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Genres    = x.XPathStringAll('json(*).post.genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('seriesStatus', json), 'COMING_SOON|MASS_RELEASED|ONGOING', 'COMPLETED', 'HIATUS', 'CANCELLED|DROPPED')
	MANGAINFO.Summary   = x.XPathString('postContent', json)

	demographic = x.XPathString('seriesType', json):gsub("^(%u)(%u*)", function(first, rest) return first .. rest:lower() end)
	if MANGAINFO.Genres ~= '' then
		MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. demographic
	else
		MANGAINFO.Genres = demographic
	end

	if API_URL ~= '' then API_URL = API_URL else API_URL = MODULE.RootURL end
	u = API_URL .. '/api/chapters?postId=' .. x.XPathString('id', json) .. '&skip=0&take=999&order=asc'

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).post.chapters()').Get() do
		title = v.GetProperty('title').ToString()
		title = title ~= 'null' and title ~= '-' and title ~= '' and string.format(' - %s', title) or ''

		if MODULE.GetOption('showpaidchapters') then
			MANGAINFO.ChapterLinks.Add('series/' .. x.XPathString('slug', json) .. '/' .. v.GetProperty('slug').ToString())
			MANGAINFO.ChapterNames.Add('Chapter ' .. v.GetProperty('number').ToString() .. title)
		else
			if v.GetProperty('isAccessible').ToString() ~= 'false' then
				MANGAINFO.ChapterLinks.Add('series/' .. x.XPathString('slug', json) .. '/' .. v.GetProperty('slug').ToString())
				MANGAINFO.ChapterNames.Add(ChapterName .. v.GetProperty('number').ToString() .. title)
			end
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('"images":', '],', x.XPathString('//script[contains(., "images")]'):gsub('\\"', '\"')) .. ']')
	x.XPathStringAll('json(*)().url', TASK.PageLinks)

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