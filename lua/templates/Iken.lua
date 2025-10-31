----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

ChapterName = 'Chapter '
DirectoryPagination = '/api/query?perPage=9999'
New = false

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Sign in to the current website.
function _M.Login()
	local u = MODULE.RootURL .. '/auth/signin'

	if MODULE.Account.Enabled == false then return false end

	local s = '[{"email":"' .. MODULE.Account.Username ..
	'","password":"' .. MODULE.Account.Password .. '"}]'
	MODULE.Account.Status = asChecking

	if HTTP.POST(u, s) then
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
	if API_URL ~= '' then API_URL = API_URL else API_URL = MODULE.RootURL end
	local u = API_URL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).posts()').Get() do
		if v.GetProperty('isNovel').ToString() ~= 'true' then
			LINKS.Add('series/' .. v.GetProperty('slug').ToString())
			NAMES.Add(v.GetProperty('postTitle').ToString())
		end
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local s = HTTP.Document.ToString():gsub('\\"', '"'):gsub('\\\\"', ''):gsub('\\\\', '\\'):gsub('"%]%)</script><script>self%.__next_f%.push%(%[1,"', '')
	local x = CreateTXQuery(s)

	local json_root_key
	if New then
		MANGAINFO.Summary = x.XPathString('(//div[contains(@class, "xl:leading-relaxed")])[1]/string-join(p[not(contains(., "Alt title"))], "\r\n")')
		local w = '{"series"' .. x.XPathString('//script[contains(., "totalChapterCount")]/substring-before(substring-after(., "{""series"""), "false}]}],")') .. 'false}'
		if w == '' then w = '{"series"' .. x.XPathString('//script[contains(., "totalChapterCount")]/substring-before(substring-after(., "{""series"""), "],[")') end
		x.ParseHTML(w)
		json_root_key = 'series'
	else
		MANGAINFO.Summary = x.XPathString('//meta[@name="description"]/@content'):gsub('</p><p>', '\r\n')
		local w = '{"post"' .. x.XPathString('//script[contains(., "postId")]/substring-before(substring-after(., "{""post"""), "}]]")') .. '}'
		x.ParseHTML(w)
		json_root_key = 'post'
	end

	local json = x.XPath('json(*).' .. json_root_key)
	MANGAINFO.Title     = x.XPathString('postTitle', json)
	MANGAINFO.AltTitles = x.XPathString('alternativeTitles', json)
	MANGAINFO.CoverLink = x.XPathString('featuredImage', json)
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Genres    = x.XPathStringAll('json(*).' .. json_root_key .. '.genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('seriesStatus', json), 'COMING_SOON|MASS_RELEASED|ONGOING', 'COMPLETED', 'HIATUS', 'CANCELLED|DROPPED')

	local type = x.XPathString('seriesType', json):gsub("^(%u)(%u*)", function(first, rest) return first .. rest:lower() end)
	local genres = {}
	if MANGAINFO.Genres and MANGAINFO.Genres ~= '' then table.insert(genres, MANGAINFO.Genres) end
	if type and type ~= '' then table.insert(genres, type) end
	MANGAINFO.Genres = table.concat(genres, ', ')

	if not HTTP.GET(API_URL .. '/api/chapters?take=999&order=asc&postId=' .. x.XPathString('id', json)) then return net_problem end

	local slug = x.XPathString('slug', json)
	local show_paid_chapters = MODULE.GetOption('showpaidchapters')

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).post.chapters()').Get() do
		local is_accessible = v.GetProperty('isAccessible').ToString() ~= 'false'

		if show_paid_chapters or is_accessible then
			local title = v.GetProperty('title').ToString()
			local chapter = v.GetProperty('number').ToString()
			local chapter_slug = v.GetProperty('slug').ToString()
			title = (title ~= 'null' and title ~= '-' and title ~= '') and (' - ' .. title) or ''

			MANGAINFO.ChapterLinks.Add('series/' .. slug .. '/' .. chapter_slug)
			MANGAINFO.ChapterNames.Add(ChapterName .. chapter .. title)
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local s = HTTP.Document.ToString():gsub('\\"', '"'):gsub('"%]%)</script><script>self%.__next_f%.push%(%[1,"', '')
	local x = CreateTXQuery(s)
	local json_extraction
	local prefix = ''
	local suffix = ''

	if New then
		json_extraction = '//script[contains(., "API_Response")]/substring-before(substring-after(., "API_Response"":"), "}],[")'
	else
		json_extraction = '//script[contains(., "images")]/substring-before(substring-after(., """chapter"""), "],")'
		prefix = '{"chapter"'
		suffix = ']}}'
	end

	x.ParseHTML(prefix .. x.XPathString(json_extraction) .. suffix)
	x.XPathStringAll('json(*).chapter.images().url', TASK.PageLinks)

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