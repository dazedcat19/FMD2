----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/query?perPage=9999'

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

	local s = HTTP.Document.ToString()
	local mid = s:match('&quot;postId&quot;:%[0,(%d+)%]') or s:match('{\\"postId\\":(%d+)}')

	if not HTTP.GET(API_URL .. '/api/post?postId=' .. mid) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('parse-json(.)?post')
	MANGAINFO.Title     = x.XPathString('postTitle', info)
	MANGAINFO.AltTitles = x.XPathString('alternativeTitles', info)
	MANGAINFO.CoverLink = x.XPathString('featuredImage', info)
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Artists   = x.XPathString('artist', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*/name, concat(upper-case(substring(seriesType, 1, 1)), lower-case(substring(seriesType, 2)))), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('seriesStatus', info), 'COMING_SOON|MASS_RELEASED|ONGOING', 'COMPLETED', 'HIATUS', 'CANCELLED|DROPPED')
	MANGAINFO.Summary   = x.XPathString('postContent', info)

	local slug = x.XPathString('slug', info)
	local show_paid_chapters = MODULE.GetOption('showpaidchapters')

	for v in x.XPath('chapters?*', info).Get() do
		local is_accessible = v.GetProperty('isAccessible').ToString() ~= 'false'

		if show_paid_chapters or is_accessible then
			local cid = v.GetProperty('id').ToString()
			local number = v.GetProperty('number').ToString()
			local slug_ch = v.GetProperty('slug').ToString()
			local title = v.GetProperty('title').ToString()

			local chapter = 'Chapter ' .. number

			if title == '' then
				title = chapter
			elseif not title:find(chapter, 1, true) then
				title = chapter .. ' - ' .. title
			end

			MANGAINFO.ChapterLinks.Add(slug .. '/' .. slug_ch .. '/' .. cid)
			MANGAINFO.ChapterNames.Add(title)
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = API_URL .. '/api/chapter?chapterId=' .. URL:match('/(%d+)$')

	if not HTTP.GET(u) then return false end

	local images = {}
	local has_order = false

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).chapter.images()').Get() do
		local url = v.GetProperty('url').ToString()
		local order_prop = v.GetProperty('order')

		local order = nil
		if order_prop then
			order = tonumber(order_prop.ToString())
			if order then
				has_order = true
			end
		end

		images[#images + 1] = {
			url = url,
			order = order
		}
	end

	if has_order then
		table.sort(images, function(a, b) return a.order < b.order end)
	end

	for _, img in ipairs(images) do
		TASK.PageLinks.Add(img.url)
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M