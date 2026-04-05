----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/series?perPage=100&sort=newest&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Sign in to the current website.
function _M.Login()
	local u = API_URL .. '/auth/login'

	if MODULE.Account.Enabled == false then return false end

	local s = '{"email":"' .. MODULE.Account.Username ..
	'","password":"' .. MODULE.Account.Password .. '"}'
	MODULE.Account.Status = asChecking
	HTTP.MimeType = 'application/json'

	if HTTP.POST(u, s) then
		if (HTTP.ResultCode == 200) and (HTTP.Cookies.Values['accessToken'] ~= '') then
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

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).totalPages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		if v.GetProperty('type').ToString() ~= 'NOVEL' then
			LINKS.Add('series/' .. v.GetProperty('slug').ToString())
			NAMES.Add(v.GetProperty('title').ToString())
		end
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = API_URL .. URL

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('parse-json(.)')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('alternativeTitles', json)
	MANGAINFO.CoverLink = x.XPathString('cover', json)
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, concat(upper-case(substring(type, 1, 1)), lower-case(substring(type, 2)))), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json))
	MANGAINFO.Summary   = x.XPathString('description', json)

	local page = 1
	local pages = nil
	local slug = x.XPathString('slug', json)
	local show_paid_chapters = MODULE.GetOption('showpaidchapters')
	while true do
		if not HTTP.GET(u .. '/chapters?perPage=100&sort=asc&page=' .. page) then return net_problem end
		for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
			local is_accessible = v.GetProperty('requiresPurchase').ToString() ~= 'true'

			if show_paid_chapters or is_accessible then
				local title = v.GetProperty('title').ToString()
				local chapter = v.GetProperty('number').ToString()
				local chapter_slug = v.GetProperty('slug').ToString()
				title = (title ~= 'null' and title ~= '') and (' - ' .. title) or ''

				MANGAINFO.ChapterLinks.Add('series/' .. slug .. '/chapters/' .. chapter_slug)
				MANGAINFO.ChapterNames.Add('Chapter ' .. chapter .. title)
			end
		end
		if not pages then
			pages = tonumber(x.XPathString('json(*).totalPages')) or 1
		end
		page = page + 1
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = API_URL .. URL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).images().url', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M