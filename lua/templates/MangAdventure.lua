----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api/v2'
local DirectoryPagination = '?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. '/series' .. DirectoryPagination .. (URL + 1)
	HTTP.Headers.Values['Accept'] = '*/*'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPathString('json(*).last') == '' then return no_error end

	for v in x.XPath('json(*).results()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MODULE.RootURL .. API_URL .. '/series/' .. URL:match('/reader/(.-)/$')
	HTTP.Headers.Values['Accept'] = '*/*'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.AltTitles = x.XPathStringAll('json(*).aliases()')
	MANGAINFO.CoverLink = x.XPathString('json(*).cover')
	MANGAINFO.Authors   = x.XPathStringAll('json(*).authors()')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).artists()')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).categories()')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).status'))
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	HTTP.Reset()
	HTTP.Headers.Values['Accept'] = '*/*'

	if not HTTP.GET(u .. '/chapters') then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).results()').Get() do
		local title    = v.GetProperty('title').ToString()
		local volume   = v.GetProperty('volume').ToString()
		local chapter  = v.GetProperty('number').ToString()

		local name = ''
		if volume ~= 'null' then name = 'Vol. ' .. volume .. ' ' end
		if chapter ~= '0' then
			name = name .. 'Ch. ' .. chapter
		end
		if title ~= 'null' then
			if chapter ~= '0' then
				name = name .. ' - ' .. title
			else
				name = title
			end
		end

		MANGAINFO.ChapterLinks.Add(v.GetProperty('url').ToString())
		MANGAINFO.ChapterNames.Add(name)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local series, volume, number = URL:match('/reader/(.-)/(.-)/(.-)/$')
	local u = MODULE.RootURL .. API_URL .. '/pages?series=' .. series .. '&volume=' .. volume .. '&number=' .. number
	HTTP.Headers.Values['Accept'] = '*/*'

	if not HTTP.GET(u) then return false end

	for image in CreateTXQuery(HTTP.Document).XPath('json(*).results().image').Get() do
		TASK.PageLinks.Add(image.ToString())
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M