----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.cdnlibs.org/api'
local DirectoryPagination = '/manga?site_id[]=%s&sort_by=created_at&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = API_URL .. DirectoryPagination:format(SITE_ID) .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPathString('json(*).meta.has_next_page') == 'false' then return no_error end
	for v in x.XPath('json(*).data()').Get() do
		local name = v.GetProperty('rus_name').ToString()
		if name == '' then name = v.GetProperty('name').ToString() end

		LINKS.Add('ru/manga/' .. v.GetProperty('slug_url').ToString())
		NAMES.Add(name)
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local s = '?fields[]=tags&fields[]=authors&fields[]=artists&fields[]=genres&fields[]=status_id&fields[]=summary'
	local slug = '/' .. URL:match('/manga/(.-)$'):gsub('(.*)?.*', '%1')
	local u = API_URL .. '/manga' .. slug
	HTTP.Headers.Values['Site-Id'] = SITE_ID
	HTTP.Headers.Values['Authorization'] = MODULE.GetOption('auth')

	if not HTTP.GET(u .. s) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	MANGAINFO.CoverLink = x.XPathString('json(*).data.cover.thumbnail')
	MANGAINFO.Authors   = x.XPathStringAll('json(*).data.authors().name')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).data.artists().name')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).data.genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).data.scanlateStatus.label'), 'Продолжается', 'Завершён', 'Заморожен', 'Заброшен')
	MANGAINFO.Summary   = x.XPathString('json(*).data.summary')

	local tname = x.XPathString('json(*).data.name')
	local trname = x.XPathString('json(*).data.rus_name')
	local tename = x.XPathString('json(*).data.eng_name')
	MANGAINFO.Title = trname
	if trname == '' then MANGAINFO.Title = tname end
	MANGAINFO.AltTitles = ''
	if trname ~= '' and tname ~= tename then MANGAINFO.AltTitles = tname end
	if tename ~= '' and MANGAINFO.AltTitles ~= '' then
		MANGAINFO.AltTitles = MANGAINFO.AltTitles .. ', ' .. tename
	else
		MANGAINFO.AltTitles = tename
	end

	local tags = x.XPathStringAll('json(*).data.tags().name')
	if tags ~= '' then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. tags end

	local optgroup = MODULE.GetOption('showscangroup')
	HTTP.Reset()
	HTTP.Headers.Values['Site-Id'] = SITE_ID
	HTTP.Headers.Values['Authorization'] = MODULE.GetOption('auth')
	if HTTP.GET(u .. '/chapters') then
		local x = CreateTXQuery(HTTP.Document)
		local chapters = x.XPath('json(*).data()')
		for i = 1, chapters.Count do
			local v = chapters.Get(i)

			local branches = x.XPath('json(*).data()[' .. i .. '].branches()')
			for j = 1, branches.Count do
				local w = branches.Get(j)

				local bteam = x.XPath('json(*).data()[' .. i .. '].branches()[' .. j .. '].teams().name')
				local teams = {}
				for team = 1, bteam.Count do
					teams[team] = bteam.Get(team).ToString()
				end

				local bid = w.GetProperty('branch_id').ToString()
				local name = v.GetProperty('name').ToString()
				local volume = v.GetProperty('volume').ToString()
				local chapter = v.GetProperty('number').ToString()
				local scanlators = ' [' .. table.concat(teams, ", ") .. ']'

				bid = bid ~= 'null' and bid ~= '' and string.format('&branch_id=%s', bid) or ''
				name = name ~= 'null' and name ~= '' and string.format(' - %s', name) or ''

				if optgroup then
					if bteam.Count == 0 then scanlators = ' [no group]' end
				else
					scanlators = ''
				end

				MANGAINFO.ChapterLinks.Add('manga' .. slug .. '/chapter?number=' .. chapter .. '&volume=' .. volume .. bid)
				MANGAINFO.ChapterNames.Add('Том ' .. volume .. ' Глава ' .. chapter .. name .. scanlators)
			end
		end
	end

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local s = '/constants?fields[]=imageServers'
	local u = API_URL .. URL

	if not HTTP.GET(API_URL .. s) then return false end

	local svr = {'main', 'secondary', 'compress'}
	local sel_svr = (MODULE.GetOption('svr') or 0) + 1
	local server = CreateTXQuery(HTTP.Document).XPathString('json(*).data.imageServers()[id="' .. svr[sel_svr] .. '" and site_ids="' .. SITE_ID .. '"].url') .. '/'

	HTTP.Reset()
	HTTP.Headers.Values['Site-Id'] = SITE_ID
	HTTP.Headers.Values['Authorization'] = MODULE.GetOption('auth')

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	for pages in x.XPath('json(*).data.pages()').Get() do
		TASK.PageLinks.Add(server .. pages.GetProperty('url').ToString():gsub('^//', ''))
	end

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