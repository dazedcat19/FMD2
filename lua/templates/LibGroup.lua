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
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http headers for making a request.
local function SetRequestHeaders()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.Headers.Values['Site-Id'] = SITE_ID
	HTTP.Headers.Values['Authorization'] = MODULE.GetOption('auth')
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = API_URL .. DirectoryPagination:format(SITE_ID) .. (URL + 1)
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*)')
	for v in x.XPath('data?*', json).Get() do
		local name = v.GetProperty('rus_name').ToString()
		if name == '' then name = v.GetProperty('name').ToString() end

		LINKS.Add('ru/manga/' .. v.GetProperty('slug_url').ToString())
		NAMES.Add(name)
	end
	if x.XPathString('meta.has_next_page', json) == 'false' then return no_error end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local s = '?fields[]=authors&fields[]=artists&fields[]=genres&fields[]=tags&fields[]=status_id&fields[]=summary'
	local slug = '/' .. URL:match('/manga/(.-)$'):gsub('(.*)?.*', '%1')
	local u = API_URL .. '/manga' .. slug
	SetRequestHeaders()

	if not HTTP.GET(u .. s) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('json(*).data')
	MANGAINFO.CoverLink = x.XPathString('cover?default', json)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*?name, ", ")', json)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*?name, ", ")', json)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, tags?*?name, type?label), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('scanlateStatus?label', json), 'Продолжается', 'Завершён', 'Заморожен', 'Заброшен')
	MANGAINFO.Summary   = x.XPathString('summary', json)

	local tname = x.XPathString('name', json)
	local trname = x.XPathString('rus_name', json)
	local tename = x.XPathString('eng_name', json)
	MANGAINFO.Title = trname
	if trname == '' then MANGAINFO.Title = tname end
	MANGAINFO.AltTitles = ''
	if trname ~= '' and tname ~= tename then MANGAINFO.AltTitles = tname end
	if tename ~= '' and MANGAINFO.AltTitles ~= '' then
		MANGAINFO.AltTitles = MANGAINFO.AltTitles .. ', ' .. tename
	else
		MANGAINFO.AltTitles = tename
	end

	local optgroup = MODULE.GetOption('showscangroup')

	HTTP.Reset()
	SetRequestHeaders()

	if not HTTP.GET(u .. '/chapters') then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		local volume  = v.GetProperty('volume').ToString()
		local chapter = v.GetProperty('number').ToString()
		local title   = v.GetProperty('name').ToString()
		local base    = 'manga' .. slug .. '/chapter?number=' .. chapter .. '&volume=' .. volume

		for w in x.XPath('branches?*[not(restricted_view)]', v).Get() do
			local teams = {}
			for team in x.XPath('teams?*?name', w).Get() do
				teams[#teams + 1] = team.ToString()
			end

			local bid = w.GetProperty('branch_id').ToString()
			local scanlators = ' [' .. table.concat(teams, ', ') .. ']'

			bid = (bid ~= 'null' and bid ~= '') and ('&branch_id=' .. bid) or ''
			title = (title ~= 'null' and title ~= '') and (' - ' .. title) or ''

			if optgroup then
				if #teams == 0 then scanlators = ' [no group]' end
			else
				scanlators = ''
			end

			MANGAINFO.ChapterLinks.Add(base .. bid)
			MANGAINFO.ChapterNames.Add('Том ' .. volume .. ' Глава ' .. chapter .. title .. scanlators)
		end
	end

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local s = '/constants?fields[]=imageServers'
	local u = API_URL .. URL
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(API_URL .. s) then return false end

	local svr = {'main', 'secondary', 'compress'}
	local sel_svr = (MODULE.GetOption('svr') or 0) + 1
	local server = CreateTXQuery(HTTP.Document).XPathString('json(*).data.imageServers()[id="' .. svr[sel_svr] .. '" and site_ids="' .. SITE_ID .. '"].url') .. '/'

	HTTP.Reset()
	SetRequestHeaders()

	if not HTTP.GET(u) then return false end

	for pages in CreateTXQuery(HTTP.Document).XPath('json(*).data.pages()').Get() do
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