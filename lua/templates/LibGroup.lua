----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://api.imglib.info/api'
DirectoryPagination = '/manga?site_id[]=%s&sort_by=created_at&page='
SITE_ID = ''

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local name, v, x = nil
	local u = API_URL .. DirectoryPagination:format(SITE_ID) .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	if x.XPathString('json(*).meta.has_next_page') == 'false' then return no_error end
	for v in x.XPath('json(*).data()').Get() do
		name = v.GetProperty('rus_name').ToString()
		if name == '' then name = v.GetProperty('name').ToString() end

		LINKS.Add('ru/manga/' .. v.GetProperty('slug_url').ToString())
		NAMES.Add(name)
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local bid, branches, bteam, chapter, chapters, j, name, optgroup, scanlators, tags, team, teams, v, volume, w, x = nil
	local s = '?fields[]=tags&fields[]=authors&fields[]=artists&fields[]=genres&fields[]=status_id&fields[]=summary'
	local slug = '/' .. URL:match('/manga/(.-)$'):gsub('(.*)?.*', '%1')
	local u = API_URL .. '/manga' .. slug

	if not HTTP.GET(u .. s) then return net_problem end

	x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	MANGAINFO.Title     = x.XPathString('json(*).data.rus_name')
	if MANGAINFO.Title == '' then MANGAINFO.Title = x.XPathString('json(*).data.name') end
	MANGAINFO.CoverLink = x.XPathString('json(*).data.cover.thumbnail')
	MANGAINFO.Authors   = x.XPathStringAll('json(*).data.authors().name')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).data.artists().name')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).data.genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).data.scanlateStatus.label'), 'Продолжается|Заморожен', 'Завершён|Заброшен')
	MANGAINFO.Summary   = x.XPathString('json(*).data.summary')

	tags = x.XPathStringAll('json(*).data.tags().name')
	if tags ~= '' then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. tags end

	optgroup = MODULE.GetOption('showscangroup')
	HTTP.Reset()
	HTTP.Headers.Values['Authorization'] = MODULE.GetOption('auth')
	if HTTP.GET(u .. '/chapters') then
		x = CreateTXQuery(HTTP.Document)
		chapters = x.XPath('json(*).data()')
		for i = 1, chapters.Count do
			v = chapters.Get(i)

			branches = x.XPath('json(*).data()[' .. i .. '].branches()')
			for j = 1, branches.Count do
				w = branches.Get(j)

				bteam = x.XPath('json(*).data()[' .. i .. '].branches()[' .. j .. '].teams().name')
				teams = {}
				for team = 1, bteam.Count do
					teams[team] = bteam.Get(team).ToString()
				end

				bid        = w.GetProperty('branch_id').ToString()
				name       = v.GetProperty('name').ToString()
				volume     = v.GetProperty('volume').ToString()
				chapter    = v.GetProperty('number').ToString()
				scanlators = ' [' .. table.concat(teams, ", ") .. ']'

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

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local i, server, sel_svr, svr = nil
	local s = '/constants?fields[]=imageServers'
	local u = API_URL .. URL

	if HTTP.GET(API_URL .. s) then
		svr = {'main', 'secondary', 'compress'}
		sel_svr = (MODULE.GetOption('svr') or 0) + 1
		server = CreateTXQuery(HTTP.Document).XPathString('json(*).data.imageServers()[id="' .. svr[sel_svr] .. '" and site_ids="' .. SITE_ID .. '"].url') .. '/'
	end

	HTTP.Reset()
	HTTP.Headers.Values['Authorization'] = MODULE.GetOption('auth')
	if not HTTP.GET(u) then return net_problem end

	for i in CreateTXQuery(HTTP.Document).XPath('json(*).data.pages()').Get() do
		TASK.PageLinks.Add(server .. i.GetProperty('url').ToString())
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M