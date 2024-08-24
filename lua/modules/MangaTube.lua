----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fc8d20df870e4f70853dac8141b34459'
	m.Name                     = 'MangaTube'
	m.RootURL                  = 'https://manga-tube.me'
	m.Category                 = 'German'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.TotalDirectory           = #DirectoryPages
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPages = {'0', '1', '2', '3', '4', '5'}
DirectoryParameters = '/api/manga/search?year[]=1970&year[]=2024&status=-1&mature=-1&query=&rating[]=1&rating[]=5&sort=asc&order=name&type='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryParameters .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. '&page=' .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('json(*).pagination.last_page')) or 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local chapter, g, genrelist, json, status, title, v, volume, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('"data":', ',};', x.XPathString('//script[contains(., "titleOriginal")]')))
	json = x.XPath('json(*).manga.manga')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.CoverLink = x.XPathString('cover', json)
	MANGAINFO.Authors   = x.XPathStringAll('json(*).manga.manga.author().name')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).manga.manga.artist().name')
	MANGAINFO.Summary   = x.XPathString('description', json)

	MANGAINFO.Genres = ''
	genrelist = {}
	for g in x.XPath('json(*).manga.manga.genre()').Get() do
		genrelist[g.ToString()] = g.ToString()
	end
	for g in x.XPath('json(*).genre-map()').Get() do
		if genrelist[g.GetProperty('genre_id').ToString()] == g.GetProperty('genre_id').ToString() then MANGAINFO.Genres = MANGAINFO.Genres .. g.GetProperty('genre_name').ToString() .. ', ' end
	end
	MANGAINFO.Genres = MANGAINFO.Genres:sub(1, -2)

	status = x.XPathString('statusScanlation', json)
	if (status == '0') or (status == '1') then
		status = 'ongoing'
	else
		status = 'completed'
	end
	MANGAINFO.Status = MangaInfoStatusIfPos(status)

	for v in x.XPath('json(*).manga.chapters()').Get() do
		volume  = v.GetProperty('volume').ToString()
		chapter = v.GetProperty('number').ToString()
		title   = v.GetProperty('name').ToString()

		volume = volume ~= '0' and string.format('Band %s ', volume) or ''
		chapter = chapter ~= '' and string.format('Kapitel %s ', chapter) or ''
		title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

		MANGAINFO.ChapterLinks.Add(v.GetProperty('readerURL').ToString())
		MANGAINFO.ChapterNames.Add(volume .. chapter .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('"pages":', '},"chapters"', x.XPathString('//script[contains(., "adminURL")]')))
	x.XPathStringAll('json(*)().url', TASK.PageLinks)

	return no_error
end