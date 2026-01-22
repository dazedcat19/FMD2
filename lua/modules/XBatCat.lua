----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, url, cat)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = 'XBatCat'
		m.RootURL                  = url
		m.Category                 = cat
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.SortedList               = true
	end
	AddWebsiteModule('5257a0c426b94accb6dcee3101308314', 'https://xbat.app', 'English')
	AddWebsiteModule('41e43d6fa1434937afad3bc04a1e8603', 'https://xbat.si')
	AddWebsiteModule('53347251db9d4d5eb92ef8bc6101e5f7', 'https://xbat.io')
	AddWebsiteModule('cf8702f7f5d24bd2a1b9b9904beb246b', 'https://xbat.me')
	AddWebsiteModule('ac808ac813a3499baa65bf640519ed59', 'https://xbat.tv')
	AddWebsiteModule('52c9306a3b93482ea3145c9e619b67fa', 'https://xbat.la')
	AddWebsiteModule('c7908a2cdb0c4966bff604ebedc9f468', 'https://xcat.tv')
	AddWebsiteModule('4040307fbc04489587bb71ffcefb3ccf', 'https://xcat.si')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/ap2/'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Get the language suffix.
local function GetLanguageCodeSuffix(s)
	if not s or s == '' then
		return ''
	end

	return ' [' .. string.upper(s:gsub('_', '-'):gsub('419', 'la')) .. ']'
end

-- Convert API genre keys to readable genre names.
local function GetGenre(genre)
	local genres = {
		['_4_koma'] = '4-Koma',
		['shoujo'] = 'Shoujo(G)',
		['shounen'] = 'Shounen(B)',
		['josei'] = 'Josei(W)',
		['seinen'] = 'Seinen(M)',
		['yuri'] = 'Yuri(GL)',
		['yaoi'] = 'Yaoi(BL)',
		['bara'] = 'Bara(ML)',
		['kodomo'] = 'Kodomo(Kid)',
		['old_people'] = 'Silver & Golden',
		['shoujo_ai'] = 'Shoujo ai',
		['shounen_ai'] = 'Shounen ai',
		['non_human'] = 'Non-human',
		['age_gap'] = 'Age Gap',
		['cheating_infidelity'] = 'Cheating/Infidelity',
		['childhood_friends'] = 'Childhood Friends',
		['college_life'] = 'College life',
		['contest_winning'] = 'Contest winning',
		['emperor_daughte'] = "Emperor's daughter",
		['fan_colored'] = 'Fan-Colored',
		['full_color'] = 'Full Color',
		['gender_bender'] = 'Gender Bender',
		['magical_girls'] = 'Magical Girls',
		['martial_arts'] = 'Martial Arts',
		['monster_girls'] = 'Monster Girls',
		['netorare'] = 'Netorare/NTR',
		['office_workers'] = 'Office Workers',
		['post_apocalyptic'] = 'Post-Apocalyptic',
		['reverse_harem'] = 'Reverse Harem',
		['reverse_isekai'] = 'Reverse Isekai',
		['royal_family'] = 'Royal family',
		['school_life'] = 'School Life',
		['sci_fi'] = 'Sci-Fi',
		['slice_of_life'] = 'Slice of Life',
		['sm_bdsm'] = 'SM/BDSM/SUB-DOM',
		['super_power'] = 'Super Power',
		['time_travel'] = 'Time Travel',
		['tower_climbing'] = 'Tower Climbing',
		['traditional_games'] = 'Traditional Games',
		['video_games'] = 'Video Games',
		['virtual_reality'] = 'Virtual Reality'
	}
	if genres[genre] then
		return genres[genre]
	end

	return (genre:gsub('^%l', string.upper))
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_comic_browse_pager($select: Comic_Browse_Select) { get_comic_browse_pager( select: $select ) { pages } }","variables":{"select":{"where":"browse","size":120,"sortby":"field_create","ignoreGlobalULangs":true,"ignoreGlobalGenres":true}}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).data.get_comic_browse_pager.pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_comic_browse($select: Comic_Browse_Select) { get_comic_browse( select: $select ) { items { data { id name tranLang } } } }","variables":{"select":{"where":"browse","page":' .. (URL + 1) .. ',"size":120,"sortby":"field_create","ignoreGlobalULangs":true,"ignoreGlobalGenres":true}}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.get_comic_browse.items().data').Get() do
		LINKS.Add('title/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('name').ToString() .. GetLanguageCodeSuffix(v.GetProperty('tranLang').ToString()))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('/(%d+)')
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_comicNode($id: ID!) { get_comicNode(id: $id) { data { name tranLang altNames urlCoverOri authors artists genres uploadStatus originalStatus summary extraInfo } } }","variables":{"id":"' .. mid .. '"}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('json(*).data.get_comicNode.data')
	MANGAINFO.Title     = x.XPathString('name', json) .. GetLanguageCodeSuffix(x.XPathString('tranLang', json))
	MANGAINFO.AltTitles = x.XPathString('string-join(altNames?*, ", ")', json)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('urlCoverOri', json))
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*, ", ")', json)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*, ", ")', json)
	MANGAINFO.Summary   = x.XPathString('summary', json)

	local genres = {}
	local seen = {}
	for genre in x.XPath('genres?*', json).Get() do
		local key = genre.ToString()
		if not seen[key] then
			table.insert(genres, GetGenre(key))
			seen[key] = true
		end
	end
	MANGAINFO.Genres = table.concat(genres, ', ')

	local status = x.XPathString('uploadStatus', json)
	if status == 'null' then status = x.XPathString('originalStatus', json) end
	MANGAINFO.Status = MangaInfoStatusIfPos(status)

	local extra_info = x.XPathString('extraInfo', json)
	if extra_info ~= '' then
		MANGAINFO.Summary = MANGAINFO.Summary .. '\r\n \r\nExtra Info:\r\n' .. extra_info
	end

	local s = '{"query":"query get_comic_chapterList($comicId: ID!, $start: Int) { get_comic_chapterList(comicId: $comicId, start: $start) { data { id dname title } } }","variables":{"comicId":"' .. mid .. '","start":-1}}'
	HTTP.Reset()
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.get_comic_chapterList().data').Get() do
		local chapter = v.GetProperty('dname').ToString()
		local title = v.GetProperty('title').ToString()
		title = (title ~= 'null' and title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(chapter .. title)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_chapterNode($id: ID!) { get_chapterNode(id: $id) { data { imageFile { urlList } } } }","variables":{"id":"' .. URL:match('(%d+)') .. '"}}'
	HTTP.Reset()
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.get_chapterNode.data.imageFile.urlList()', TASK.PageLinks)

	return true
end