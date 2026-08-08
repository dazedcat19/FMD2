----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/query/'

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
		['shounen'] = 'Shounen(B)',
		['shoujo'] = 'Shoujo(G)',
		['seinen'] = 'Seinen(M)',
		['josei'] = 'Josei(W)',
		['kodomo'] = 'Kodomo(Kid)',
		['silver_golden'] = 'Silver & Golden',
		['non_human'] = 'Non-human',
		['4_koma'] = '4 Koma',
		['award_winning'] = 'Award Winning',
		['fan_colored'] = 'Fan Colored',
		['full_color'] = 'Full Color',
		['long_strip'] = 'Long Strip',
		['official_colored'] = 'Official Colored',
		['web_comic'] = 'Web Comic',
		['age_gap'] = 'Age Gap',
		['art_by_ai'] = 'Art-by-AI',
		['boys_love'] = 'Boys Love',
		['brocon_siscon'] = 'Brocon Siscon',
		['cheating_infidelity'] = 'Cheating/Infidelity',
		['childhood_friends'] = 'Childhood Friends',
		['college_life'] = 'College life',
		['contest_winning'] = 'Contest winning',
		['death_game'] = 'Death Game',
		['emperors_daughter'] = "Emperor's Daughter",
		['female_protagonists'] = 'Female-protagonists',
		['girls_love'] = 'Girls Love',
		['magical_girls'] = 'Magical Girls',
		['male_protagonists'] = 'Male-protagonists',
		['martial_arts'] = 'Martial Arts',
		['master_servant'] = 'Master-Servant',
		['monster_girls'] = 'Monster Girls',
		['netorare_ntr'] = 'Netorare/NTR',
		['office_workers'] = 'Office Workers',
		['post_apocalyptic'] = 'Post-Apocalyptic',
		['reverse_harem'] = 'Reverse Harem',
		['reverse_isekai'] = 'Reverse Isekai',
		['royal_family'] = 'Royal family',
		['school_life'] = 'School Life',
		['sci_fi'] = 'Sci-Fi',
		['sexual_violence'] = 'Sexual Violence',
		['shoujo_ai'] = 'Shoujo ai',
		['shounen_ai'] = 'Shounen ai',
		['slice_of_life'] = 'Slice of Life',
		['sm_bdsm_sub_dom'] = 'SM/BDSM/SUB-DOM',
		['step_family'] = 'Step-family',
		['story_by_ai'] = 'Story-by-AI',
		['super_power'] = 'Super Power',
		['teacher_student'] = 'Teacher-Student',
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
	local s = '{"query":"query get_comic_browse_items($select: Comic_Browse_Select) { get_comic_browse_items( select: $select ) { data { id name translatedLanguage } } }","variables":{"select":{"where":"browse","page":' .. (URL + 1) .. ',"size":120,"sortby":"field_create","ignoreGlobalULangs":true,"ignoreGlobalGenres":true}}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.get_comic_browse_items().data').Get() do
		LINKS.Add('title/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('name').ToString() .. GetLanguageCodeSuffix(v.GetProperty('translatedLanguage').ToString()))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('^/[^/]+/([^-]+)')
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_comicNode($id: ID!) { get_comicNode(id: $id) { data { name translatedLanguage altNames urlCover authors artists genres originalStatus uploadStatus summary extraInfo } } }","variables":{"id":"' .. mid .. '"}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('parse-json(.)?data?get_comicNode?data')
	MANGAINFO.Title     = x.XPathString('name', info) .. GetLanguageCodeSuffix(x.XPathString('translatedLanguage', info))
	MANGAINFO.AltTitles = x.XPathString('string-join(altNames?*, ", ")', info)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('urlCover', info))
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*, ", ")', info)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*, ", ")', info)

	local genres = {}
	local seen = {}
	for genre in x.XPath('genres?*', info).Get() do
		local key = genre.ToString()
		if not seen[key] then
			table.insert(genres, GetGenre(key))
			seen[key] = true
		end
	end
	MANGAINFO.Genres = table.concat(genres, ', ')

	local status = x.XPathString('uploadStatus', info)
	if status == '' then status = x.XPathString('originalStatus', info) end
	MANGAINFO.Status = MangaInfoStatusIfPos(status)

	local summary = x.XPathString('summary', info)
	if summary ~= '' then
		MANGAINFO.Summary = CreateTXQuery(summary).XPathString('string-join(//text(), "\r\n")')
	end

	local extra_info = x.XPathString('extraInfo', info)
	if extra_info ~= '' then
		MANGAINFO.Summary = MANGAINFO.Summary .. '\r\n \r\nExtra Info:\r\n' .. extra_info
	end

	local page = 1
	local pages = nil
	while true do
		local s = '{"query":"query get_comic_chapterList_fullList($select: Select_Comic_ChapterList) { get_comic_chapterList_fullList(select: $select) { paging { pages } items { data { id dname title } } } }","variables":{"select":{"comic_id":"' .. mid .. '","page":' .. page .. ',"size":100,"sortby":"chapter_asc"}}}'

		HTTP.Reset()
		HTTP.MimeType = 'application/json'

		if not HTTP.POST(u, s) then return net_problem end

		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('parse-json(.)?data?get_comic_chapterList_fullList?items?*?data').Get() do
			local chapter = v.GetProperty('dname').ToString()
			local title = v.GetProperty('title').ToString()
			title = (title ~= '') and (' - ' .. title) or ''

			MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
			MANGAINFO.ChapterNames.Add(chapter .. title)
		end
		if not pages then
			pages = tonumber(x.XPathString('json(*).data.get_comic_chapterList_fullList.paging.pages')) or 1
		end
		if page >= pages then break end
		page = page + 1
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_chapterNode($id: ID!) { get_chapterNode(id: $id) { data { imageUrls } } }","variables":{"id":"' .. URL:match('[^/]+') .. '"}}'

	HTTP.Reset()
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.get_chapterNode.data.imageUrls()').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, url, cat)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = 'XComic'
		m.RootURL                  = url
		m.Category                 = cat
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.SortedList               = true
	end
	AddWebsiteModule('54f7ea4222e84dec965f2171c555292a', 'https://xcomic.me', 'English')
	AddWebsiteModule('baa132b2a72342e7a9bc7b2b6452f8df', 'https://xcomic.net')
end