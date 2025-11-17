----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '88a001d7619244ef98d13ecd869b8e64'
	m.Name                     = 'Comix'
	m.RootURL                  = 'https://comix.to'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
	m.MaxTaskLimit             = 2
	m.MaxConnectionLimit       = 4

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local translations = {
		['en'] = {
			['showscangroup'] = 'Show scanlation group',
			['deduplicatechapters'] = 'Deduplicate chapters (Prefer official chapters, followed by the highest-voted or most recent)'
		},
		['id_ID'] = {
			['showscangroup'] = 'Tampilkan grup scanlation',
			['deduplicatechapters'] = 'Hapus bab ganda (Utamakan bab resmi, diikuti yang paling banyak dipilih atau terbaru)'
		}
	}
	local lang = translations[slang] or translations['en']
	m.AddOptionCheckBox('showscangroup', lang.showscangroup, false)
	m.AddOptionCheckBox('deduplicatechapters', lang.deduplicatechapters, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://comix.to/api/v2'
local DirectoryPagination = '/manga?order[created_at]=desc&limit=100&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).result.pagination.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).result.items()').Get() do
		LINKS.Add('title/' .. v.GetProperty('hash_id').ToString() .. '-' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. '/manga/' .. URL:match('/title/([^%-]+)%-')
	local s = '?includes[]=author&includes[]=artist&includes[]=genre&includes[]=theme&includes[]=demographic'

	if not HTTP.GET(u .. s) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*).result')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.AltTitles = x.XPathString('string-join(alt_titles?*, ", ")', json)
	MANGAINFO.CoverLink = x.XPathString('poster/medium', json)
	MANGAINFO.Authors   = x.XPathString('string-join(author?*/title, ", ")', json)
	MANGAINFO.Artists   = x.XPathString('string-join(artist?*/title, ", ")', json)
	MANGAINFO.Genres    = x.XPathString('string-join((genre?*/title, theme?*/title, demographic?*/title, concat(upper-case(substring(type, 1, 1)), lower-case(substring(type, 2)))), ", ")', json)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', json), 'releasing', 'finished', 'on_hiatus', 'discontinued')
	MANGAINFO.Summary   = x.XPathString('synopsis', json)

	local deduplicate  = MODULE.GetOption('deduplicatechapters')
	local optgroup     = MODULE.GetOption('showscangroup')
	local chapter_map  = {}
	local chapter_list = {}

	local page = 1
	while true do
		if not HTTP.GET(u .. '/chapters?order[number]=asc&limit=100&page=' .. tostring(page)) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).result.items()').Get() do
			local number_str = v.GetProperty('number').ToString()
			local ch_id = v.GetProperty('chapter_id').ToString()
			local name = v.GetProperty('name').ToString()
			local volume = v.GetProperty('volume').ToString()
			local scan_group_id = v.GetProperty('scanlation_group_id').ToString()
			local scan_group_name = v.GetProperty('scanlation_group').GetProperty('name').ToString()
			local votes = v.GetProperty('votes').ToString()
			local updated_at = v.GetProperty('updated_at').ToString()

			if not deduplicate then
				local vol_str = (volume ~= '0') and ('Vol. ' .. volume .. ' ') or ''
				local ch_str = (number_str ~= '') and ('Ch. ' .. number_str) or ''
				local title_str = (name ~= '') and (' - ' .. name) or ''
				local scan_str = (scan_group_name ~= '') and optgroup and (' [' .. scan_group_name .. ']') or ''

				MANGAINFO.ChapterLinks.Add(ch_id)
				MANGAINFO.ChapterNames.Add(vol_str .. ch_str .. title_str .. scan_str)
			else
				local current = chapter_map[number_str]
				local ch_data = {
					id = ch_id, name = name, volume = volume, number = number_str,
					scan_group_id = scan_group_id, scan_group_name = scan_group_name,
					votes = votes, updated_at = updated_at
				}

				if not current then
					chapter_map[number_str] = ch_data
					table.insert(chapter_list, number_str)
				else
					local official_new = (ch_data.scan_group_id == 9275)
					local official_current = (current.scan_group_id == 9275)
					local better = false

					if official_new and not official_current then
						better = true
					elseif not official_new and official_current then
						better = false
					else
						if ch_data.votes > current.votes then
							better = true
						elseif ch_data.votes < current.votes then
							better = false
						elseif ch_data.updated_at > current.updated_at then
							better = true
						end
					end

					if better then
						chapter_map[number_str] = ch_data
					end
				end
			end
		end
		page = page + 1
		local pages = tonumber(x.XPathString('json(*).result.pagination.last_page')) or 1
		if page > pages then
			break
		end
	end

	if deduplicate then
		for _, number_str in ipairs(chapter_list) do
			local ch = chapter_map[number_str]

			local vol_str = (ch.volume ~= '0') and ('Vol. ' .. ch.volume .. ' ') or ''
			local ch_str = (ch.number ~= '') and ('Ch. ' .. ch.number) or ''
			local title_str = (ch.name ~= '') and (' - ' .. ch.name) or ''
			local scan_str = (ch.scan_group_name ~= '') and MODULE.GetOption('showscangroup') and (' [' .. ch.scan_group_name .. ']') or ''

			MANGAINFO.ChapterLinks.Add(ch.id)
			MANGAINFO.ChapterNames.Add(vol_str .. ch_str .. title_str .. scan_str)
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/chapters' .. URL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).result.images()', TASK.PageLinks)

	return true
end