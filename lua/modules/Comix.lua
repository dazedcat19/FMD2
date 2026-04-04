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
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
	m.MaxTaskLimit             = 2
	m.MaxConnectionLimit       = 4

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local translations = {
		['en'] = {
			['showscangroup'] = 'Show scanlation group',
			['deduplicatechapters'] = 'Deduplicate chapters (prefer official chapters, followed by the highest-voted or most recent)'
		},
		['id_ID'] = {
			['showscangroup'] = 'Tampilkan grup scanlation',
			['deduplicatechapters'] = 'Hapus bab ganda (utamakan bab resmi, diikuti yang paling banyak dipilih atau terbaru)'
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
local crypto = require 'fmd.crypto'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function ToBytes(s)
    local t = {}
    for i = 1, #s do
        t[i] = s:byte(i)
    end
    return t
end

local function FromBytes(t)
    local s = {}
    for i = 1, #t do
        s[i] = string.char(t[i])
    end
    return table.concat(s)
end

local function Ror(x, n) return ((x >> n) | (x << (8 - n))) & 0xFF end
local function Rol(x, n) return ((x << n) | (x >> (8 - n))) & 0xFF end
local function MutB(e) return (e - 12 + 256) % 256 end
local function MutC(e) return (e + 115) % 256 end
local function MutDollar(e) return Rol(e, 4) end
local function MutF(e) return (e - 188 + 256) % 256 end
local function MutG(e) return Rol(e, 2) end
local function MutH(e) return (e - 42 + 256) % 256 end
local function MutK(e) return (e - 241 + 256) % 256 end
local function MutL(e) return Ror(e, 1) end
local function MutM(e) return (e ~ 177) & 0xFF end
local function MutS(e) return (e + 143) % 256 end
local function MutUnderscore(e) return (e - 20 + 256) % 256 end
local function MutY(e) return Ror(e, 1) end

local raw_keys = {
    '13YDu67uDgFczo3DnuTIURqas4lfMEPADY6Jaeqky+w=',
    'yEy7wBfBc+gsYPiQL/4Dfd0pIBZFzMwrtlRQGwMXy3Q=',
    'yrP+EVA1Dw==',
    'vZ23RT7pbSlxwiygkHd1dhToIku8SNHPC6V36L4cnwM=',
    'QX0sLahOByWLcWGnv6l98vQudWqdRI3DOXBdit9bxCE=',
    'WJwgqCmf',
    'BkWI8feqSlDZKMq6awfzWlUypl88nz65KVRmpH0RWIc=',
    'v7EIpiQQjd2BGuJzMbBA0qPWDSS+wTJRQ7uGzZ6rJKs=',
    '1SUReYlCRA==',
    'RougjiFHkSKs20DZ6BWXiWwQUGZXtseZIyQWKz5eG34=',
    'LL97cwoDoG5cw8QmhI+KSWzfW+8VehIh+inTxnVJ2ps=',
    '52iDqjzlqe8=',
    'U9LRYFL2zXU4TtALIYDj+lCATRk/EJtH7/y7qYYNlh8=',
    'e/GtffFDTvnw7LBRixAD+iGixjqTq9kIZ1m0Hj+s6fY=',
    'xb2XwHNB'
}

local keys = {}
for i = 1, #raw_keys do
    keys[i] = ToBytes(crypto.DecodeBase64(raw_keys[i]))
end

local function RC4(key, data)
    local s = {}
    for i = 0, 255 do
		s[i] = i
	end

    local j = 0
    for i = 0, 255 do
        j = (j + s[i] + key[(i % #key) + 1]) % 256
        s[i], s[j] = s[j], s[i]
    end

    local i, j = 0, 0
    local out = {}
    for k = 1, #data do
        i = (i + 1) % 256
        j = (j + s[i]) % 256
        s[i], s[j] = s[j], s[i]
        out[k] = (data[k] ~ s[(s[i] + s[j]) % 256]) & 0xFF
    end

    return out
end

local round_maps = {
    {map = {[0] = MutC, [1] = MutB, [2] = MutY, [3] = MutDollar, [4] = MutH, [5] = MutS, [6] = MutH, [7] = MutK, [8] = MutL, [9] = MutC}, pref = 7},
    {map = {[0] = MutC, [1] = MutB, [2] = MutDollar, [3] = MutH, [4] = MutS, [5] = MutK, [6] = MutDollar, [7] = MutUnderscore, [8] = MutC, [9] = MutS}, pref = 6},
    {map = {[0] = MutC, [1] = MutF, [2] = MutS, [3] = MutG, [4] = MutY, [5] = MutM, [6] = MutDollar, [7] = MutK, [8] = MutS, [9] = MutB}, pref = 7},
    {map = {[0] = MutB, [1] = MutM, [2] = MutL, [3] = MutS, [4] = MutUnderscore, [5] = MutS, [6] = MutUnderscore, [7] = MutL, [8] = MutY, [9] = MutM}, pref = 8},
    {map = {[0] = MutUnderscore, [1] = MutS, [2] = MutC, [3] = MutM, [4] = MutB, [5] = MutM, [6] = MutF, [7] = MutS, [8]= MutDollar, [9] = MutG}, pref = 6},
}

local function GetMutKey(mk, idx)
    if #mk == 0 then return 0 end
    if (idx % 32) < #mk then
        return mk[(idx % #mk) + 1]
    end
    return 0
end

local function Round(data, key_idx)
    local k  = keys[key_idx]
    local mk = keys[key_idx + 1]
    local pk = keys[key_idx + 2]

    local cfg = round_maps[(key_idx // 3) + 1]
    local map, pref = cfg.map, cfg.pref

    local enc = RC4(k, data)
    local out = {}

    for i = 1, #enc do
        if i <= pref and i <= #pk then
            out[#out+1] = pk[i]
        end

        local v = (enc[i] ~ GetMutKey(mk, i - 1)) & 0xFF
        local f = map[(i - 1) % 10]
        if f then v = f(v) end

        out[#out+1] = v
    end

    return out
end

local b = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

local function EncodeBase64(data)
	return ((data:gsub('.', function(x)
		local r, bits = '', x:byte()
		for i = 8, 1, -1 do
			r = r .. (bits % 2 ^ i - bits % 2 ^ (i - 1) > 0 and '1' or '0')
		end
		return r
	end) .. '0000'):gsub('%d%d%d?%d?%d?%d?', function(x)
		if (#x < 6) then return '' end
		local c = 0
		for i = 1, 6 do
			c = c + (x:sub(i ,i) == '1' and 2 ^ (6 - i) or 0)
		end
		return b:sub(c + 1, c +1)
	end) .. ({ '', '==', '=' })[#data % 3 + 1])
end

function GenerateHash(path)
    local bytes = ToBytes(crypto.DecodeURL(path .. ':0:1'))

    bytes = Round(bytes, 1)
    bytes = Round(bytes, 4)
    bytes = Round(bytes, 7)
    bytes = Round(bytes, 10)
    bytes = Round(bytes, 13)

    return EncodeBase64(FromBytes(bytes)):gsub('%+', '-'):gsub('/', '_'):gsub('=+$', '')
end

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

	for v in CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).result.items()').Get() do
		LINKS.Add('title/' .. v.GetProperty('hash_id').ToString() .. '-' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('/title/([^%-]+)%-')
	local u = API_URL .. '/manga/' .. mid
	local s = '?includes[]=author&includes[]=artist&includes[]=genre&includes[]=theme&includes[]=demographic'

	if not HTTP.GET(u .. s) then return net_problem end

	local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
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
	local has_integer  = {}

	local page = 1
	local pages = nil
	local token = GenerateHash('/manga/' .. mid .. '/chapters')
	while true do
		if not HTTP.GET(u .. '/chapters?order[number]=asc&limit=100&page=' .. page .. '&time=1&_=' .. token) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).result.items()').Get() do
			local number = v.GetProperty('number').ToString()
			local id = v.GetProperty('chapter_id').ToString()
			local name = v.GetProperty('name').ToString()
			local vol_num = v.GetProperty('volume').ToString()
			local scan_group_id = tonumber(v.GetProperty('scanlation_group_id').ToString()) or 0
			local scan_group_name = v.GetProperty('scanlation_group').GetProperty('name').ToString()
			local votes = tonumber(v.GetProperty('votes').ToString()) or 0
			local updated_at = tonumber(v.GetProperty('updated_at').ToString()) or 0
			local official_str = v.GetProperty('is_official').ToString()
			local official = (official_str == '1' or official_str == 'true') and 1 or 0

			if not number:find('%.') then
				has_integer[number] = true
			end

			if not deduplicate then
				local volume = (vol_num ~= '0') and ('Vol. ' .. vol_num .. ' ') or ''
				local chapter = (number ~= '') and ('Ch. ' .. number) or ''
				local title = (name ~= '') and (' - ' .. name) or ''
				local scanlator = ''
				if optgroup then
					if scan_group_name ~= '' then
						scanlator = ' [' .. scan_group_name .. ']'
					elseif official == 1 then
						scanlator = ' [Official]'
					else
						scanlator = ' [Unknown]'
					end
				end

				MANGAINFO.ChapterLinks.Add(id)
				MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. scanlator)
			else
				local base = number:match('^(%d+)')
				local key = (base and has_integer[base]) and base or number
				local current = chapter_map[key]
				local ch_data = {
					id = id, name = name, vol_num = vol_num, number = number,
					scan_group_id = scan_group_id, scan_group_name = scan_group_name,
					votes = votes, updated_at = updated_at, official = official
				}

				if not current then
					chapter_map[key] = ch_data
					table.insert(chapter_list, key)
				else
					local new_official = ch_data.official == 1
					local cur_official = current.official == 1
					local new_group = ch_data.scan_group_id == 10702
					local cur_group = current.scan_group_id == 10702
					local better = false

					if new_official ~= cur_official then
						better = new_official
					elseif new_group ~= cur_group then
						better = new_group
					elseif ch_data.votes ~= current.votes then
						better = ch_data.votes > current.votes
					else
						better = ch_data.updated_at > current.updated_at
					end

					if better then
						chapter_map[key] = ch_data
					end
				end
			end
		end
		if not pages then
			pages = tonumber(x.XPathString('json(*).result.pagination.last_page')) or 1
		end
		page = page + 1
		if page > pages then
			break
		end
	end

	if deduplicate then
		for _, key in ipairs(chapter_list) do
			local ch = chapter_map[key]

			local volume = (ch.vol_num ~= '0') and ('Vol. ' .. ch.vol_num .. ' ') or ''
			local chapter = (ch.number ~= '') and ('Ch. ' .. ch.number) or ''
			local title = (ch.name ~= '') and (' - ' .. ch.name) or ''
			local scanlator = ''
			if optgroup then
				if ch.scan_group_name ~= '' then
					scanlator = ' [' .. ch.scan_group_name .. ']'
				elseif ch.official == 1 then
					scanlator = ' [Official]'
				else
					scanlator = ' [Unknown]'
				end
			end

			MANGAINFO.ChapterLinks.Add(ch.id)
			MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. scanlator)
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/chapters' .. URL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).result.images().url', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end