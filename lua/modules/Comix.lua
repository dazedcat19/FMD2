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

local API_URL = 'https://comix.to/api/v1'
local DirectoryPagination = '/manga?order[created_at]=desc&limit=100&page='
local crypto = require 'fmd.crypto'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local ops = {
	SR7L1 = function(e) return ((e >> 7) | (e << 1)) & 0xFF end,
	SL1R7 = function(e) return ((e << 1) | (e >> 7)) & 0xFF end,
	SR2L6 = function(e) return ((e >> 2) | (e << 6)) & 0xFF end,
	SL4R4 = function(e) return ((e << 4) | (e >> 4)) & 0xFF end,
	SR4L4 = function(e) return ((e >> 4) | (e << 4)) & 0xFF end,

	X37  = function(e) return (e ~ 37) & 0xFF end,
	X81  = function(e) return (e ~ 81) & 0xFF end,
	X147 = function(e) return (e ~ 147) & 0xFF end,
	X180 = function(e) return (e ~ 180) & 0xFF end,
	X218 = function(e) return (e ~ 218) & 0xFF end,

	P34  = function(e) return (e + 34) & 0xFF end,
	P159 = function(e) return (e + 159) & 0xFF end
}

local raw_keys = {
	'JxTcdyiA5GZxnbrmthXBQfU2IMTKcY1+3nNhbq98Sgo=',
	'3PordjODbhqla382Cxapmo/1JiABJQcjiJj1+48gTJ4=',
	'OaKvnI5ARA==',
	'MHNBHYWA7lvy867fXgvGcJwWDk79KqUJUVFsh3RwnnI=',
	'8i0Cru/VJBSVB2Y1GcMDVpzx2WepOcfnWdd81yxICl4=',
	'Fyskubz8VvA=',
	'B46L1x+UeWP+19cRpQ+OZvdLAK9EHID8g3mSgn57tew=',
	'DTSTmUt6LpDUw9r1lSQqyb3YlFTzruT8tk8wUGkwehQ=',
	'vY/meeI=',
	'7xWfIF5THL5LAnRgAARg+4mjWHPU9n3PQwvzbaMNi+Q=',
	'bewtiTuV+HJk56xxkf2iCljLgruCpBmN9BgE8i6gc9M=',
	'/Xcb2zAu8AU=',
	'WgeCQ3T8R51uTwVSiVa7Zy0dN6JOg6Z5JleMS+HV8Aw=',
	'yXayUVFrrcW56jQCEfZzuCidjpnWKjTDUNT7XeX9i7k=',
	'tSLco2w='
}

local keys = {}
for i = 1, #raw_keys do
    keys[i] = crypto.DecodeBase64(raw_keys[i])
end

local function RC4(data, key)
	local s = {}
	for i = 0, 255 do
		s[i] = i
	end

	local j = 0
	for i = 0, 255 do
		j = (j + s[i] + key:byte((i % #key) + 1)) % 256
		s[i], s[j] = s[j], s[i]
	end

	local i, j = 0, 0
	local out = {}
	for k = 1, #data do
		i = (i + 1) % 256
		j = (j + s[i]) % 256
		s[i], s[j] = s[j], s[i]
		out[k] = string.char((data:byte(k) ~ s[(s[i] + s[j]) % 256]) & 0xFF)
	end

	return table.concat(out)
end

local round_maps = {
	{map = {[0] = ops.SR7L1, [1] = ops.X37, [2] = ops.X81, [3] = ops.X147, [4] = ops.SR2L6, [5] = ops.SR4L4, [6] = ops.X218, [7] = ops.P159, [8] = ops.SR4L4, [9] = ops.X180}, pref = 7},
	{map = {[0] = ops.X180, [1] = ops.SL1R7, [2] = ops.X147, [3] = ops.SR7L1, [4] = ops.SR2L6, [5] = ops.SR4L4, [6] = ops.P159, [7] = ops.P34, [8] = ops.P159, [9] = ops.X180}, pref = 8},
	{map = {[0] = ops.X81, [1] = ops.SR4L4, [2] = ops.SL4R4, [3] = ops.X37, [4] = ops.P159, [5] = ops.SL1R7, [6] = ops.X180, [7] = ops.P34, [8] = ops.SR2L6, [9] = ops.SL4R4}, pref = 5},
	{map = {[0] = ops.X218, [1] = ops.SL1R7, [2] = ops.SR7L1, [3] = ops.P159, [4] = ops.SL1R7, [5] = ops.X180, [6] = ops.X147, [7] = ops.X218, [8] = ops.X180, [9] = ops.X37}, pref = 8},
	{map = {[0] = ops.SL4R4, [1] = ops.X147, [2] = ops.P34, [3] = ops.X147, [4] = ops.X218, [5] = ops.SL1R7, [6] = ops.X180, [7] = ops.SL1R7, [8] = ops.SR2L6, [9] = ops.X218}, pref = 5}
}

local function GetMutKey(mk, idx)
    if #mk == 0 then return 0 end
    if (idx % 32) < #mk then
        return mk:byte((idx % #mk) + 1)
    end
    return 0
end

local function Mutate(data, mut_key, pref_key, maps)
	local out = {}

	for i = 1, #data do
		local idx = i - 1

		if i <= maps.pref and i <= #pref_key then
			out[#out + 1] = string.char(pref_key:byte(i))
		end

		local n = (data:byte(i) ~ GetMutKey(mut_key, idx)) & 0xFF

		local op = maps.map[idx % 10]
		if op then
			n = op(n)
		end

		out[#out + 1] = string.char(n)
	end

	return table.concat(out)
end

local function Round(data, round)
	local base = ((round - 1) * 3) + 1

	local rc4_key  = keys[base]
	local mut_key  = keys[base + 1]
	local pref_key = keys[base + 2]

	local mut = Mutate(data, mut_key, pref_key, round_maps[round])

	return RC4(mut, rc4_key)
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
    local data = crypto.EncodeURLElement(path)

	for round = 1, 5 do
		data = Round(data, round)
	end

    return EncodeBase64(data):gsub('%+', '-'):gsub('/', '_'):gsub('=+$', '')
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*).result.meta.lastPage')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).result.items()').Get() do
		LINKS.Add('title/' .. v.GetProperty('hid').ToString() .. '-' .. v.GetProperty('slug').ToString())
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
	local info = x.XPath('json(*).result')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(altTitles?*, ", ")', info)
	MANGAINFO.CoverLink = x.XPathString('poster?medium', info)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*?title, ", ")', info)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*?title, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?title, theme?*?title, demographics?*?title, concat(upper-case(substring(type, 1, 1)), lower-case(substring(type, 2)))), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info), 'releasing', 'finished', 'on_hiatus', 'discontinued')
	MANGAINFO.Summary   = x.XPathString('synopsis', info)

	local deduplicate  = MODULE.GetOption('deduplicatechapters')
	local optgroup     = MODULE.GetOption('showscangroup')
	local chapter_map  = {}
	local chapter_list = {}
	local has_integer  = {}

	local page = 1
	local pages = nil
	local hash = GenerateHash('/manga/' .. mid .. '/chapters')
	while true do
		if not HTTP.GET(u .. '/chapters?order[number]=asc&limit=100&page=' .. page .. '&_=' .. hash) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).result.items()').Get() do
			local number = v.GetProperty('number').ToString()
			local id = v.GetProperty('id').ToString()
			local name = v.GetProperty('name').ToString()
			local vol_num = v.GetProperty('volume').ToString()
			local scan_group_id = tonumber(v.GetProperty('group').GetProperty('id').ToString()) or 0
			local scan_group_name = v.GetProperty('group').GetProperty('name').ToString()
			local votes = tonumber(v.GetProperty('votes').ToString()) or 0
			local updated_at = tonumber(v.GetProperty('updated_at').ToString()) or 0
			local official_str = v.GetProperty('isOfficial').ToString()
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
						better = ch_data.id > current.id
					end

					if better then
						chapter_map[key] = ch_data
					end
				end
			end
		end
		if not pages then
			pages = tonumber(x.XPathString('json(*).result.meta.lastPage')) or 1
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
	local hash = GenerateHash('/chapters' .. URL)
	local u = API_URL .. '/chapters' .. URL .. '?_=' .. hash

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).result.pages().url', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end