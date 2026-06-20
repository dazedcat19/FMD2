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
	m.OnDownloadImage          = 'DownloadImage'
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

local function GetNodejsScript(interceptor)
	return [[
	const resultJSON = await page.evaluate(async () => {
		return new Promise((resolve) => {
			const originalParse = JSON.parse;
			let submitted = false;
			
			const submit = (data) => {
				if (submitted) return;
				submitted = true;
				resolve(data);
			};

			JSON.parse = new Proxy(originalParse, {
				apply(target, thisArg, args) {
					const parsed = Reflect.apply(target, thisArg, args);
					try {
						]] .. interceptor .. [[
					} catch (e) {}
					return parsed;
				}
			});

			setTimeout(() => submit({ error: 'Timed out waiting for data' }), 60000);
		});
	});
	console.log(JSON.stringify(resultJSON));
	]]
end

local function GetPermutationMatrixLcg(seed, n)
	local arr = {}
	for i = 0, n - 1 do arr[i] = i end
	
	local state = seed
	local LCG_MULTIPLIER = 1664525
	local LCG_INCREMENT = 1013904223
	
	for i = n - 1, 1, -1 do
		state = (state * LCG_MULTIPLIER + LCG_INCREMENT) & 0xffffffff

		local j = state % (i + 1)

		local tmp = arr[i]
		arr[i] = arr[j]
		arr[j] = tmp
	end

	return arr
end

local function GetPermutationMatrixXorshift(seed, n)
	local arr = {}
	for i = 0, n - 1 do arr[i] = i end
	
	local state = seed | 1
	for i = n - 1, 1, -1 do
		state = (state ~ (state << 13)) & 0xffffffff
		state = (state ~ (state >> 17)) & 0xffffffff
		state = (state ~ (state << 5)) & 0xffffffff

		local j = state % (i + 1)

		local tmp = arr[i]
		arr[i] = arr[j]
		arr[j] = tmp
	end

	return arr
end

local function DecodeLcg(data, seed, length)
	local ENC_MULTIPLIER = 1000005
	local ENC_INCREMENT = 1234567891
	local state = seed & 0xffffffff
	local limit = math.min(length, #data)

	local decoded = {}
	for i = 1, limit do
		state = (state * ENC_MULTIPLIER + ENC_INCREMENT) & 0xffffffff
		local shift = (state >> 24) & 0xff
		local byte = string.byte(data, i)
		decoded[i] = string.char(byte ~ shift)
	end

	return table.concat(decoded) .. string.sub(data, limit + 1)
end

local function DecodeXorshift(data, seed, length, highByte)
	local state = seed & 0xffffffff
	local limit = math.min(length, #data)

	local decoded = {}
	for i = 1, limit do
		state = (state ~ (state << 13)) & 0xffffffff
		state = (state ~ (state >> 17)) & 0xffffffff
		state = (state ~ (state << 5)) & 0xffffffff
		
		local key = highByte and ((state >> 24) & 0xff) or (state & 0xff)
		local byte = string.byte(data, i)
		decoded[i] = string.char(byte ~ key)
	end

	return table.concat(decoded) .. string.sub(data, limit + 1)
end

local function HasImageSignature(data)
	if #data < 12 then return false end
	local b1, b2, b3, b4 = data:byte(1, 4)
	
	if b1 == 82 and b2 == 73 and b3 == 70 and b4 == 70 then -- RIFF
		local b9, b10, b11, b12 = data:byte(9, 12)
		if b9 == 87 and b10 == 69 and b11 == 66 and b12 == 80 then -- WEBP
			return true
		end
	end
	if b1 == 0xFF and b2 == 0xD8 then return true end -- JPEG
	if b1 == 0x89 and b2 == 80 and b3 == 78 and b4 == 71 then return true end -- PNG
	return false
end

local function DecodeEncodedBytes(data, seed, length)
	seed = seed & 0xffffffff
	
	local c1 = DecodeXorshift(data, seed, length, false)
	if HasImageSignature(c1) then return c1 end

	local c2 = DecodeXorshift(data, seed | 1, length, false)
	if HasImageSignature(c2) then return c2 end

	local c3 = DecodeXorshift(data, seed, length, true)
	if HasImageSignature(c3) then return c3 end

	local c4 = DecodeXorshift(data, seed | 1, length, true)
	if HasImageSignature(c4) then return c4 end

	local l1 = DecodeLcg(data, seed, length)
	if HasImageSignature(l1) then return l1 end
	
	local l2 = DecodeLcg(data, seed | 1, length)
	if HasImageSignature(l2) then return l2 end

	return c1
end

local function ParseGrid(header)
	if header == '' then return 5, 5 end

	local parts = {}
	for part in header:lower():gmatch('%d+') do
		table.insert(parts, tonumber(part))
	end
	
	if #parts == 1 and parts[1] > 1 then
		return parts[1], parts[1]
	elseif #parts >= 2 and parts[1] > 1 and parts[2] > 1 then
		return parts[1], parts[2]
	end
	
	return 5, 5
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
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	if HTTP.ResultCode ~= 200 then MANGAINFO.Title = 'Cloudflare workaround is required' return no_error end
	local x = CreateTXQuery(HTTP.Document)
	local info = require 'utils.json'.decode(x.XPathString('//script[@id="initial-data"]')).queries['["manga","detail","' .. mid .. '"]']

	local authors = {}
	for _, author in ipairs(info.authors or {}) do
		table.insert(authors, author.title)
	end

	local artists = {}
	for _, artist in ipairs(info.artists or {}) do
		table.insert(artists, artist.title)
	end

	local genres = {}
	for _, genre in ipairs(info.genres or {}) do
		table.insert(genres, genre.title)
	end
	for _, demo in ipairs(info.demographics or {}) do
		table.insert(genres, demo.title)
	end
	for _, theme in ipairs(info.theme or {}) do
		table.insert(genres, theme.title)
	end
	if info.type then
		local capitalized = info.type:sub(1,1):upper() .. info.type:sub(2):lower()
		table.insert(genres, capitalized)
	end

	MANGAINFO.Title     = info.title
	MANGAINFO.AltTitles = table.concat(info.altTitles or {}, ', ')
	MANGAINFO.CoverLink = info.poster.medium
	MANGAINFO.Authors   = table.concat(authors, ', ')
	MANGAINFO.Artists   = table.concat(artists, ', ')
	MANGAINFO.Genres    = table.concat(genres, ', ')
	MANGAINFO.Status    = MangaInfoStatusIfPos(info.status, 'releasing', 'finished', 'on_hiatus', 'discontinued')
	MANGAINFO.Summary   = info.synopsis

	local interceptor = [[
		const items = window._capturedChapters || [];
		window._capturedChapters = items;

		if (parsed && parsed.result && Array.isArray(parsed.result.items) && parsed.result.items.length > 0) {
			if (parsed.result.items[0].mangaId) {
				const meta = parsed.result.meta || parsed.result.pagination;
				for (const it of parsed.result.items) items.push(it);

				if (meta && meta.hasNext) {
					setTimeout(() => {
						const btn = document.querySelector('.mchap-foot button[aria-label*=Next]');
						if (btn && !btn.disabled) btn.click();
					}, 200);
				} else {
					submit({ items: items });
				}
			}
		}
	]]

	local now = os.time()
	local output = MODULE.Storage[mid]
	local timestamp = tonumber(MODULE.Storage[mid .. '_time']) or 0

	if output == '' or (now - timestamp) >= 900 then
		local js_code = GetNodejsScript(interceptor)
		output = require 'utils.nodejs'.run_html_load_with_js(MODULE.RootURL .. URL, js_code)

		if not output:find('Timed out', 1, true) then
			MODULE.Storage[mid] = output
			MODULE.Storage[mid .. '_time'] = tostring(now)
		end
	end
	x.ParseHTML(output)

	local deduplicate  = MODULE.GetOption('deduplicatechapters')
	local optgroup     = MODULE.GetOption('showscangroup')
	local chapter_map  = {}
	local chapter_list = {}
	local has_integer  = {}

	for v in x.XPath('json(*).items()').Get() do
		local number = v.GetProperty('number').ToString()
		local id = v.GetProperty('id').ToString()
		local name = v.GetProperty('name').ToString()
		local vol_num = v.GetProperty('volume').ToString()
		local scan_group_id = tonumber(v.GetProperty('group').GetProperty('id').ToString()) or 0
		local scan_group_name = v.GetProperty('group').GetProperty('name').ToString()
		local votes = tonumber(v.GetProperty('votes').ToString()) or 0
		local official_str = v.GetProperty('isOfficial').ToString()
		local official = (official_str == '1' or official_str == 'true') and 1 or 0
		local url = v.GetProperty('url').ToString()

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

			MANGAINFO.ChapterLinks.Add(url)
			MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. scanlator)
		else
			local base = number:match('^(%d+)')
			local key = (base and has_integer[base]) and base or number
			local current = chapter_map[key]
			local ch_data = {
				id = id, name = name, vol_num = vol_num, number = number,
				scan_group_id = scan_group_id, scan_group_name = scan_group_name,
				votes = votes, official = official, url = url
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

			MANGAINFO.ChapterLinks.Add(ch.url)
			MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. scanlator)
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local interceptor = [[
		if (parsed && parsed.result && parsed.result.pages) {
			const res = parsed.result;
			const links = [];
			const pages = res.pages;
			const items = pages.items || (Array.isArray(pages) ? pages : []);
			let base = (pages.baseUrl || '').replace(/\/$/, '');

			for (let i = 0; i < items.length; i++) {
				let item = items[i];
				let url = typeof item === 'string' ? item : item.url;
				if (!url) continue;
				let full = url.startsWith('http') ? url : base + '/' + url.replace(/^\//, '');
				
				let isV3 = (item.s === 1) || full.includes('?v3') || full.includes('&v3');
				let isLegacy = !isV3 && ((i + 1) % 4 === 0);
				
				if (isV3) {
					if (!full.includes('v3')) {
						full += (full.includes('?') ? '&' : '?') + 'v3';
					}
				} else if (isLegacy) {
					full += '#scrambled';
				}
				links.push(full);
			}
			submit({ links: links });
		}
	]]

	local output = MODULE.Storage[URL]
	if output == '' then
		local js_code = GetNodejsScript(interceptor)
		output = require 'utils.nodejs'.run_html_load_with_js(MODULE.RootURL .. URL, js_code)
		MODULE.Storage[URL] = output
	end

	CreateTXQuery(output).XPathStringAll('json(*).links()', TASK.PageLinks)

	return true
end

-- Download, decrypt and/or descramble image given the image URL.
function DownloadImage()
	local is_legacy_scramble = URL:find('#scrambled', 1, true)
	local is_comix = URL:find('comix.to', 1, true)

	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'
	if is_comix or is_legacy_scramble then
		HTTP.Headers.Values['Origin'] = MODULE.RootURL
	end

	if not HTTP.GET(URL) then return false end

	local enc_seed = tonumber(HTTP.Headers.Values['X-Enc-Seed'])
	local enc_len = tonumber(HTTP.Headers.Values['X-Enc-Len'])
	local enc_algo = HTTP.Headers.Values['X-Enc-Algo']

	if enc_seed and enc_seed ~= 0 and enc_len then
		local data = HTTP.Document.ToString()
		local decrypted_data = DecodeEncodedBytes(data, enc_seed, enc_len, enc_algo)
		HTTP.Document.WriteString(decrypted_data)
	end

	local seed = tonumber(HTTP.Headers.Values['X-Scramble-Seed'])
	local scramble_algo = tonumber(HTTP.Headers.Values['X-Scramble-Algo'])
	local raw_scramble_hash = tonumber(HTTP.Headers.Values['X-Scramble-Hash'])

	if seed and seed ~= 0 then
		local scramble_hash = 0
		if raw_scramble_hash == 03632 then scramble_hash = 58414 end
		seed = seed ~ scramble_hash
		local grid_header = HTTP.Headers.Values['X-Scramble-Grid']
		local cols, rows = ParseGrid(grid_header)
		local grid_size = cols * rows

		local puzzle = require 'fmd.imagepuzzle'.Create(cols, rows)
		local matrix
		
		if scramble_algo == 3 then
			matrix = GetPermutationMatrixXorshift(seed, grid_size)
		else
			matrix = GetPermutationMatrixLcg(seed, grid_size)
		end

		for src_idx = 0, grid_size - 1 do
			puzzle.Matrix[src_idx] = matrix[src_idx]
		end

		puzzle.DeScramble(HTTP.Document, HTTP.Document)
	end

	return true
end