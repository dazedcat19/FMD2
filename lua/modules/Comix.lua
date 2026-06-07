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

local function GetPermutationMatrix(seed, n)
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

local function DecodeEncodedPrefix(data, seed, length)
	local ENC_MULTIPLIER = 1000005
	local ENC_INCREMENT = 1234567891
	local state = seed
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
	local u = API_URL .. '/manga/' .. mid

	if not HTTP.GET(u) then return net_problem end

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

			for (let item of items) {
				let url = typeof item === 'string' ? item : item.url;
				if (!url) continue;
				links.push(url.startsWith('http') ? url : base + '/' + url.replace(/^\//, ''));
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
	if not HTTP.GET(URL) then return false end

	if HTTP.ResultCode == 404 then
		HTTP.Reset()
		HTTP.Headers.Values['Origin'] = MODULE.RootURL
		if not HTTP.GET(URL) then return false end
	end

	local enc_seed = tonumber(HTTP.Headers.Values['X-Enc-Seed'])
	local enc_len = tonumber(HTTP.Headers.Values['X-Enc-Len'])

	if enc_seed and enc_seed ~= 0 and enc_len then
		local data = HTTP.Document.ToString()
		local decrypted_data = DecodeEncodedPrefix(data, enc_seed, enc_len)
		HTTP.Document.WriteString(decrypted_data)
	end

	local seed = tonumber(HTTP.Headers.Values['X-Scramble-Seed'])

	if seed and seed ~= 0 then
		local grid_header = HTTP.Headers.Values['X-Scramble-Grid']
		local cols, rows = ParseGrid(grid_header)
		local grid_size = cols * rows

		local puzzle = require 'fmd.imagepuzzle'.Create(cols, rows)
		local matrix = GetPermutationMatrix(seed, grid_size)

		for src_idx = 0, grid_size - 1 do
			puzzle.Matrix[src_idx] = matrix[src_idx]
		end

		puzzle.DeScramble(HTTP.Document, HTTP.Document)
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end