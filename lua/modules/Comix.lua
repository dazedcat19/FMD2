----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function GetMangaListFetchScript(page)
	return [[
	const hookCode = `
		window.__comixBrowsePayloads = [];
		(() => {
			const originalParse = JSON.parse;
			JSON.parse = new Proxy(originalParse, {
				apply(target, thisArg, args) {
					const parsed = Reflect.apply(target, thisArg, args);
					try {
						if (parsed && typeof parsed === 'object') {
							const result = parsed.result || parsed;
							if (result && Array.isArray(result.items)) window.__comixBrowsePayloads.push(result);
						}
					} catch (e) {}
					return parsed;
				}
			});
		})();
	`;
	await page.evaluateOnNewDocument(hookCode);
	await page.reload({ waitUntil: 'domcontentloaded', timeout: 90000 });

	const resultJSON = await page.evaluate(async (pageNumber) => {
		return await new Promise((resolve) => {
			let submitted = false;
			let watchdog = null;
			const finish = (data) => {
				if (submitted) return;
				submitted = true;
				if (watchdog) clearTimeout(watchdog);
				clearInterval(poll);
				resolve(data);
			};
			watchdog = setTimeout(() => finish({ error: 'Timed out waiting for browse data' }), 45000);
			const isMangaItems = (items) => items.length > 0 && items.every((item) => item &&
				typeof item.title === 'string' &&
				(typeof item.hid === 'string' || typeof item.url === 'string'));
			const poll = setInterval(() => {
				const payloads = window.__comixBrowsePayloads || [];
				for (const result of payloads) {
					if (!isMangaItems(result.items)) continue;
					const meta = result.meta || {};
					if (typeof meta.page === 'number' && meta.page !== pageNumber) continue;
					const lastPage = (typeof meta.lastPage === 'number' && meta.lastPage) ||
						(typeof meta.last_page === 'number' && meta.last_page) || null;
					finish({
						items: result.items.map((m) => ({
							url: (typeof m.url === 'string' && m.url) || ('/title/' + m.hid),
							title: m.title
						})),
						lastPage: lastPage
					});
					return;
				}
			}, 100);
		});
	}, ]] .. page .. [[);

	console.log(JSON.stringify(resultJSON));
	]]
end

local function GetChapterFetchScript(mid)
	return [[
	const hookCode = `
		window.__comixChapterPages = [];
		(() => {
			const defineProperty = Object.defineProperty;
			const consider = (holder) => setTimeout(() => {
				try {
					if (!holder.meta || !Array.isArray(holder.items)) return;
					window.__comixChapterPages.push({ items: holder.items, meta: holder.meta });
				} catch (e) {}
			});
			defineProperty(Object.prototype, 'items', {
				configurable: true,
				set(value) {
					defineProperty(this, 'items', {
						value: value,
						writable: true,
						enumerable: true,
						configurable: true
					});
					consider(this);
				}
			});
			Object.defineProperty = function (target, key, descriptor) {
				const result = defineProperty.apply(this, arguments);
				if (key === 'items' && descriptor && 'value' in descriptor) consider(target);
				return result;
			};
		})();
	`;
	await page.evaluateOnNewDocument(hookCode);
	await page.reload({ waitUntil: 'domcontentloaded', timeout: 90000 });

	const resultJSON = await page.evaluate(async (mangaId) => {
		let info = null;
		try {
			const initialData = document.querySelector('script#initial-data');
			if (initialData) {
				const queries = JSON.parse(initialData.textContent).queries || {};
				for (const key of Object.keys(queries)) {
					if (key.includes('"detail"')) {
						info = queries[key];
						break;
					}
				}
			}
		} catch (e) {}

		try {
			return await new Promise((resolve) => {
				let settled = false;
				let watchdog = null;
				const finish = (data) => {
					if (settled) return;
					settled = true;
					if (watchdog) clearTimeout(watchdog);
					clearInterval(poll);
					resolve(data);
				};
				const fail = (message) => finish({ error: String(message) });
				const resetWatchdog = () => {
					if (watchdog) clearTimeout(watchdog);
					watchdog = setTimeout(() => fail('Timed out waiting for chapter data'), 90000);
				};

				const chapterPath = '/title/' + mangaId + '-';
				const maxPages = 1000;
				const pagesInFlight = 4;
				const chapters = [];
				const received = new Map();
				const isChapter = (item) => item &&
					typeof item.id === 'number' &&
					typeof item.number === 'number' &&
					typeof item.url === 'string' &&
					item.url.includes(chapterPath);
				let lastPage = null;
				let nextToRead = 1;
				let nextToRequest = 2;
				let done = false;

				const navigateToPage = (page) => {
					const url = new URL(location.href);
					url.searchParams.set('page', String(page));
					history.pushState(history.state, '', url);
					dispatchEvent(new PopStateEvent('popstate', { state: history.state }));
				};

				const advance = () => {
					while (received.has(nextToRead)) {
						const pageData = received.get(nextToRead);
						received.delete(nextToRead);
						chapters.push(...pageData.items);
						const meta = pageData.meta;
						if (!meta.hasNext || nextToRead >= lastPage) {
							done = true;
							const count = new Set(chapters.map((item) => item.id)).size;
							if (count === meta.total) {
								chapters.sort((a, b) => (a.number || 0) - (b.number || 0) || (a.id || 0) - (b.id || 0));
								finish({ items: chapters, info: info });
							} else {
								fail('Got ' + count + ' of ' + meta.total + ' chapters');
							}
							return;
						}
						nextToRead++;
					}
					while (nextToRequest <= lastPage && nextToRequest < nextToRead + pagesInFlight) {
						const page = nextToRequest++;
						setTimeout(() => { try { navigateToPage(page); } catch (e) {} });
					}
				};

				const handle = (holder) => {
					if (done) return;
					const items = holder.items;
					const meta = holder.meta;
					if (!meta || !Array.isArray(items) || !items.every(isChapter)) return;
					if (items.length === 0 && meta.total !== 0) return;
					if (typeof meta.page !== 'number' || typeof meta.lastPage !== 'number') {
						done = true;
						fail('Chapter list has no page numbers');
						return;
					}
					if (meta.page < nextToRead || received.has(meta.page)) return;
					if (lastPage === null) lastPage = Math.min(meta.lastPage, maxPages);
					received.set(meta.page, { items: items, meta: meta });
					resetWatchdog();
					advance();
				};

				const poll = setInterval(() => {
					const queue = window.__comixChapterPages;
					while (queue.length > 0) {
						handle(queue.shift());
						if (settled) return;
					}
				}, 100);
				resetWatchdog();

				navigateToPage(1);
				setTimeout(() => { if (!received.has(1) && !done) navigateToPage(2); }, 4000);
				setTimeout(() => { if (!received.has(1) && !done) navigateToPage(1); }, 8000);
			});
		} catch (e) {
			return { error: e.message || String(e) };
		}
	}, ]] .. string.format('%q', mid) .. [[);

	console.log(JSON.stringify(resultJSON));
	]]
end

local function GetPageFetchScript()
	return [[
	const hookCode = `
		window.__comixPagesHolder = null;
		(() => {
			const defineProperty = Object.defineProperty;
			const consider = (holder) => setTimeout(() => {
				try {
					const items = holder.items;
					if (holder.meta || !Array.isArray(items) || items.length === 0) return;
					if (!items.every((item) => item && typeof item.url === 'string')) return;
					if (window.__comixPagesHolder) return;
					window.__comixPagesHolder = { baseUrl: holder.baseUrl, alt: holder.alt, node: holder.node, items: items };
				} catch (e) {}
			});
			defineProperty(Object.prototype, 'items', {
				configurable: true,
				set(value) {
					defineProperty(this, 'items', {
						value: value,
						writable: true,
						enumerable: true,
						configurable: true
					});
					consider(this);
				}
			});
			Object.defineProperty = function (target, key, descriptor) {
				const result = defineProperty.apply(this, arguments);
				if (key === 'items' && descriptor && 'value' in descriptor) consider(target);
				return result;
			};
		})();
	`;
	await page.evaluateOnNewDocument(hookCode);
	await page.reload({ waitUntil: 'domcontentloaded', timeout: 90000 });

	const resultJSON = await page.evaluate(async () => {
		return await new Promise((resolve) => {
			let submitted = false;
			let watchdog = null;
			const finish = (data) => {
				if (submitted) return;
				submitted = true;
				if (watchdog) clearTimeout(watchdog);
				clearInterval(poll);
				resolve(data);
			};
			watchdog = setTimeout(() => finish({ error: 'Timed out waiting for chapter pages' }), 60000);
			const poll = setInterval(() => {
				const holder = window.__comixPagesHolder;
				if (!holder) return;
				const base = String(holder.baseUrl || '').replace(/\/$/, '');
				const links = [];
				for (let i = 0; i < holder.items.length; i++) {
					const item = holder.items[i];
					let url = item.url.startsWith('http') ? item.url : base + '/' + item.url.replace(/^\//, '');
					const isV3 = (item.s === 1) || url.includes('?v3') || url.includes('&v3');
					const isLegacy = !isV3 && ((i + 1) % 4 === 0);
					if (isV3) {
						if (!url.includes('v3')) {
							url += (url.includes('?') ? '&' : '?') + 'v3';
						}
					} else if (isLegacy) {
						url += '#scrambled';
					}
					links.push(url);
				}
				finish({ links: links });
			}, 100);
		});
	});
	console.log(JSON.stringify(resultJSON));
	]]
end

local function DecodeRunnerOutput(output)
	local json = require 'utils.json'
	local ok, decoded = pcall(json.decode, output)
	if ok then return decoded end
	local candidates = {}
	for line in output:gmatch('[^\r\n]+') do
		table.insert(candidates, line)
	end
	for i = #candidates, 1, -1 do
		local ok2, decoded2 = pcall(json.decode, candidates[i])
		if ok2 and type(decoded2) == 'table' then return decoded2 end
	end
	return nil
end

local function RunBrowserScript(url, js_code)
	local json = require 'utils.json'
	local nodejs = require 'utils.nodejs'
	local output = nodejs.run_html_load_with_js(url, js_code)
	local decoded = DecodeRunnerOutput(output)
	if decoded and decoded.error and decoded.error:find('Execution context was destroyed', 1, true) then
		output = nodejs.run_html_load_with_js(url, js_code)
		decoded = DecodeRunnerOutput(output)
	end
	if decoded then return json.encode(decoded) end
	return output
end

local function GetDirectoryPage(page)
	local json = require 'utils.json'
	local now = os.time()
	local key = 'directory_' .. page
	local output = MODULE.Storage[key]
	local timestamp = tonumber(MODULE.Storage[key .. '_time']) or 0

	if output == '' or (now - timestamp) >= 900 then
		local js_code = GetMangaListFetchScript(page)
		output = RunBrowserScript(MODULE.RootURL .. '/browse?order%5Bcreated_at%5D=desc&page=' .. page, js_code)

		if not json.decode(output).error then
			MODULE.Storage[key] = output
			MODULE.Storage[key .. '_time'] = now
		end
	end

	return output
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
	local json = require 'utils.json'
	local data = json.decode(GetDirectoryPage(1))

	if data.error then
		print('Error: ' .. data.error)
		return net_problem
	end

	PAGENUMBER = tonumber(data.lastPage) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local json = require 'utils.json'
	local data = json.decode(GetDirectoryPage(URL + 1))

	if data.error then
		print('Error: ' .. data.error)
		return net_problem
	end

	for _, v in ipairs(data.items or {}) do
		LINKS.Add(v.url)
		NAMES.Add(v.title)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local json = require 'utils.json'
	local mid = URL:match('/title/([^%-]+)%-')

	local now = os.time()
	local output = MODULE.Storage[mid]
	local timestamp = tonumber(MODULE.Storage[mid .. '_time']) or 0

	if output == '' or (now - timestamp) >= 900 then
		local js_code = GetChapterFetchScript(mid)
		output = RunBrowserScript(MODULE.RootURL .. URL, js_code)

		if not json.decode(output).error then
			MODULE.Storage[mid] = output
			MODULE.Storage[mid .. '_time'] = now
		end
	end

	local data = json.decode(output)
	if data.error then
		MANGAINFO.Title = data.error
		print(data.error)
		return no_error
	end

	local info = data.info
	if not info then return no_error end

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
		local capitalized = info.type:sub(1, 1):upper() .. info.type:sub(2):lower()
		table.insert(genres, capitalized)
	end

	MANGAINFO.Title     = info.title
	MANGAINFO.AltTitles = table.concat(info.altTitles or {}, ', ')
	MANGAINFO.CoverLink = info.poster and info.poster.medium
	MANGAINFO.Authors   = table.concat(authors, ', ')
	MANGAINFO.Artists   = table.concat(artists, ', ')
	MANGAINFO.Genres    = table.concat(genres, ', ')
	MANGAINFO.Status    = MangaInfoStatusIfPos(info.status, 'releasing', 'finished', 'on_hiatus', 'discontinued')
	MANGAINFO.Summary   = info.synopsis

	local deduplicate  = MODULE.GetOption('deduplicatechapters')
	local optgroup     = MODULE.GetOption('showscangroup')
	local chapter_map  = {}
	local chapter_list = {}
	local has_integer  = {}
	local raw_chapters = {}

	for _, v in ipairs(data.items or {}) do
		local number = tostring(v.number)
		local id = v.id
		local name = v.name
		local vol_num = v.volume
		local group = type(v.group) == 'table' and v.group or {}
		local scan_group_id = group.id or 0
		local scan_group_name = group.name
		local votes = v.votes or 0
		local official = v.isOfficial
		local url = v.url

		if not deduplicate then
			local volume = (vol_num ~= 0) and ('Vol. ' .. vol_num .. ' ') or ''
			local chapter = number ~= '' and ('Ch. ' .. number) or ''
			local title = name ~= '' and (' - ' .. name) or ''
			local scanlator = ''
			if optgroup then
				if scan_group_name then
					scanlator = ' [' .. scan_group_name .. ']'
				elseif official then
					scanlator = ' [Official]'
				else
					scanlator = ' [Unknown]'
				end
			end

			MANGAINFO.ChapterLinks.Add(url)
			MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. scanlator)
		else
			table.insert(raw_chapters, {
				id = id, name = name, vol_num = vol_num, number = number,
				scan_group_id = scan_group_id, scan_group_name = scan_group_name,
				votes = votes, official = official, url = url
			})
		end
	end

	if deduplicate then
		for _, ch in ipairs(raw_chapters) do
			if not ch.number:find('%.') then
				has_integer[ch.number] = true
			end
		end

		for _, ch in ipairs(raw_chapters) do
			local base = ch.number:match('^(%d+)')
			local key = (base and has_integer[base]) and base or ch.number
			local current = chapter_map[key]

			if not current then
				chapter_map[key] = ch
				table.insert(chapter_list, key)
			else
				local new_official = ch.official
				local cur_official = current.official
				local new_group = ch.scan_group_id == 10702
				local cur_group = current.scan_group_id == 10702
				local better = false

				if new_official ~= cur_official then
					better = new_official
				elseif new_group ~= cur_group then
					better = new_group
				elseif ch.votes ~= current.votes then
					better = ch.votes > current.votes
				else
					better = ch.id > current.id
				end

				if better then
					chapter_map[key] = ch
				end
			end
		end

		for _, key in ipairs(chapter_list) do
			local ch = chapter_map[key]

			local volume = (ch.vol_num ~= 0) and ('Vol. ' .. ch.vol_num .. ' ') or ''
			local chapter = ch.number ~= '' and ('Ch. ' .. ch.number) or ''
			local title = ch.name ~= '' and (' - ' .. ch.name) or ''
			local scanlator = ''
			if optgroup then
				if ch.scan_group_name then
					scanlator = ' [' .. ch.scan_group_name .. ']'
				elseif ch.official then
					scanlator = ' [Official]'
				else
					scanlator = ' [Unknown]'
				end
			end

			MANGAINFO.ChapterLinks.Add(ch.url)
			MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. scanlator)
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local json = require 'utils.json'
	local output = MODULE.Storage[URL]
	if output == '' then
		local js_code = GetPageFetchScript()
		output = RunBrowserScript(MODULE.RootURL .. URL, js_code)

		if not json.decode(output).error then
			MODULE.Storage[URL] = output
		end
	end

	local ok, decoded = pcall(json.decode, output)
	if not ok or type(decoded) ~= 'table' or decoded.error then
		local msg = (ok and type(decoded) == 'table' and decoded.error) or 'unexpected chapter page response'
		print('Error: ' .. msg)
		return false
	end

	CreateTXQuery(output).XPathStringAll('json(*).links()', TASK.PageLinks)

	return true
end

-- Download, decrypt and/or descramble image given the image URL.
function DownloadImage()
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
		if raw_scramble_hash == 03632 then
			scramble_hash = 58414
		elseif raw_scramble_hash == 02900 then
			scramble_hash = 117532
		end
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
			['deduplicatechapters'] = 'Hapus bab ganda (utamakan bab resmi, diikuti oleh yang paling banyak mendapat suara atau terbaru)'
		}
	}
	local lang = translations[slang] or translations['en']
	m.AddOptionCheckBox('showscangroup', lang.showscangroup, false)
	m.AddOptionCheckBox('deduplicatechapters', lang.deduplicatechapters, false)
end