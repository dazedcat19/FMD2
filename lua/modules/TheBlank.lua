----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/library?page='
local WorkerFlagMeta = 'm9a49ea12cdb848'
local PageReferer = '/build/assets/BSE14Tw9b.js'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Chapter reader script.
local function GetReaderInitScript()
	return [[
	const hookCode = `
		window.__blankData = { manifests: [], kdfKeys: [] };
		(() => {
			const strip = () => {
				try {
					const m = document.querySelector('meta[name="]] .. WorkerFlagMeta .. [["]');
					if (m) m.remove();
				} catch (e) {}
			};
			try {
				new MutationObserver(strip).observe(document, { childList: true, subtree: true });
			} catch (e) {}
			try { document.addEventListener('readystatechange', strip); } catch (e) {}

			const consider = (parsed) => {
				try {
					if (parsed && typeof parsed === 'object' &&
						typeof parsed.base === 'string' &&
						parsed.base.indexOf('/p/') === 0) {
						window.__blankData.manifests.push(parsed);
					}
				} catch (e) {}
			};
			try {
				const origParse = JSON.parse;
				JSON.parse = new Proxy(origParse, {
					apply(target, thisArg, args) {
						const parsed = Reflect.apply(target, thisArg, args);
						consider(parsed);
						return parsed;
					}
				});
			} catch (e) {}
			try {
				const origJson = Response.prototype.json;
				Response.prototype.json = async function () {
					const value = await origJson.apply(this, arguments);
					consider(value);
					return value;
				};
			} catch (e) {}

			try {
				const origInstantiate = WebAssembly.instantiate.bind(WebAssembly);
				WebAssembly.instantiate = async function (bytes, imports) {
					const result = await origInstantiate(bytes, imports);
					try {
						const instance = result && result.instance ? result.instance : result;
						const origExports = instance.exports;
						const isKdfRot = (name) => name === 'm' || name === '_ip9g3bzelx';
						if (origExports && Object.keys(origExports).some(isKdfRot)) {
							const memory = origExports.e;
							const wrapExports = {};
							for (const key of Object.keys(origExports)) {
								const value = origExports[key];
								wrapExports[key] = (isKdfRot(key) && typeof value === 'function') ?
									function (...args) {
										const ret = value.apply(origExports, args);
										try {
											if (ret) {
												const out = new Uint8Array(memory.buffer, args[5], 32);
												window.__blankData.kdfKeys.push(btoa(String.fromCharCode.apply(null, out)));
											}
										} catch (e) {}
										return ret;
									} : value;
							}
							if (result && result.instance) {
								return { module: result.module, instance: Object.assign({}, instance, { exports: wrapExports }) };
							}
							return wrapExports;
						}
					} catch (e) {}
					return result;
				};
			} catch (e) {}
		})();
	`;
	await page.setViewport({ width: 1366, height: 768 });
	await page.evaluateOnNewDocument(hookCode);
	await page.reload({ waitUntil: 'domcontentloaded', timeout: 90000 });

	const resultJSON = await page.evaluate(async () => {
		return await new Promise((resolve) => {
			let submitted = false;
			let poll = null;
			const finish = (data) => {
				if (submitted) return;
				submitted = true;
				if (poll) clearInterval(poll);
				clearTimeout(watchdog);
				resolve(data);
			};
			const watchdog = setTimeout(() => finish({ error: 'Timed out waiting for the chapter reader to unlock' }), 60000);
			poll = setInterval(() => {
				const data = window.__blankData;
				if (!data || data.manifests.length === 0 || data.kdfKeys.length === 0) return;
				const manifest = data.manifests[data.manifests.length - 1];
				const key = data.kdfKeys[data.kdfKeys.length - 1];
				if (!manifest.base || !key) return;
				finish({
					base: manifest.base,
					pt: manifest['9d2e7b4a1c6f0e83'] || '',
					count: manifest['cad8fbc0b0a1f7a5'] || 0,
					variants: manifest['9c0e92122ffcad77'] || [],
					key: key
				});
			}, 100);
		});
	});
	console.log(JSON.stringify(resultJSON));
	]]
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(//div[@id="app"]/@data-page).props.series.meta.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(//div[@id="app"]/@data-page).props.series.data()').Get() do
		LINKS.Add(v.GetProperty('link').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(//div[@id="app"]/@data-page).props.serie')
	MANGAINFO.Title     = x.XPathString('name', info)
	MANGAINFO.AltTitles = x.XPathString('name_alternative', info)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('cover_image', info))
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Artists   = x.XPathString('artist', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, type?name), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info), 'ongoing', 'finished', 'onhold', 'dropped')
	MANGAINFO.Summary   = x.XPathString('description', info)

	local slug = x.XPathString('slug', info)
	local uid = x.XPathString('uid', info)
	local page = 1
	local pages = nil
	while true do
		u = MODULE.RootURL .. '/api/v1/series/' .. uid .. '/chapters?sort=asc&page=' .. page
		if not HTTP.GET(u) then return net_problem end
		x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('parse-json(.)?items?*[not(type="premium")]').Get() do
			MANGAINFO.ChapterLinks.Add(slug .. '/' .. v.GetProperty('slug').ToString())
			MANGAINFO.ChapterNames.Add(v.GetProperty('title').ToString())
		end
		if not pages then
			pages = tonumber(x.XPathString('json(*).last_page')) or 1
		end
		if page >= pages then break end
		page = page + 1
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local json = require 'utils.json'
	local mslug, cslug = URL:match('^/([^/]+)/([^/]+)$')

	local output = MODULE.Storage[URL]
	if output == '' then
		local js_code = GetReaderInitScript()
		output = require 'utils.nodejs'.run_html_load_with_js(MODULE.RootURL .. '/serie/' .. mslug .. '/chapter/' .. cslug, js_code)
		MODULE.Storage[URL] = output
	end

	local ok, decoded = pcall(json.decode, output)
	if not ok or decoded.error then
		local msg = (ok and decoded.error) or 'unexpected chapter reader response'
		print('Error: ' .. msg)
		return false
	end

	local base = decoded.base
	local pt = decoded.pt
	local key = decoded.key
	local count = tonumber(decoded.count) or 0
	if not base or not pt or not key or count < 1 then
		print('Error: chapter reader data is incomplete')
		return false
	end

	local variants = decoded.variants
	local width = 1080
	if type(variants) == 'table' and tonumber(variants[1]) then
		width = tonumber(variants[1])
	end

	for i = 1, count do
		TASK.PageLinks.Add(string.format('%s%s%d-%d.ece#pt=%s;key=%s', MODULE.RootURL, base, i, width, pt, key))
	end

	return true
end

-- Download and decrypt image given the image URL.
function DownloadImage()
	local crypto = require 'fmd.crypto'

	local pt = URL:match('pt=([^;]+)')
	local key_b64 = URL:match('key=([^;]+)$')

	if not pt or not key_b64 then return false end

	HTTP.Headers.Values['x-9d2e7b'] = pt
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. PageReferer

	if not HTTP.GET(URL) then return false end

	local data = HTTP.Document.ToString()
	if #data < 38 then return false end

	local salt = data:sub(1, 16)
	local record_size = data:byte(17) * 16777216 + data:byte(18) * 65536 + data:byte(19) * 256 + data:byte(20)
	local id_len = data:byte(21)
	local offset = 22 + id_len

	local page_key = crypto.DecodeBase64(key_b64)
	if #page_key ~= 32 then return false end

	local prk = crypto.HMAC_SHA256(page_key, salt)
	local cek = crypto.HMAC_SHA256('Content-Encoding: aes128gcm\0\1', prk):sub(1, 16)
	local nonce = crypto.HMAC_SHA256('Content-Encoding: nonce\0\1', prk):sub(1, 12)

	local nonce_bytes = { nonce:byte(1, 12) }
	local function RecordNonce(sequence)
		local bytes = { nonce_bytes[1], nonce_bytes[2], nonce_bytes[3], nonce_bytes[4], nonce_bytes[5], nonce_bytes[6],
				nonce_bytes[7], nonce_bytes[8], nonce_bytes[9], nonce_bytes[10], nonce_bytes[11], nonce_bytes[12] }
		local r = sequence
		for i = 12, 1, -1 do
			if r <= 0 then break end
			bytes[i] = bytes[i] ~ (r % 256)
			r = math.floor(r / 256)
		end
		return string.char(bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6],
					bytes[7], bytes[8], bytes[9], bytes[10], bytes[11], bytes[12])
	end

	local parts = {}
	local sequence = 0
	while offset <= #data do
		local chunk = data:sub(offset, offset + record_size - 1)
		offset = offset + #chunk
		if #chunk <= 16 then return false end

		local plain = crypto.AESDecryptGCM(chunk, cek, RecordNonce(sequence), '')
		if plain == '' then return false end

		local delimiter = string.byte(plain, #plain)
		if delimiter ~= 1 and delimiter ~= 2 then return false end
		parts[#parts + 1] = plain:sub(1, #plain - 1)

		sequence = sequence + 1
		if #chunk < record_size then break end
	end

	local image = table.concat(parts)
	if #image < 8 then return false end

	HTTP.Document.WriteString(image)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd3cbc04324b349d680855130ed7c4ff3'
	m.Name                     = 'The Blank'
	m.RootURL                  = 'https://theblank.net'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
	m.MaxTaskLimit             = 2
	m.MaxConnectionLimit       = 4
	m.SortedList               = true
end
