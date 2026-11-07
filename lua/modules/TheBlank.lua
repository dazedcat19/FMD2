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
	return [==[
	const hookCode = `
		window.__blankData = { manifests: [], keys: [], workerUrls: [], workerSrc: '' };
		(() => {
			const isFlagMeta = (name) => /^m[0-9a-f]{14}$/.test(name || '');
			const strip = () => {
				try {
					const metas = document.querySelectorAll('meta[name]');
					for (const m of metas) {
						const n = m.getAttribute('name') || '';
						if (isFlagMeta(n) && (m.getAttribute('content') || '') === '1') m.remove();
					}
				} catch (e) {}
			};
			try { new MutationObserver(strip).observe(document, { childList: true, subtree: true }); } catch (e) {}
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
				const opm = Worker.prototype.postMessage;
				Worker.prototype.postMessage = function (msg, transfer) {
					try {
						if (msg && typeof msg === 'object') {
							const ks = Object.keys(msg);
							const urls = [];
							let buf32 = null;
							for (const k of ks) {
								const v = msg[k];
								if (typeof v === 'string' && v.indexOf('/build/assets/') >= 0 && v.indexOf('.js') >= 0) urls.push(v);
								if (v instanceof Uint8Array && v.length === 32) buf32 = v;
								if (v instanceof ArrayBuffer && v.byteLength === 32) buf32 = new Uint8Array(v);
							}
							if (urls.length) window.__blankData.workerUrls.push(urls);
							if (buf32 && ks.length === 2 && Array.isArray(transfer) && transfer.length) {
								window.__blankData.keys.push(btoa(String.fromCharCode.apply(null, buf32)));
							}
						}
					} catch (e) {}
					return opm.apply(this, arguments);
				};
			} catch (e) {}

			const workerInj = \`
				;(function(){
					try {
						const oi = WebAssembly.instantiate.bind(WebAssembly);
						WebAssembly.instantiate = async function (bytes, imports) {
							const result = await oi(bytes, imports);
							try {
								const inst = result && result.instance ? result.instance : result;
								const exp = inst.exports;
								let mem = exp.memory || null;
								if (!mem) for (const k of Object.keys(exp)) { const v = exp[k]; if (v && typeof v === 'object' && v.buffer instanceof ArrayBuffer) { mem = v; break; } }
								if (mem) {
									const wrap = {};
									for (const k of Object.keys(exp)) {
										const v = exp[k];
										wrap[k] = (typeof v === 'function') ? function (...args) {
											const ret = v.apply(exp, args);
											try {
												const ptrs = [];
												if (typeof ret === 'number' && ret > 1023) ptrs.push(ret);
												if (args.length >= 6 && typeof args[5] === 'number' && args[5] > 1023) ptrs.push(args[5]);
												for (const p of ptrs) {
													const out = new Uint8Array(mem.buffer, p, 32);
													self.__blankCaps = self.__blankCaps || [];
													const b64 = btoa(String.fromCharCode.apply(null, out));
													if (self.__blankCaps.indexOf(b64) < 0) self.__blankCaps.push(b64);
												}
											} catch (e) {}
											return ret;
										} : v;
									}
									const wrapped = Object.assign({}, inst, { exports: wrap });
									return result && result.instance ? { module: result.module, instance: wrapped } : wrapped;
								}
							} catch (e) {}
							return result;
						};
					} catch (e) {}
					try {
						const opm = self.postMessage.bind(self);
						self.postMessage = function (msg, transfer) {
							try {
								if (self.__blankCaps && self.__blankCaps.length && msg && typeof msg === 'object' && !msg.__blankCaps) {
									msg.__blankCaps = self.__blankCaps.splice(0);
								}
							} catch (e) {}
							return opm(msg, transfer);
						};
					} catch (e) {}
				})();
			\`;
			try {
				const OrigBlob = window.Blob;
				window.Blob = new Proxy(OrigBlob, {
					construct(target, args) {
						try {
							const parts = args && args[0];
							if (Array.isArray(parts)) {
								const s = parts.filter((p) => typeof p === 'string').join('');
								if (s.length > 4000 && s.indexOf('onmessage') >= 0) {
									window.__blankData.workerSrc = s;
									args[0] = parts.concat([workerInj]);
								}
							}
						} catch (e) {}
						return Reflect.construct(target, args);
					}
				});
			} catch (e) {}

			try {
				const OW = window.Worker;
				window.Worker = new Proxy(OW, {
					construct(target, args) {
						const w = Reflect.construct(target, args);
						try {
							w.addEventListener('message', (ev) => {
								try {
									const d = ev.data;
									if (d && d.__blankCaps && d.__blankCaps.length) {
										for (const c of d.__blankCaps) window.__blankData.keys.push(c);
									}
								} catch (e) {}
							});
						} catch (e) {}
						return w;
					}
				});
			} catch (e) {}

			try {
				const origInstantiate = WebAssembly.instantiate.bind(WebAssembly);
				WebAssembly.instantiate = async function (bytes, imports) {
					const result = await origInstantiate(bytes, imports);
					try {
						const instance = result && result.instance ? result.instance : result;
						const origExports = instance.exports;
						if (origExports && typeof origExports === 'object') {
							let memory = origExports.memory || null;
							if (!memory) {
								for (const k of Object.keys(origExports)) {
									const v = origExports[k];
									if (v && typeof v === 'object' && v.buffer instanceof ArrayBuffer) { memory = v; break; }
								}
							}
							if (memory) {
								const seen = {};
								const wrapExports = {};
								for (const key of Object.keys(origExports)) {
									const value = origExports[key];
									wrapExports[key] = (typeof value === 'function') ? function (...args) {
										const ret = value.apply(origExports, args);
										try {
											if (args.length === 6 && typeof args[5] === 'number' && ret) {
												const out = new Uint8Array(memory.buffer, args[5], 32);
												const b64 = btoa(String.fromCharCode.apply(null, out));
												if (!seen[b64]) { seen[b64] = 1; window.__blankData.keys.push(b64); }
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
			let watchdog = null;
			const finish = (data) => {
				if (submitted) return;
				submitted = true;
				if (poll) clearInterval(poll);
				if (watchdog) clearTimeout(watchdog);
				resolve(data);
			};
			watchdog = setTimeout(() => finish({ error: 'Timed out waiting for the chapter reader to unlock' }), 90000);

			const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
			const b64of = (bytes) => btoa(String.fromCharCode.apply(null, bytes));

			const run = async () => {
				const data = window.__blankData || { manifests: [], keys: [], workerUrls: [], workerSrc: '' };
				const diag = { keyCandidates: 0, keySource: '', refererSource: '', probes: [], notes: [] };

				let manifest = null;
				const t0 = Date.now();
				while (Date.now() - t0 < 45000) {
					if (data.manifests.length) { manifest = data.manifests[data.manifests.length - 1]; break; }
					await sleep(100);
				}
				if (!manifest) {
					finish({ error: 'Timed out waiting for the chapter manifest', diag: diag });
					return;
				}
				diag.manifestKeys = Object.keys(manifest);

				const base = manifest.base;
				const mk = Object.keys(manifest);

				let pt = null;
				if (typeof manifest['9d2e7b4a1c6f0e83'] === 'string') {
					pt = manifest['9d2e7b4a1c6f0e83'];
				} else {
					for (const k of mk) {
						const v = manifest[k];
						if (k !== 'base' && typeof v === 'string' && /^[A-Za-z0-9_-]{32}$/.test(v)) { pt = v; break; }
					}
				}
				if (!pt) { finish({ error: 'Manifest is missing the ticket value', diag: diag }); return; }

				let width = 1080;
				for (const k of mk) {
					const v = manifest[k];
					if (typeof v === 'string' && v.indexOf(base) === 0) {
						const m = v.slice(base.length).match(/^0-(\d+)\.ece$/);
						if (m) { width = parseInt(m[1], 10); break; }
					}
				}

				const parts = base.split('/').filter(Boolean);
				const uid = parts[1] || '';
				const kv = Number(parts[2]) || 1;

				const countCandidates = [];
				for (const k of mk) {
					const v = manifest[k];
					if (typeof v === 'number' && Number.isInteger(v) && v >= 1 && v <= 500) countCandidates.push(v);
				}

				const t1 = Date.now();
				while (Date.now() - t1 < 15000 && data.keys.length === 0) await sleep(100);
				diag.keyCandidates = data.keys.length;

				const resSet = new Set();
				try { for (const r of performance.getEntriesByType('resource')) resSet.add(r.name); } catch (e) {}
				const inRes = (u) => { try { return resSet.has(new URL(u).href); } catch (e) { return false; } };
				let refCandidates = [];
				let wasmUrl = '';
				if (data.workerUrls.length) {
					const uniq = [...new Set(data.workerUrls[data.workerUrls.length - 1])];
					if (uniq.length >= 2) {
						const aRes = inRes(uniq[0]);
						const bRes = inRes(uniq[1]);
						if (aRes && !bRes) { refCandidates = [uniq[0], uniq[1]]; wasmUrl = uniq[1]; }
						else if (bRes && !aRes) { refCandidates = [uniq[1], uniq[0]]; wasmUrl = uniq[0]; }
						else { refCandidates = [uniq[0], uniq[1]]; wasmUrl = uniq[0]; diag.notes.push('referer ambiguous'); }
					} else if (uniq.length === 1) {
						refCandidates = [uniq[0]];
					}
				}
				refCandidates.push('');

				const pagePath = (n) => base + n + '-' + width + '.ece';
				let page1 = null;
				let refererUsed = '';
				const refTried = [];
				for (const ref of refCandidates) {
					try {
						const opt = { credentials: 'omit', headers: { 'x-9d2e7b': pt } };
						if (ref) opt.referrer = ref; else opt.referrerPolicy = 'no-referrer';
						const r = await fetch(new URL(pagePath(1), location.origin).href, opt);
						refTried.push([ref || '(none)', r.status]);
						if (r.ok) {
							page1 = new Uint8Array(await r.arrayBuffer());
							refererUsed = ref;
							diag.refererSource = ref ? 'captured' : 'none';
							break;
						}
					} catch (e) {
						refTried.push([ref || '(none)', -1]);
					}
				}
				diag.refTried = refTried;
				if (!page1) { finish({ error: 'Could not download page 1 for verification', diag: diag }); return; }

				const verifyKey = async (keyB64) => {
					try {
						const keyBytes = Uint8Array.from(atob(keyB64), (c) => c.charCodeAt(0));
						if (keyBytes.length !== 32) return null;
						const d = page1;
						if (d.length < 21) return null;
						const salt = d.slice(0, 16);
						const enc = new TextEncoder();
						const hmac = async (keyArg, msgBytes) => {
							const k = await crypto.subtle.importKey('raw', keyArg, { name: 'HMAC', hash: 'SHA-256' }, false, ['sign']);
							return await crypto.subtle.sign('HMAC', k, msgBytes);
						};
						const withCounter = (label) => {
							const i = enc.encode(label);
							const o = new Uint8Array(i.length + 1);
							o.set(i); o[i.length] = 1;
							return o;
						};
						const prk = await hmac(salt, keyBytes);
						const cekBuf = await hmac(new Uint8Array(prk), withCounter('Content-Encoding: aes128gcm\u0000'));
						const nonceBuf = await hmac(new Uint8Array(prk), withCounter('Content-Encoding: nonce\u0000'));
						const cek = await crypto.subtle.importKey('raw', new Uint8Array(cekBuf, 0, 16), 'AES-GCM', false, ['decrypt']);
						const rs = (d[16] << 24) | (d[17] << 16) | (d[18] << 8) | d[19];
						const idlen = d[20];
						const start = 21 + idlen;
						const rec = d.slice(start, Math.min(start + rs, d.length));
						if (rec.length < 18) return null;
						const plain = new Uint8Array(await crypto.subtle.decrypt({ name: 'AES-GCM', iv: new Uint8Array(nonceBuf, 0, 12), tagLength: 128 }, cek, rec));
						const delim = plain[plain.length - 1];
						if (delim !== 1 && delim !== 2) return null;
						return plain.slice(0, plain.length - 1);
					} catch (e) { return null; }
				};
				const looksLikeImage = (b) => {
					if (!b || b.length < 12) return false;
					if (b[0] === 0xFF && b[1] === 0xD8 && b[2] === 0xFF) return true;
					if (b[0] === 0x89 && b[1] === 0x50 && b[2] === 0x4E && b[3] === 0x47) return true;
					if (b[0] === 0x47 && b[1] === 0x49 && b[2] === 0x46) return true;
					if (b[0] === 0x52 && b[1] === 0x49 && b[2] === 0x46 && b[3] === 0x46 && b[8] === 0x57 && b[9] === 0x45 && b[10] === 0x42 && b[11] === 0x50) return true;
					if (b[4] === 0x66 && b[5] === 0x74 && b[6] === 0x79 && b[7] === 0x70) return true;
					return false;
				};

				const candidates = [...new Set(data.keys)];
				let verifiedKey = null;
				for (const cand of candidates) {
					const img = await verifyKey(cand);
					if (img && looksLikeImage(img)) {
						verifiedKey = cand;
						diag.keySource = 'captured';
						break;
					}
				}

				diag.keyCandidates = [...new Set(candidates)].length;
				if (!verifiedKey) { finish({ error: 'No verified page key found', diag: diag }); return; }

				let count = 0;
				const uniqCands = [...new Set(countCandidates)].sort((a, b) => a - b);
				for (const N of uniqCands) {
					try {
						const opt = { credentials: 'omit', headers: { 'x-9d2e7b': pt } };
						if (refererUsed) opt.referrer = refererUsed; else opt.referrerPolicy = 'no-referrer';
						const s1 = (await fetch(new URL(pagePath(N), location.origin).href, opt)).status;
						if (s1 !== 200 && s1 !== 206) { diag.probes.push([N, s1, '-']); continue; }
						const s2 = (await fetch(new URL(pagePath(N + 1), location.origin).href, opt)).status;
						diag.probes.push([N, s1, s2]);
						if (s2 !== 200 && s2 !== 206) { count = N; break; }
					} catch (e) {
						diag.probes.push([N, -1, '-']);
					}
				}
				if (!count) { finish({ error: 'Could not determine the page count', diag: diag }); return; }

				finish({
					base: base,
					pt: pt,
					key: verifiedKey,
					count: count,
					width: width,
					referer: refererUsed,
					header: 'x-9d2e7b',
					diag: diag
				});
			};
			run().catch((e) => finish({ error: String(e).slice(0, 200) }));
		});
	});
	console.log(JSON.stringify(resultJSON));
	]==]
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
	local rev = tonumber(require 'fmd.env'.Revision)
	if rev < 6948 then
		print('Require FMD2 revision 6948 and later')
		print('Revision ' .. rev .. ' detected')
		return false
	end
	local json = require 'utils.json'
	local mslug, cslug = URL:match('^/([^/]+)/([^/]+)$')

	local output = MODULE.Storage[URL]
	if output == '' then
		local js_code = GetReaderInitScript()
		output = require 'utils.nodejs'.run_html_load_with_js(MODULE.RootURL .. '/serie/' .. mslug .. '/chapter/' .. cslug, js_code, { 
			webgl = true
		})
	end

	local ok, decoded = pcall(json.decode, output)
	if not ok or decoded.error then
		local msg = (ok and decoded.error) or 'unexpected chapter reader response'
		local diag = (ok and decoded.diag) or nil
		print('Error: ' .. msg)
		if diag then print('Diag: ' .. json.encode(diag)) end
		return false
	end
	MODULE.Storage[URL] = output

	local base = decoded.base
	local pt = decoded.pt
	local key = decoded.key
	local count = tonumber(decoded.count) or 0
	local width = tonumber(decoded.width) or 1080
	local referer = decoded.referer or ''
	local header = decoded.header or 'x-9d2e7b'
	if base == nil or pt == nil or key == nil or count < 1 then
		print('Error: chapter reader data is incomplete')
		return false
	end

	for i = 1, count do
		local frag = 'pt=' .. pt .. ';key=' .. key
		if referer ~= '' then frag = frag .. ';ref=' .. referer end
		frag = frag .. ';hdr=' .. header
		TASK.PageLinks.Add(string.format('%s%s%d-%d.ece#%s', MODULE.RootURL, base, i, width, frag))
	end

	return true
end

-- Download and decrypt image given the image URL.
function DownloadImage()
	local crypto = require 'fmd.crypto'
	local pt = URL:match('pt=([^;]+)')
	local key_b64 = URL:match('key=([^;]+)')
	local referer = URL:match('ref=([^;]+)')
	local header = URL:match('hdr=([^;]+)')

	if not pt or not key_b64 then return false end

	HTTP.Headers.Values[header or 'x-9d2e7b'] = pt
	if referer and referer ~= '' then
		HTTP.Headers.Values['Referer'] = referer
	end

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
