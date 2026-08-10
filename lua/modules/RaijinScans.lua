----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/page/'
local DirectoryParameters = '/?s&post_type=wp-manga&sort=new-manga'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1 .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()]/a/@href'):match('/(%d+)/')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="info"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="serie-title"]')
	MANGAINFO.AltTitles = x.XPathString('//h6[@class="alternative-title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="main-cover"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[span="Auteur"]/span[2]')
	MANGAINFO.Artists   = x.XPathStringAll('//div[span="Artiste"]/span[2]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genre-list"]//div')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[span="État du titre"]/span[2]'), 'En cours', 'Terminé', 'En pause', 'Annulé')
	MANGAINFO.Summary   = x.XPathString('//div[@id="synopsis"]//string-join(p, "\r\n")')

	x.XPathHREFTitleAll('//ul[@class="scroll-sm"]/li[@class="item"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	local node = require 'utils.nodejs'

	local js_code = [=[
		const result = await page.evaluate(async () => {
			try {
				var manifestScript = null;
				var scripts = document.querySelectorAll("script");
				for (var i = 0; i < scripts.length; i++) {
					var t = scripts[i].textContent || "";
					if (t.indexOf("rjfr_") >= 0) { manifestScript = t; break; }
				}

				if (!manifestScript) return { error: "No reader manifest found." };

				var mKey = manifestScript.search(/"m"\s*:/);
				if (mKey < 0) return { error: "Invalid manifest format" };

				var start = manifestScript.lastIndexOf("{", mKey);
				if (start < 0) return { error: "Invalid manifest format" };

				function extractObject(s, start) {
					var depth = 0, inStr = false, esc = false;
					for (var i = start; i < s.length; i++) {
						var ch = s[i];
						if (inStr) {
							if (esc) esc = false;
							else if (ch === '\\') esc = true;
							else if (ch === '"') inStr = false;
						} else if (ch === '"') inStr = true;
						else if (ch === '{') depth++;
						else if (ch === '}') { 
							depth--; 
							if (depth === 0) return s.slice(start, i + 1); 
						}
					}
					throw new Error("Unterminated manifest object");
				}

				var manifest = JSON.parse(extractObject(manifestScript, start));
				var mOrder = manifest.m.split("|");
				var cObj = manifest.c;
				var b64 = mOrder.map(function (k) { return cObj[k]; }).join("");

				function decodeBase64(s) {
					s = s.replace(/=+$/, "");
					while (s.length % 4) s += "=";
					return decodeURIComponent(escape(atob(s)));
				}

				var config = JSON.parse(decodeBase64(b64));
				var shuffled = config.d;
				var perm = config.m;
				var order = config.l;
				var ordered = new Array(shuffled.length);

				perm.forEach(function (p, i) { ordered[p] = shuffled[i]; });
				var vals = order.map(function (o) { return ordered[o]; });

				var action = vals.filter(function (v) { return typeof v === "string" && v.indexOf("rjfr_") === 0; })[0];
				var keyArr = vals[13];
				var contentValues = [vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]];

				var rootEl = document.querySelector("[data-rj-free-reader-root]");
				var rjfrValue = rootEl ? rootEl.getAttribute("data-rj-free-reader-root") : "";

				var pages = [];
				var cursor = "";
				var run = true;
				var guard = 0;
				var baseUrl = window.location.origin;

				function imageUrlOrNull(obj) {
					if (!obj || typeof obj !== "object" || Array.isArray(obj)) return null;
					var keys = Object.keys(obj);
					for (var i = 0; i < keys.length; i++) {
						var v = obj[keys[i]];
						if (typeof v === "string" && v.indexOf("http") === 0 && /\.(webp|jpe?g|png|gif|avif)/i.test(v)) return v;
					}
					return null;
				}
				
				function isImageArray(arr) {
					return Array.isArray(arr) && arr.length > 0 && imageUrlOrNull(arr[0]) !== null;
				}

				function findImages(el) {
					if (el && typeof el === "object" && !Array.isArray(el)) {
						var keys = Object.keys(el);
						for (var i = 0; i < keys.length; i++) {
							if (isImageArray(el[keys[i]])) return { payload: el, images: el[keys[i]] };
						}
						for (var j = 0; j < keys.length; j++) {
							var r = findImages(el[keys[j]]);
							if (r) return r;
						}
					} else if (Array.isArray(el)) {
						for (var k = 0; k < el.length; k++) {
							var r2 = findImages(el[k]);
							if (r2) return r2;
						}
					}
					return null;
				}

				while (run && guard++ < 100) {
					var formData = new FormData();
					formData.append("action", action);
					contentValues.forEach(function (v, j) { formData.append(keyArr[j], String(v)); });
					formData.append(keyArr[6], String(pages.length));
					formData.append(keyArr[7], "0");
					formData.append(keyArr[8], rjfrValue);
					formData.append(keyArr[9], cursor);

					var resp = await fetch(baseUrl + "/wp-admin/admin-ajax.php", {
						method: "POST",
						body: formData,
						headers: { "X-Requested-With": "XMLHttpRequest" }
					});
					
					if (!resp.ok) throw new Error("Fetch failed: " + resp.status);

					var root = await resp.json();
					var found = findImages(root);
					if (!found) throw new Error("Images not found in payload");

					found.images.forEach(function (img) {
						var u = imageUrlOrNull(img);
						if (u) pages.push(u);
					});

					var pv = Object.keys(found.payload).map(function (k) { return found.payload[k]; });
					cursor = pv.filter(function (x) { return typeof x === "string" && /^\d+\.\d+\.[0-9a-f]{64}$/.test(x); })[0] || "";
					run = cursor !== "" && pv.filter(function (x) { return typeof x === "boolean"; })[0] === true;
				}

				return { pages: pages };
			} catch (e) {
				return { error: e.toString() };
			}
		});

		console.log(JSON.stringify(result));
	]=]

	local out = node.run_html_load_with_js(u, js_code)

	if out ~= '' then
		local parsed = require 'utils.json'.decode(out)
		if parsed.pages then
			for _, p in ipairs(parsed.pages) do
				TASK.PageLinks.Add(p)
			end
		elseif parsed.error then
			print(parsed.error)
		end
	end

	return false
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
    local m = NewWebsiteModule()
    m.ID                       = '5a09b38e0c144b689e3d9c7eb7ebcd'
    m.Name                     = 'Raijin Scans'
    m.RootURL                  = 'https://raijin-scans.fr'
    m.Category                 = 'French'
    m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
    m.OnGetNameAndLink         = 'GetNameAndLink'
    m.OnGetInfo                = 'GetInfo'
    m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end