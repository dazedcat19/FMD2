----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '1a7b98800a114a3da5f48de91f45a880'
	m.Name                     = 'ReadComicOnline'
	m.RootURL                  = 'https://rcostation.xyz'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['datasaver'] = 'Data saver'
		},
		['id_ID'] = {
			['datasaver'] = 'Penghemat data'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('datasaver', lang.datasaver, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/ComicList/Newest'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pager"]/li[last()]/a/@href'):match('=(%d+)$')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. '?page=' .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@class="list-comic"]/div/a[1]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('span', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//a[@class="bigChar"]')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//link[@rel="image_src"]/@href'))
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="barContent"]//p[span=("Author:","Writer:")]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@class="barContent"]//p[span="Artist:"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="barContent"]//p[span="Genres:"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos((x.XPathString('//div[@class="barContent"]/div/p[span="Status:"]')))
	MANGAINFO.Summary   = x.XPathString('//div[@class="barContent"]/div/p[starts-with(.,"Summary:")]//following-sibling::p[1]')

	x.XPathHREFAll('//table[@class="listing"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local json = require 'utils.json'
	local duktape = require 'fmd.duktape'
	local quality = MODULE.GetOption('datasaver') and 'lq' or 'hq'
	local u = MaybeFillHost(MODULE.RootURL, URL .. '&quality=' .. quality .. '&readType=1')

	if not HTTP.GET(u) then return false end

	local body = HTTP.Document.ToString()
	local combined_scripts = ''
	for s in body:gmatch('<script[^>]*>(.-)</script>') do
		combined_scripts = combined_scripts .. s .. '\n'
	end

	local decrypt_logic = [=[
		var pageLinks = [];
		var urlPattern = /^https?:\/\/(?:www\.)?[a-z0-9-]+(?:\.[a-z0-9-]+)+\b(?:[\/a-z0-9-._~:?#@!$&'()*+,;=%]*)$/i;
		var reverseOrder = false;
		var replacePatternRegex = /\.replace\(\s*\/(\w+__\w+_)\/g\s*,\s*(?:['"](\w)['"]|(\w+))\s*\)/;
		var replaceMatch = _encryptedString.match(replacePatternRegex);
		var obfuscationPattern = /\w{2}__\w{6}_/g;
		var replacementChar = "e";

		if (replaceMatch) {
			obfuscationPattern = new RegExp(replaceMatch[1], "g");
			if (replaceMatch[2]) {
				replacementChar = replaceMatch[2];
			} else {
				var t_var = replaceMatch[3];
				var e_regex = new RegExp(t_var + "\\s*=\\s*(?:['\"](.*?)['\"]\\s*\\+\\s*)?['\"](.)['\"]", "g");
				var sMatches = [];
				var sMatch;
				while ((sMatch = e_regex.exec(_encryptedString)) !== null) {
					sMatches.push(sMatch);
				}
				if (sMatches.length > 0) {
					replacementChar = sMatches[sMatches.length - 1][2];
				}
			}
		}

		var arrayVars = [];
		var varRegex = /var\s+(\w+)\s*=\s*new\s+Array\(\)\s*;/g;
		var varMatch;
		while ((varMatch = varRegex.exec(_encryptedString)) !== null) {
			arrayVars.push(varMatch[1]);
		}

		var baseUrlMatch = _encryptedString.match(/baeu\(\w+,\s*["'](https?:\/\/[^"']+)["']\)/);
		var detectedBaseUrl = baseUrlMatch ? baseUrlMatch[1] : null;

		for (var i = 0; i < arrayVars.length; i++) {
			var t = arrayVars[i];
			var r = [];

			var pushRegex = new RegExp(t + "\\.push\\(['\"]([^'\"]{20,})['\"]\\)", "g");
			var pMatch;
			while ((pMatch = pushRegex.exec(_encryptedString)) !== null) {
				r.push(pMatch[1]);
			}

			if (r.length === 0) {
				var e = new RegExp("\\w+\\s*\\([^)]*\\b" + t + "\\b[^)]*\\)", "g");
				var sMatches2 = [];
				var sMatch2;
				while ((sMatch2 = e.exec(_encryptedString)) !== null) {
					sMatches2.push(sMatch2);
				}

				if (sMatches2.length > 0) {
					for (var j = 0; j < sMatches2.length; j++) {
						var fullMatch = sMatches2[j][0];
						var innerRegex = /["']([^"']{20,})["']/g;
						var eMatches = [];
						var eMatch;
						while ((eMatch = innerRegex.exec(fullMatch)) !== null) {
							eMatches.push(eMatch[1]);
						}
						eMatches.sort(function(a, b) { return b.length - a.length; });
						if (eMatches.length > 0 && eMatches[0]) {
							r.push(eMatches[0]);
						}
					}
				}
			}

			if (r.length === 0) continue;

			var n = findPrefixOffset(r);
			for (var k = 0; k < r.length; k++) {
				pageLinks.push(decryptLink(r[k], n));
			}
		}

		function findPrefixOffset(t) {
			if (t.length === 0) return 0;
			var e = t[0];
			var s = 0;
			for (var r = 0; r < e.length; r++) {
				var n = e.charAt(r);
				var allMatch = true;
				for (var m = 0; m < t.length; m++) {
					if (t[m].charAt(r) !== n) {
						allMatch = false;
						break;
					}
				}
				if (allMatch) {
					s++;
					if (s >= 5 && e.slice(s - 5, s) === "https") return s - 5;
				} else {
					break;
				}
			}
			return s;
		}

		function atob(t) {
			var e = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
			var s = String(t).replace(/=+$/, "");
			if (s.length % 4 === 1) throw new Error("'atob' failed: The string to be decoded is not correctly encoded.");
			var r = "";
			for (var t_idx = 0, n, c, o = 0; c = s.charAt(o++); ~c && (n = t_idx % 4 ? n * 64 + c : c, t_idx++ % 4) ? r += String.fromCharCode(255 & n >> (-2 * t_idx & 6)) : 0) c = e.indexOf(c);
			return r;
		}

		function endsWith(str, suffix) {
			return str.indexOf(suffix, str.length - suffix.length) !== -1;
		}

		function startsWith(str, prefix) {
			return str.indexOf(prefix) === 0;
		}

		function decryptLink(t, e) {
			e = e || 0;
			var s = t.replace(obfuscationPattern, replacementChar).replace(/pw_.g28x/g, "b").replace(/d2pr.x_27/g, "h");
			if (e != 0) s = s.substr(e, s.length - e);
			
			if (endsWith(s, "=s0") || endsWith(s, "=s1600")) {
				s = s.replace("https://2.bp.blogspot.com/", "") + "?";
			}
			
			if (!startsWith(s, "https")) {
				var t_idx = s.indexOf("?");
				var e_str = s.substring(t_idx);
				var r = s.indexOf("=s0?") !== -1;
				var n = r ? s.indexOf("=s0?") : s.indexOf("=s1600?");
				var c = s.substring(0, n);
				c = c.substring(15, 33) + c.substring(50);
				var o = c.length;
				c = c.substring(0, o - 11) + c.charAt(o - 2) + c.charAt(o - 1);
				var a = atob(c);
				var l = decodeURIComponent(a);
				l = l.substring(0, 13) + l.substring(17);
				l = l.substring(0, l.length - 2) + (r ? "=s0" : "=s1600");
				var i = detectedBaseUrl ? detectedBaseUrl : (_useServer2 ? "https://ano1.rconet.biz/pic" : "https://2.bp.blogspot.com");
				s = i + "/" + l + e_str + (_useServer2 ? "&t=10" : "");
			}
			return s;
		}

		var blocklist = ["https://2.bp.blogspot.com/pw/AP1GczP6zCVVfdmN6OoVnm7CLvEfmHMUawyEwJWouX9C6SHwsiuYfLkUr9FsM6Zo34qNzPKeQeahBx9ckBZJQckiJmX1UwKD7uh900yz5rKyG4zT2rfIrqFviEJIev1Pg_pGRuSG57rIH6BDwGCTmiE4MjA", "https://2.bp.blogspot.com/pw/AP1GczP48thKMga7cud0tjtHtYqsvZzhYY0HyAxVzM3O1D6tkLbi0fT9NDZFFFH69hNnoGsnqJSEIh4mmpEoU1BJSfNXIz1f5aLXl41RM9os7ePn7ipbrYbIuqiQxAV0hhJZrNLl7FmauwLQ01paCrP6KAE", "https://2.bp.blogspot.com/pw/AP1GczNXprTMfAP2AHFFWvCbKq6qReXrqSohz87KeBjV0nh6XoLsE1NpzL7Rp9llxoY208IPARiIDON_TO6dZB0ZMNeB8J7xzUzbS9h6To7aGpOZshFofw-wFQ0KJ3y3wolSwzLrduZZ_0w8_6gGuTEB-98", "https://2.bp.blogspot.com/pw/AP1GczMVY_zWeag2n981CRX7jaZ73Sr0NtidtJhnvJ3-Rmh2fIo-PoQRI0ZksQEbpTjDHgBeNYbQ2hQodsY-Dv0FXUhiU_mus5z5L5lMVAH82kXYqOd2IEw", "https://2.bp.blogspot.com/pw/AP1GczOKY-6EDGVvlQGB2wj0xxB5JgcyiujFJC3CHgwqBOLIidwmoP6DLiMpX__Fw6MMPvLezN6soeV0A8pKSHUrC4rxZyO5vov40g1g4ipZdkFlzUouAFA", "https://2.bp.blogspot.com/pw/AP1GczO8AETT3k19nhJwxHm0sHCSy0tXyhSOYxnq3EUrmlvgY5yPqDaxcd1XZ7reQKH-lKgpGK4o3sW_9Yu6feqii79riXN3Ghi8Xs1S5Z4wi-aeHrq5PzOX"];

		function getCleanedLinks() {
			var filtered = [];
			for (var i = 0; i < pageLinks.length; i++) {
				var t = pageLinks[i];
				if (!t) continue;
				if (t.indexOf("blank.gif") !== -1) continue;
				var s = t.split("?")[0].split("=")[0];
				
				var isFirst = true;
				for (var j = 0; j < i; j++) {
					if (pageLinks[j] && pageLinks[j].split("?")[0].split("=")[0] === s) {
						isFirst = false;
						break;
					}
				}
				
				var n = blocklist.indexOf(s) === -1;
				var c = urlPattern.test(s);
				
				if (isFirst && n && c) {
					filtered.push(t);
				}
			}
			return reverseOrder ? filtered.reverse() : filtered;
		}

		JSON.stringify(getCleanedLinks());
		]=]

	local js = 'var _encryptedString = ' .. json.encode(combined_scripts) .. ';\n' .. 'var _useServer2 = false;\n' .. decrypt_logic

	local result = duktape.ExecJS(js)
	local links = json.decode(result)
	for i = 1, #links do
		TASK.PageLinks.Add(links[i])
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end