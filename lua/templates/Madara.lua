----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

XPathTokenAuthors = 'Author(s)'
XPathTokenArtists = 'Artist(s)'
XPathTokenGenres  = 'Genre(s)'
XPathTokenStatus  = 'Status'
ChapterParameters = 'action=manga_get_chapters&manga='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local x = nil
	local s = 'action=madara_load_more&template=madara-core/content/content-archive&page=' .. URL .. '&vars[paged]=0&vars[post_type]=wp-manga&vars[posts_per_page]=250'
	local u = MODULE.RootURL .. '/wp-admin/admin-ajax.php'
	HTTP.Headers.Values['Referer'] = u
	HTTP.MimeType = 'application/x-www-form-urlencoded'

	if not HTTP.POST(u, s) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	if x.XPath('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a').Count == 0 then return no_error end
	x.XPathHREFAll('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local id, s, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="post-title" or @id="manga-title"]/*[self::h1 or self::h3]/text()')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="summary-heading" and ./h5="Alternative" or ./h5="Judul Lain"]/following-sibling::div')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="summary_image"]//img/@data-src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="author-content"]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@class="artist-content"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres-content"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="summary-heading" and contains(., "' .. XPathTokenStatus .. '")]/following-sibling::div'), 'Berjalan|Ongoing|مستمرة|Em Andamento', 'Tamat|Completed|مكتملة|Concluído', 'Hiatus|On Hold|متوقفة|Em espera', 'Diberhentikan|Canceled|مُلغَاة|Cancelado')
	MANGAINFO.Summary   = x.XPathString('string-join(//div[contains(@class, "summary__content") or @class="manga-summary"]|//div[@class="manga-excerpt"]|//div[@class="post-content_item" and contains(h5, "Summary") or contains(h5, "Sinopsis")]//p, "\r\n")')

	if MANGAINFO.CoverLink == '' then MANGAINFO.CoverLink = x.XPathString('//div[@class="summary_image"]//img/@src') end
	if MANGAINFO.Authors == '' then MANGAINFO.Authors = x.XPathString('//div[@class="summary-heading" and contains(., "' .. XPathTokenAuthors .. '")]/following-sibling::div|//div[@class="manga-authors"]/a') end
	if MANGAINFO.Artists == '' then MANGAINFO.Artists = x.XPathString('//div[@class="summary-heading" and contains(., "' .. XPathTokenArtists .. '")]/following-sibling::div|//div[@class="manga-artists"]/a') end
	if MANGAINFO.Genres == '' then MANGAINFO.Genres = x.XPathStringAll('//div[@class="summary-heading" and contains(., "' .. XPathTokenGenres .. '")]/following-sibling::div/a') end

	id = x.XPathString('//div[contains(@id, "manga-chapters-holder")]/@data-id')
	x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	if MANGAINFO.ChapterLinks.Count == 0 then
		HTTP.Reset()
		HTTP.Headers.Values['Content-Length'] = 0
		HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
		if HTTP.POST(MANGAINFO.URL .. 'ajax/chapters') then
			x = CreateTXQuery(HTTP.Document)
			x.XPathHREFAll('//div[@class="li__text"]/a[not(@href="#")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			if MANGAINFO.ChapterLinks.Count == 0 then
				x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			end
		end
	end
	if MANGAINFO.ChapterLinks.Count == 0 then
		HTTP.Reset()
		HTTP.Headers.Values['Cache-Control'] = 'no-cache'
		HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
		HTTP.MimeType = 'application/x-www-form-urlencoded'
		s = ChapterParameters .. id
		if HTTP.POST(MODULE.RootURL .. '/wp-admin/admin-ajax.php', s) then
			CreateTXQuery(HTTP.Document).XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local i, img, script, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if string.find(u, 'style=list', 1, true) == nil then u = string.gsub(u, '?style=paged', '') .. '?style=list' end

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	x.XPathStringAll('//div[contains(@class, "page-break")]/img/@data-src', TASK.PageLinks)
	if TASK.PageLinks.Count == 0 then x.XPathStringAll('//div[contains(@class, "page-break")]/img/@data-src-base64', TASK.PageLinks) end --mangacrab
	if TASK.PageLinks.Count == 0 then x.XPathStringAll('//div[contains(@class, "page-break")]/img/@src', TASK.PageLinks) end
	if TASK.PageLinks.Count == 0 then
		x.ParseHTML('[' .. GetBetween('[', ']', x.XPathString('//script[@id="chapter_preloaded_images"]')) .. ']')
		x.XPathStringAll('json(*)().src', TASK.PageLinks)
	end
	if TASK.PageLinks.Count == 0 then
		x = CreateTXQuery(HTTP.Document)
		script = x.XPathString('//script[@id="chapter-protector-data"]')

		if script == "" and MODULE.Storage["fullpageload"] ~= "" then
			nodejs = require("utils.nodejs")
			result = nodejs.run_html_load(string.gsub(u, "?style=list", ""))

			x.ParseHTML(result)
			script = x.XPathString('//script[@id="chapter-protector-data"]')
		end

		if script ~= "" then
			img = require "fmd.duktape".ExecJS(script .. [[

			var CryptoJS = require("utils/crypto-js.min.js");
			var CryptoJSAesJson = require("utils/cryptojs-aes-format.js");
			JSON.parse(CryptoJS.AES.decrypt(chapter_data, wpmangaprotectornonce, {format: CryptoJSAesJson}).toString(CryptoJS.enc.Utf8));

			]]):gsub('\\/', '/'):gsub('%[', ''):gsub('%]', '')

			for i in img:gmatch('"([^",]+)') do
				TASK.PageLinks.Add(i)
			end
		end
	end
	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub('i%d.wp.com/', ''):gsub('cdn.statically.io/img/', '')
		i = i + 1
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
