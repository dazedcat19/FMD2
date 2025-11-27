----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

ChapterParameters   = 'action=manga_get_chapters&manga='
DirectoryPagination = '/page/'
DirectoryParameters = '/?s&post_type=wp-manga&m_orderby=new-manga'
XPathTokenAuthors   = 'Author(s)'
XPathTokenArtists   = 'Artist(s)'
XPathTokenGenres    = 'Genre(s)'
XPathTokenStatus    = 'Status'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1 .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="wp-pagenavi"]/(a[@class="last" or @class="page-numbers"])[last()]/@href'):match('/(%d+)/')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLinkWithPagination()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[contains(@class, "post-title")]/*[self::h3 or self::h2]/a', LINKS, NAMES)

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local s = 'action=madara_load_more&template=madara-core/content/content-archive&page=' .. URL .. '&vars[paged]=0&vars[post_type]=wp-manga&vars[posts_per_page]=250'
	local u = MODULE.RootURL .. '/wp-admin/admin-ajax.php'
	HTTP.Headers.Values['Referer'] = u
	HTTP.MimeType = 'application/x-www-form-urlencoded'

	if not HTTP.POST(u, s) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPathCount('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a') == 0 then return no_error end
	x.XPathHREFAll('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="post-title" or @id="manga-title"]/*[self::h1 or self::h3]/text()')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="summary-heading" and contains(./h5, "Alternativ") or contains(./h5, "Judul Lain") or contains(./h5, "أسم بديل")]/following-sibling::div')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="summary_image"]//img/@data-src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="author-content"]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@class="artist-content"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres-content"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="summary-heading" and contains(., "' .. XPathTokenStatus .. '")]/following-sibling::div'), 'Berjalan|Ongoing|Releasing|مستمرة|Em Andamento|En curso', 'Tamat|Completed|مكتملة|Concluído|Terminado', 'Hiatus|On Hold|متوقفة|Em espera|En espera', 'Diberhentikan|Canceled|مُلغَاة|Cancelado')
	MANGAINFO.Summary   = x.XPathString('string-join((//div[contains(@class, "summary__content") or @class="manga-summary"])[1]|//div[@class="manga-excerpt"]|//div[@class="post-content_item" and contains(h5, "Summary") or contains(h5, "Sinopsis")]//p, "\r\n")')

	if MANGAINFO.CoverLink == '' then MANGAINFO.CoverLink = x.XPathString('//div[@class="summary_image"]//img/@src') end
	if MANGAINFO.Authors == '' then MANGAINFO.Authors = x.XPathString('//div[@class="summary-heading" and contains(., "' .. XPathTokenAuthors .. '")]/following-sibling::div|//div[@class="manga-authors"]/a') end
	if MANGAINFO.Artists == '' then MANGAINFO.Artists = x.XPathString('//div[@class="summary-heading" and contains(., "' .. XPathTokenArtists .. '")]/following-sibling::div|//div[@class="manga-artists"]/a') end
	if MANGAINFO.Genres == '' then MANGAINFO.Genres = x.XPathStringAll('//div[@class="summary-heading" and contains(., "' .. XPathTokenGenres .. '")]/following-sibling::div/a') end

	local id = x.XPathString('//div[contains(@id, "manga-chapters-holder")]/@data-id')

	x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#") and not(@class="reward_ads")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	if MANGAINFO.ChapterLinks.Count == 0 then
		local p = 1
		while true do
			HTTP.Reset()
			HTTP.Headers.Values['Content-Length'] = 0
			HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
			if not HTTP.POST(MANGAINFO.URL .. 'ajax/chapters/?t=' .. p) then return net_problem end
			local x = CreateTXQuery(HTTP.Document)
			local first_link = x.XPathString('(//li[contains(@class,"wp-manga-chapter")]/a[not(@href="#") and not(@class="reward_ads")])[1]/@href')
			if first_link == '' then break end
			if MANGAINFO.ChapterLinks.Count > 0 and MANGAINFO.ChapterLinks[0] == first_link then break end
			local s = x.XPathString('(//div[@class="li__text"]/a[not(@href="#") and not(@class="reward_ads")])[1]')
			if s ~= '' then
				x.XPathHREFAll('//div[@class="li__text"]/a[not(@href="#") and not(@class="reward_ads")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			else
				x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#") and not(@class="reward_ads")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			end
			p = p + 1
		end
	end
	if MANGAINFO.ChapterLinks.Count == 0 then
		HTTP.Reset()
		HTTP.Headers.Values['Cache-Control'] = 'no-cache'
		HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
		HTTP.MimeType = 'application/x-www-form-urlencoded'
		local s = ChapterParameters .. id
		if not HTTP.POST(MODULE.RootURL .. '/wp-admin/admin-ajax.php', s) then return net_problem end
		CreateTXQuery(HTTP.Document).XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#") and not(@class="reward_ads")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not u:find('style=list', 1, true) then u = u:gsub('?style=paged', '') .. '?style=list' end

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	x.XPathStringAll('//div[contains(@class, "page-break")]/img/@data-src', TASK.PageLinks)
	if TASK.PageLinks.Count == 0 then x.XPathStringAll('//div[contains(@class, "page-break")]/img/@src', TASK.PageLinks) end
	if TASK.PageLinks.Count == 0 then
		x.ParseHTML('[' .. GetBetween('[', ']', x.XPathString('//script[@id="chapter_preloaded_images"]')) .. ']')
		x.XPathStringAll('json(*)().src', TASK.PageLinks)
	end
	if TASK.PageLinks.Count == 0 then
		local x = CreateTXQuery(HTTP.Document)
		local script = x.XPathString('//script[@id="chapter-protector-data"]')

		if script == '' and MODULE.Storage['fullpageload'] ~= '' then
			local result = require 'utils.nodejs'.run_html_load(string.gsub(u, '?style=list', ''))

			x.ParseHTML(result)
			script = x.XPathString('//script[@id="chapter-protector-data"]')
		end

		if script ~= '' then
			local img = require 'fmd.duktape'.ExecJS(script .. [[

			var CryptoJS = require("utils/crypto-js.min.js");
			var CryptoJSAesJson = require("utils/cryptojs-aes-format.js");
			JSON.parse(CryptoJS.AES.decrypt(chapter_data, wpmangaprotectornonce, { format: CryptoJSAesJson }).toString(CryptoJS.enc.Utf8));

			]]):gsub('\\/', '/'):gsub('%[', ''):gsub('%]', '')

			for v in img:gmatch('"([^",]+)') do
				TASK.PageLinks.Add(v)
			end
		end
	end
	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub('i%d.wp.com/', ''):gsub('cdn.statically.io/img/', '')
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M