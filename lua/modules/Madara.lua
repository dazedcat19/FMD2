Modules = {}

function Modules.Madara()
	local Madara = {}

	function Madara:new()
		local obj = {}
		setmetatable(obj, self)
		self.__index = self
		return obj
	end

	function Madara:getinfo()
		MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
		if HTTP.GET(MANGAINFO.URL) then
			local x=CreateTXQuery(HTTP.Document)

			if MODULE.Name == 'NinjaScans' then
				local fixedHtml = HTTP.Document.ToString():gsub('a href=(.-/)>', 'a href="%1">')
				x.ParseHTML(fixedHtml)
			end

			MANGAINFO.Title=x.XPathStringAll('//div[@class="post-title"]/*[self::h1 or self::h2 or self::h3]/text()', '')
			if MODULE.Name == 'ArtemisNF' then
				MANGAINFO.Title=x.XPathStringAll('//div[@class="post-title post-sigle-title"]/*[self::h1 or self::h2 or self::h3]/text()', '')
			elseif MODULE.Name == 'GetManhwa' then
				MANGAINFO.Title=x.XPathStringAll('//div[@class="post-title-dpage"]/h3')
			elseif MODULE.ID == '38ee90a6e1f343e284478e090399d7d2' then -- MMScans
				MANGAINFO.Title = x.XPathString('//div[@class="series-title"]/h1')
			end
			if MANGAINFO.Title == '' then
				MANGAINFO.Title = x.XPathStringAll('//*[@id="manga-title"]/h1/text()')
			end
			if string.match(MANGAINFO.Title:upper(), ' RAW$') ~= nil then
				MANGAINFO.Title = MANGAINFO.Title:sub(1, -5)
			end
			MANGAINFO.CoverLink=x.XPathString('//div[@class="summary_image"]//img/@data-src')
			if MANGAINFO.CoverLink == '' then
				MANGAINFO.CoverLink=x.XPathString('//div[@class="summary_image"]//img/@data-lazy-src')
			end
			if MANGAINFO.CoverLink == '' then
				MANGAINFO.CoverLink=x.XPathString('//div[@class="summary_image"]//img/@src')
			end
			if MODULE.Name == 'GetManhwa' then
				MANGAINFO.CoverLink=x.XPathString('//div[@class="my_profile-manga"]/@style'):match('background%-image:URL%((.-)%)')
			end
			if MODULE.ID == '38ee90a6e1f343e284478e090399d7d2' then -- MMScans
				MANGAINFO.CoverLink = x.XPathString('//div[@class="series-img"]/img/@data-src')
			end
			MANGAINFO.Authors=x.XPathStringAll('//div[@class="author-content"]/a')
			if MANGAINFO.Authors == '' then
				MANGAINFO.Authors=x.XPathStringAll('//div[@class="summary-heading-creator"]/a')
			end
			MANGAINFO.Artists=x.XPathStringAll('//div[@class="artist-content"]/a')
			MANGAINFO.Genres=x.XPathStringAll('//div[@class="genres-content"]/a')
			if MODULE.Name == 'ATMSubs' then
				MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="summary-heading" and contains(h5, "Statut")]/following-sibling::div'), 'En Cours', 'Complete')
			else
				MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="summary-heading" and contains(h5, "Status")]/following-sibling::div'))
			end
			if MODULE.Name == 'Mangareceh' then
				MANGAINFO.Summary=x.XPathString('//div[contains(@class,"description-summary")]//p[2]')
			else
				MANGAINFO.Summary=x.XPathString('//div[contains(@class,"description-summary")]//p')
			end

			if MODULE.Name == 'DoujinYosh' or MODULE.Name == 'MangaYosh' or MODULE.Name == 'KIDzScan' then
				local v = x.XPath('//li[contains(@class, "wp-manga-chapter")]/a')
				for i = 1, v.Count do
					local v1 = v.Get(i)
					local link = v1.GetAttribute('href')
					if MODULE.Name == 'MangaYosh' then
						link = string.gsub(link, 'https://yosh.tranivson.me', MODULE.RootURL)
					else
						link = string.gsub(link, 'https://doujinyosh.bloghadi.me', MODULE.RootURL)
					end
					MANGAINFO.ChapterNames.Add(v1.ToString());
					MANGAINFO.ChapterLinks.Add(link);
				end
			elseif MODULE.Name == 'PlotTwistNoFansub' then
				x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			elseif MODULE.Name == 'Mangareceh' then
				x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[1]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			elseif MODULE.ID == '8c8adf7a1eba4b7cb449f1fd127fe696' then -- ShieldManga
				x.XPathHREFAll('//li[contains(@class, "wp-manga-hapter")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			elseif MODULE.ID == '187417a02af74223a390dce786bb2a9f' then -- ManhwasMen
				local v for v in x.XPath('//div[@class="chapter-link"]/a').Get() do
					MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
					MANGAINFO.ChapterNames.Add(x.XPathString('p', v))
				end
			elseif MODULE.ID == '123fa1ed637e469b8cb4a154965a6423' then -- DragonTranslation
				local v for v in x.XPath('//li[contains(@class, "wp-manga-chapter")]').Get() do
					MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', v))
					MANGAINFO.ChapterNames.Add(x.XPathString('a/text()[not(parent::span)]', v))
				end
			else
				x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			end
			if MANGAINFO.ChapterLinks.Count == 0 then
				HTTP.Reset()
				HTTP.Headers.Values['Cache-Control'] = 'no-cache'
				HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
				HTTP.Headers.Values['Content-Length'] = '0'
				if HTTP.POST(MANGAINFO.URL .. 'ajax/chapters') then
					local x = CreateTXQuery(HTTP.Document)
					if MODULE.ID == '287f665620664e468d4e05f5d76f5a43' then -- ResetScans
						x.XPathHREFAll('//div[@class="li__text"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
					end
					x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[1]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
					if MANGAINFO.ChapterLinks.Count == 0 then
						local v for v in x.XPath('//div[@class="chapter-link"]/a').Get() do
							MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
							MANGAINFO.ChapterNames.Add(x.XPathString('p', v))
						end
					end
					if MANGAINFO.ChapterLinks.Count == 0 then
						local v for v in x.XPath('//li[@class="chapter-li"]/a').Get() do
							MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
							MANGAINFO.ChapterNames.Add(x.XPathString('div/p', v))
						end
					end
				end
			end
			if MANGAINFO.ChapterLinks.Count == 0 then
				HTTP.Reset()
				HTTP.Headers.Values['Cache-Control'] = 'no-cache'
				HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
				HTTP.MimeType = 'application/x-www-form-urlencoded; charset=UTF-8'
				local idmanga = x.XPathString('//div[contains(@id, "manga-chapters-holder")]/@data-id')
				local q = 'action=manga_get_chapters&manga=' .. idmanga
				if HTTP.POST(MODULE.RootURL .. '/wp-admin/admin-ajax.php', q) then
					local x = CreateTXQuery(HTTP.Document)
					x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[1]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
				end
			end
			MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
			return no_error
		end
		return net_problem
	end

	function Madara:getpagenumber()
		if MODULE.ID == '5a19d20a9731446489df49fd01c7cf77' then -- ChibiManga
			TASK.PageLinks.Clear()
			if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
			local x = CreateTXQuery(HTTP.Document)
			local s = x.XPathString('//script[contains(., "chapter_preloaded_images")]')
			s = "["..GetBetween("[", "]", s).."]"
			x.ParseHTML(s)
			x.XPathStringAll('json(*)()', TASK.PageLinks)
			end
		end

		TASK.PageLinks.Clear()
		local aurl = MaybeFillHost(MODULE.RootURL, URL)
		if string.find(aurl, 'style=list', 1, true) == nil then
			aurl = string.gsub(aurl, '?style=paged', '') .. '?style=list'
		end
		if HTTP.GET(aurl) then
			local x = CreateTXQuery(HTTP.Document)
			if MODULE.Name == 'ManhwaHentai' then
				v = x.XPath('//div[contains(@class, "page-break")]/img')
				for i = 1, v.Count do
					v1 = v.Get(i)
					local src = v1.GetAttribute('src')
					src = src:gsub('https://cdn.shortpixel.ai/client/q_glossy,ret_img/', '')
					TASK.PageLinks.Add(src)
				end
			elseif MODULE.ID == 'f01040ee781d4ae1929031419b97d2e0' then -- VoidScans
				local v for v in x.XPath('//div[contains(@class, "page-break")]/img').Get() do
					local src = v.GetAttribute('src')
					if src:find('src=') then
						src = string.match(src, "src=(.*)&")
						TASK.PageLinks.Add(src)
					end
				end
			else
				x.XPathStringAll('//div[contains(@class, "page-break")]/img/@data-src', TASK.PageLinks)
			end
			if TASK.PageLinks.Count == 0 then
				x.XPathStringAll('//div[@class="entry-content"]//picture/img/@src', TASK.PageLinks)
			end
			if TASK.PageLinks.Count == 0 then
				x.XPathStringAll('//div[contains(@class, "page-break")]/img/@src', TASK.PageLinks)
			end
			if TASK.PageLinks.Count == 0 then
				x.XPathStringAll('//*[@class="wp-manga-chapter-img webpexpress-processed"]/@src', TASK.PageLinks)
			end
			if TASK.PageLinks.Count == 0 then
				x.XPathStringAll('//div[@class="reading-content"]//img/@src', TASK.PageLinks)
			end
			for i = 0, TASK.PageLinks.Count - 1 do -- Bypass 'i0.wp.com' image CDN to ensure original images are loaded directly from host
				TASK.PageLinks[i] = TASK.PageLinks[i]:gsub("i%d.wp.com/", "")
				i = i + 1
			end
			return true
		end
		return false
	end

	function Madara:getnameandlink()
		local perpage = '1500'
		local q = 'action=madara_load_more&page='.. URL ..'&template=madara-core%2Fcontent%2Fcontent-archive&vars%5Bpost_type%5D=wp-manga&vars%5Berror%5D=&vars%5Bm%5D=&vars%5Bp%5D=0&vars%5Bpost_parent%5D=&vars%5Bsubpost%5D=&vars%5Bsubpost_id%5D=&vars%5Battachment%5D=&vars%5Battachment_id%5D=0&vars%5Bname%5D=&vars%5Bstatic%5D=&vars%5Bpagename%5D=&vars%5Bpage_id%5D=0&vars%5Bsecond%5D=&vars%5Bminute%5D=&vars%5Bhour%5D=&vars%5Bday%5D=0&vars%5Bmonthnum%5D=0&vars%5Byear%5D=0&vars%5Bw%5D=0&vars%5Bcategory_name%5D=&vars%5Btag%5D=&vars%5Bcat%5D=&vars%5Btag_id%5D=&vars%5Bauthor%5D=&vars%5Bauthor_name%5D=&vars%5Bfeed%5D=&vars%5Btb%5D=&vars%5Bpaged%5D=1&vars%5Bmeta_key%5D=&vars%5Bmeta_value%5D=&vars%5Bpreview%5D=&vars%5Bs%5D=&vars%5Bsentence%5D=&vars%5Btitle%5D=&vars%5Bfields%5D=&vars%5Bmenu_order%5D=&vars%5Bembed%5D=&vars%5Bignore_sticky_posts%5D=false&vars%5Bsuppress_filters%5D=false&vars%5Bcache_results%5D=true&vars%5Bupdate_post_term_cache%5D=true&vars%5Blazy_load_term_meta%5D=true&vars%5Bupdate_post_meta_cache%5D=true&vars%5Bposts_per_page%5D='.. perpage ..'&vars%5Bnopaging%5D=false&vars%5Bcomments_per_page%5D=50&vars%5Bno_found_rows%5D=false&vars%5Border%5D=ASC&vars%5Borderby%5D=post_title&vars%5Btemplate%5D=archive&vars%5Bsidebar%5D=full&vars%5Bpost_status%5D=publish'
		HTTP.MimeType = 'application/x-www-form-urlencoded; charset=UTF-8'
		if HTTP.POST(MODULE.RootURL .. '/wp-admin/admin-ajax.php', q) then
			if HTTP.Headers.Values['Content-Length'] == '0' then return no_error end
			local x = CreateTXQuery(HTTP.Document)
			if x.XPath('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a').Count == 0 then return no_error end
			x.XPathHREFAll('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a', LINKS, NAMES)
			UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1
			return no_error
		else
			return net_problem
		end
	end

	return Madara
end

function Modules.HentaiRead()
	local HentaiRead = {}
	setmetatable(HentaiRead, { __index = Modules.Madara() })

	function HentaiRead:getpagenumber()
		TASK.PageLinks.Clear()
		if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
			local x = CreateTXQuery(HTTP.Document)
			local s = x.XPathString('//script[contains(., "chapter_preloaded_images")]', TASK.PageLinks)
			s = "["..GetBetween("[", "]", s).."]"
			x.ParseHTML(s)
			x.XPathStringAll('json(*)()', TASK.PageLinks)
			return true
		end
		return false
	end

	return HentaiRead
end

function Modules.OnManga()
	local OnManga = {}
	setmetatable(OnManga, { __index = Modules.Madara() })

	function OnManga:getpagenumber()
		TASK.PageLinks.Clear()
		if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
			local x = CreateTXQuery(HTTP.Document)
			local s = x.XPathString('//script[contains(., "chapter_preloaded_images")]', TASK.PageLinks)
			s = "{"..GetBetween("{", "}", s).."}"
			x.ParseHTML(s)
			x.XPathStringAll('let $c := json(*) return for $k in jn:keys($c) return $c($k)', TASK.PageLinks)
			return true
		end
		return false
	end

	return OnManga
end
-------------------------------------------------------------------------------

function createInstance()
	local m = Modules[MODULE.Name]
	if m ~= nil then
		return m():new()
	else
		return Modules.Madara():new()
	end
end

-------------------------------------------------------------------------------

function getinfo()
	return createInstance():getinfo()
end

function getpagenumber()
	return createInstance():getpagenumber()
end

function getnameandlink()
	return createInstance():getnameandlink()
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])
	return true
end

-------------------------------------------------------------------------------
function Init()
	local cat = 'Raw'
	local function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                    = id
		m.Name                  = name
		m.RootURL               = url
		m.Category              = cat
		m.OnGetInfo             = 'getinfo'
		m.OnGetPageNumber       = 'getpagenumber'
		m.OnGetNameAndLink      = 'getnameandlink'
		m.OnBeforeDownloadImage = 'BeforeDownloadImage'
	end
	AddWebsiteModule('29e070b824344c8697ceb9554a6d1d4b', 'MangazukiClub', 'https://mangazuki.club')
	AddWebsiteModule('1d7090e3140f4957973a0e063c43f106', 'ManhwaRaw', 'https://manhwaraw.com')
	AddWebsiteModule('f164e6052faf4c8a896a8be3cef7cb61', 'RawDex', 'https://rawdex.net')
	AddWebsiteModule('b70544bbe16d42a0a70a2a483aee718c', 'Manga-1001', 'https://doki1001.com')

	cat = 'English'
	AddWebsiteModule('4bce5afe51b646c6b0d30329a069ee83', 'IsekaiScan', 'https://isekaiscan.com')
	AddWebsiteModule('da2696a55d66491093c8fed44cc862fc', 'MangaZukiWhatStatus', 'https://whatstatus.co')
	AddWebsiteModule('b0b9bb6881b54db9a9f55d97ad2412cf', 'MangaZukiOnline', 'https://www.mangazuki.online')
	AddWebsiteModule('bc9f265005764a729a937792b59ec0e8', 'MangaZukiSite', 'https://www.mangazuki.site')
	AddWebsiteModule('c5013ac1bc754fdca1f6206507dad6f0', 'MangaZukiMe', 'https://mangazuki.me')
	AddWebsiteModule('41f215fc066f462497a3ce177d1384d2', 'YoManga', 'https://yomanga.info')
	AddWebsiteModule('2885dbccc8e8472ca21b819eb31031fa', 'OnManga', 'https://onmanga.com')
	AddWebsiteModule('ab063b144d1f465697374fb8a2222edc', 'ReadRawManga', 'https://www.readrawmanga.com')
	AddWebsiteModule('0d904cb0bdf6475e9b36ad835cd3050c', 'MangaStreamCC', 'https://www.mangastream.cc')
	AddWebsiteModule('d42f178a248241c89f943fa1b77c5df8', 'TeabeerComics', 'https://teabeercomics.com')
	AddWebsiteModule('5cca6c0272c84ae98a92b55d2106d6e0', 'MangaNeloTeam', 'https://manganeloteam.com')
	AddWebsiteModule('b618f80c4b0242ce9f39af4178d03be1', 'AstralLibrary', 'https://astrallibrary.net')
	AddWebsiteModule('829fe500a3e64b8f9637a3d5ebdb4e11', 'LilyManga', 'https://lilymanga.com')
	AddWebsiteModule('e3f643dfdca24576adf5b87c99878bfb', 'Mangax18', 'https://mangax18.com')
	AddWebsiteModule('29003d3eb6c348ab8f8d4ed0cb8f7c6f', 'AllTopManga', 'https://alltopmanga.com')
	AddWebsiteModule('89497fea6b55470c89fac1eb8f75f61a', 'MangaHyphenTx', 'https://manga-tx.com')
	AddWebsiteModule('e62d472cc686400b91528c5b52ed0452', 'MangaBin', 'https://mangabin.com')
	AddWebsiteModule('27433894c7594c97ad00ea083947c28c', 'Manga1stOnline', 'https://manga1st.online')
	AddWebsiteModule('e4e226f0270440b7b906171082c8a02f', 'ColoredManga', 'https://coloredmanga.com')
	AddWebsiteModule('58f2ead66dd441ed83166a1676317d19', 'ComicOnline', 'https://comiconline.org')

	cat = 'English-Scanlation'
	AddWebsiteModule('f17a22a1a24640ecb2ae4f51c47a45c8', 'TrashScanlations', 'https://trashscanlations.com')
	AddWebsiteModule('5a19d20a9731446489df49fd01c7cf77', 'ChibiManga', 'https://www.cmreader.info')
	AddWebsiteModule('6812a5f225734ae5acd4b851d66a1450', 'SiXiangScans','http://www.sixiangscans.com')
	AddWebsiteModule('bb35ade3f2bd46b1a6617756eddafb8d', 'NinjaScans', 'https://ninjascans.com')
	AddWebsiteModule('3858e0e419ef41789e3e869ad1d56cd6', 'ReadManhua', 'https://readmanhua.net')
	AddWebsiteModule('9bec8a0a18e94de1b99c2bc2598438b4', 'MangaDods', 'https://www.mangadods.com')
	AddWebsiteModule('a1b569bcec9147f4945383655c052676', 'RaiderScans', 'https://raiderscans.com')
	AddWebsiteModule('f95e0abd491e48aa81e686d14c0b983f', 'MartialScans', 'https://martialscans.com')
	AddWebsiteModule('c07a3a37cb3f4e1a8fe48ad156056887', 'HeroManhua', 'https://heromanhua.com')
	AddWebsiteModule('b810006e1bba4fe3a22bdbf9b235a537', 'MangaTurf', 'https://mangaturf.com')
	AddWebsiteModule('805140cf71b44550a99d74f63bb50a2a', 'Aloalivn', 'https://aloalivn.com')
	AddWebsiteModule('5f60bc66acfa4605a3668857287eed1d', 'MangaGreat', 'https://mangagreat.com')
	AddWebsiteModule('1b7fba26de3e49d7af3ab6549921c567', 'SKScans', 'https://skscans.com')
	AddWebsiteModule('716dab165a0f4f80b0ee49a4518866a2', 'GrazeScans', 'https://grazescans.com')
	AddWebsiteModule('b94734480a0e4f25a21c0f01cd5e9779', 'XuNScans', 'https://xunscans.xyz')
	AddWebsiteModule('5aca1e42e8544aff8420b617c7a3983e', 'ComicDom', 'https://comicdom.org')
	AddWebsiteModule('4e56f26935a74ffca73f698fa4c9de5c', 'HScans', 'https://hscans.com')
	AddWebsiteModule('825fa36408ca4f7ab2eb7c4ad7777aca', 'LevelerScans', 'https://levelerscans.xyz')
	AddWebsiteModule('a6e1277d9b8b4f99afb2de63f4718eb9', 'JiroComics', 'https://jirocomics.com')
	AddWebsiteModule('c34264a0b72a49b58f2a4e9476f2fd15', 'Mangas20', 'https://mangas20.com')
	AddWebsiteModule('5e8a01ec43e24ed28372bab7f2c2c531', 'DragonTea', 'https://dragontea.ink')
	AddWebsiteModule('1bc20f34e9c8466bbf5898a89c374a3b', 'MangaVisa', 'https://mangavisa.com')
	AddWebsiteModule('ca3c550ab0f249d0accf06286e908cbe', 'ManhuaPlanet', 'https://manhuaplanet.com')
	AddWebsiteModule('47292820ced14adfbdcb3d5412ebed9f', 'ManhuaPlusOnline', 'https://manhuaplus.online')
	AddWebsiteModule('4900e6bfedcf45a08a5407436ddd78c2', 'MangaKing', 'https://mangaking.net')
	AddWebsiteModule('ba20dccc488d4abbab4f672937fe4b5e', 'AstraScans', 'https://astrascans.com')
	AddWebsiteModule('e84127c9687d4bed8658922ee9feb9a1', 'Anshascans', 'https://anshscans.org')

	cat = 'French'
	AddWebsiteModule('41867fa36f2f49959df9fef8aa53ffb5', 'WakaScan', 'https://wakascan.com')
	AddWebsiteModule('d41da6d28179493ab074698a3a60cbcd', 'ATMSubs', 'https://atm-subs.fr')
	AddWebsiteModule('d31eddfe86ee4ac584f3120d4cc6f8c9', 'NovelFrance', 'http://novel-france.fr')

	cat = 'Indonesian'
	AddWebsiteModule('6c3cb7a05d8243d8817a81d3875ff1a1', 'MangaYosh', 'https://mangayosh.xyz')
	AddWebsiteModule('9ff90d87df4c48fbb1cd310cbccca181', 'KomikGo', 'https://komikgo.com')
	AddWebsiteModule('deafed31465943078a7b67ddf4885fda', 'KomikTap', 'https://manhwa.komiktap.co')
	AddWebsiteModule('fce276573cab499fad96374d1df22121', 'Mangceh', 'https://mangceh.me')
	AddWebsiteModule('1da2819dd79f4967a12953495ec52afe', 'Manhwaid', 'https://manhwaid.club')
	AddWebsiteModule('545acbf017814caab2b6bb28e48779fa', 'MGKomik', 'https://mgkomik.com')

	cat = 'H-Sites'
	AddWebsiteModule('c0214763110b4d14b6e359ecef6df2e4', 'DoujinYosh', 'https://doujinyosh.fun')
	AddWebsiteModule('d8e44dca037b4e159d320466adc06ca3', 'ManhwaHentai', 'https://manhwahentai.me')
	AddWebsiteModule('dfdec22299bc4fc6adbad401eeca2211', 'HentaiRead', 'http://hentairead.com')
	AddWebsiteModule('349e30b0c30643f4a8d0aaece2a2c41e', 'ManhwaClub', 'https://manhwa.club')
	AddWebsiteModule('c87aeae76e884adc9de8bc6b9d56f2c6', 'ShosetsuManga', 'https://www.shosetsu-manga.org')
	AddWebsiteModule('7fd5031b766a4d228fd0940c0cdb1424', 'MangaHentai', 'https://mangahentai.me')
	AddWebsiteModule('58f8a130748443c285036e4c4bf49fe8', 'Hentaidexy', 'https://hentaidexy.com')
	AddWebsiteModule('9bdde4abb0a24e1783c8f3681839d064', 'HManhwa', 'https://hmanhwa.com')

	cat = 'Spanish-Scanlation'
	AddWebsiteModule('eb72aedce86b4598b3d702bc055079e8', 'GodsRealmScan', 'https://godsrealmscan.com')
	AddWebsiteModule('f1614587f5ca4f9588943f73f2434711', 'DarkskyProjects', 'https://darkskyprojects.org')
	AddWebsiteModule('a142bbc912ce48e1a1fbee17d48d2aa2', 'PlotTwistNoFansub', 'https://www.plotwistscan.com')
	AddWebsiteModule('59742c2d007d47c487ce93b7db44b8f4', 'ZManga', 'https://zmanga.org')
	AddWebsiteModule('3a669fd3bd104cfbbc917520d6483d0a', 'SDLGFansub', 'https://www.sdlg-fansub.tk')
	AddWebsiteModule('7976200824a7422d8f16a71dea38f672', 'ArtemisNF', 'https://artemisnf.com')
	AddWebsiteModule('cfc11bf4971f4db892f96867239771e7', 'LazyBoysScan', 'https://lazyboysscan.com')
	AddWebsiteModule('a9e9305e841f43f588a0deff0bc6ac88', 'BDSFansub', 'https://bdsfansub.com')
	AddWebsiteModule('df25c494b176424785ee82142d0af425', 'LeerManhua', 'https://leermanhua.com')
	AddWebsiteModule('1a9038c97ad04b6b8c54425a01b0bcb9', 'BakaguyaScan', 'https://bakaguya-scan.tk')
	AddWebsiteModule('2458c40d646f4c0796aa550558e5e6b4', 'MundoWuxia', 'https://mundowuxia.com')
	AddWebsiteModule('06e732a1ccdc4894877e5dee6e065df1', 'LeviatanScans', 'https://es.leviatanscans.com')
	AddWebsiteModule('aeb97b0ab5db4fbfa12990119f1904e2', 'MangaFenix', 'https://manga-fenix.com')
	AddWebsiteModule('8bb50a7089ca41439e99813df2c8bf49', 'HZmangas', 'https://hzmangas.com')
	AddWebsiteModule('48d4e659f6ed4498a8586258371a4015', 'AncientEmpireScan', 'https://www.ancientempirescan.website')
	AddWebsiteModule('73cfa250c661470c81428d99cdb8a140', 'MangaCrab', 'https://mangacrab.com')
	AddWebsiteModule('ed8828b5ea52499c9217435d1b6cd437', 'InmortalScan', 'https://manga.mundodrama.site')
	AddWebsiteModule('29714bf99a7e4dafa8bd6f88ca0b7b5e', 'StickHorse', 'https://www.stickhorse.cl')
	AddWebsiteModule('df82ea6b9f1442e580320d8449ca5ab6', 'Sksubs', 'https://sksubs.net')
	AddWebsiteModule('c59285fdab344262bb6ae8a6a2687277', 'TecnoScan', 'https://tecnoscann.com')
	AddWebsiteModule('a29b6757649f4683b72d2e669d9fbffc', 'MHscans', 'https://mhscans.com')
	AddWebsiteModule('73cfa250c661470c81428d99cdb8a140', 'Mangastk18', 'https://mangastk18.com')

	cat = 'Webcomics'
	AddWebsiteModule('d903d55663f0423ca4dd928c8203f7ce', 'PocketAngelScan', 'https://pocketangelscans.com')
	AddWebsiteModule('4f0a570000854509b71350983fa55eec', 'ManhuaBox', 'https://manhuabox.net')
	AddWebsiteModule('00f2827e03654ad5bcde3b7ae536b416', 'Manga3S', 'https://manga3s.com')
	AddWebsiteModule('8a230d1a3f5c416fa71d07127e8dfab5', 'GetManhwa', 'https://getmanhwa.co')
	AddWebsiteModule('35da2f2d501c4035b03c7a66c449cb90', 'Manhuas', 'https://manhuas.net')
	AddWebsiteModule('9dd41197c1d74ce780502dddc7515722', 'MixedManga', 'https://mixedmanga.com')
	AddWebsiteModule('dcbd58d5134d424b8e11802bf0671873', 'MangaDao', 'https://mangadao.com')
	AddWebsiteModule('74e6c5f8121846029f367d48db7da3d6', '365Manga', 'https://365manga.com')
	AddWebsiteModule('59387ea5d936420290485efe70432f07', 'MangaBob', 'https://mangabob.com')
	AddWebsiteModule('35a8b3b4c8064f589aa7da94bb52f1fd', 'Manga68', 'https://manga68.com')
	AddWebsiteModule('35a207b2fd0c47b68e78b531b57cde3f', 'MangaLord', 'https://www.mangalord.com')
	AddWebsiteModule('03cb00729e26448294329face3a8b53b', 'MiracleScans', 'https://miraclescans.com')
	AddWebsiteModule('c368172d02684553925cc7675490ae6e', 'WebNovelLive', 'https://webnovel.live')
	AddWebsiteModule('94b5ff5f2dd3421ab9e76113d5a1e600', 'MangaRockTeam', 'https://mangarockteam.com')
	AddWebsiteModule('2b653482cef845d980a0cba2a7e798e0', 'PhenomenalNoFansub', 'https://phenomenalnofansub.com')
	AddWebsiteModule('6d15a803c5504e91918a9d5f2b1351f8', 'WebtoonXYZ', 'https://www.webtoon.xyz')
	AddWebsiteModule('249da73cb3e941f2932b0435710e4a65', 'ManhwaPool', 'https://manhwapool.com')
	AddWebsiteModule('9bd1dd136cef40608ed691546251f46b', 'AHStudios', 'https://ahstudios.net')
	AddWebsiteModule('3dc5bebff80f4c1aa19298ddcf8cb504', 'FenixManga', 'https://fenixscan.com')
	AddWebsiteModule('2ea2c910054841e5bd82f2d34f6e832a', 'Apolltoons', 'https://apolltoons.xyz')
	AddWebsiteModule('a964861afd0f410cb5122df5f50d3e1a', 'ManhuaPlus', 'https://manhuaplus.com')
	AddWebsiteModule('f31e7a36050c424291c839f0a02d92a4', 'ApollComics', 'https://apollcomics.xyz')
	AddWebsiteModule('404bb1f54c374a53bd850afdd6f873b3', 'ToonilyNet', 'https://toonily.net')
	AddWebsiteModule('ae84d6cc416a473d85cf9fa416305378', 'SkyManga', 'https://skymanga.xyz')
	AddWebsiteModule('c22df90060eb4d44b7baf89791745d5c', 'ManhuasWorld', 'https://manhuasworld.com')
	AddWebsiteModule('d031c93743204c6e9f647faa9b31db2b', 'AncientEmpireScan', 'https://www.ancientempirescan.site')
	AddWebsiteModule('46e0c618a19748d6af150c2f198f5360', '1stKissManhua', 'https://1stkissmanhua.com')
	AddWebsiteModule('0a296830cf814700ac18c74139982679', 'TwilightScans', 'https://twilightscans.com')
	AddWebsiteModule('d3113e5b43b94fd9b3b99d2cb75787d7', 'MangaKik', 'https://mangakik.biz')
	AddWebsiteModule('fa9659f5511441c6a5b5cc969d91a204', 'MangaNeloLink', 'https://manganelo.link')
	AddWebsiteModule('c7406bf3452343f8adb4cd257c9222cb', 'ComicKiba', 'https://comickiba.com')
	AddWebsiteModule('38ee90a6e1f343e284478e090399d7d2', 'MMScans', 'https://mm-scans.org')
	AddWebsiteModule('6d95277ed6864a16aeebf025ca667c3b', 'NovelMic', 'https://novelmic.com')
	AddWebsiteModule('8c8adf7a1eba4b7cb449f1fd127fe696', 'ShieldManga', 'https://shieldmanga.club')
	AddWebsiteModule('8e96a101438b401daf3a500590c91d62', 'Skymanhwa', 'https://skymanhwa.com')
	AddWebsiteModule('80eb1cd9d38a4cb3ae4ea57f993847eb', 'S2Manga', 'https://s2manga.com')
	AddWebsiteModule('33618723b5a74da3a28275258a47ad67', 'FreeComicOnline', 'https://freecomiconline.me')
	AddWebsiteModule('287f665620664e468d4e05f5d76f5a44', 'HadesNoFansub', 'https://mangareaderpro.com/es')
	AddWebsiteModule('287f665618784e468d4e05f5d76f5a45', 'EroManhwas', 'https://eromanhwas.com')
	AddWebsiteModule('d9615a731b1243538663e96f0c1ad595', 'Manga347', 'https://manga347.com')
	AddWebsiteModule('99bb0476a95e4590848c8fdc0c03817c', 'MangaCultivator', 'https://mangacultivator.com')
	AddWebsiteModule('441c6a5b5cwt6590848c8fdd9d9b67a7', 'EroMangacrab', 'https://ero.mangacrab.com')
	AddWebsiteModule('187417a02af74223a390dce786bb2a9f', 'ManhwasMen', 'https://manhwas.men')

	cat = 'Turkish'
	AddWebsiteModule('baeb4d0c63d9456dbc8da6f1d29faf60', 'AdonisFansub', 'https://manga.adonisfansub.com')

	cat = 'Arabic'
	AddWebsiteModule('dec0b341fb92445ab6c435053941f2bd', 'MangaAction', 'https://manga-action.com')
	AddWebsiteModule('7afaf3a070bc4bd499255c3fd8dec1f3', 'NijiTranslations', 'https://niji-translations.com')
	AddWebsiteModule('c9fc048e3f82419996345bbb1626f7f7', 'MangaArabTeam', 'https://mangaarabteam.com')

	cat = 'Portuguese'
	AddWebsiteModule('59d791cfd7dc425897581c231f6f6481', 'IchirinNoHanaYuri', 'https://ichirinnohanayuri.com.br')
	AddWebsiteModule('c6839e89512b46dda84a5992480b63e0', 'YaoiToshokan', 'https://www.yaoitoshokan.com.br')

	cat = 'Russian'
	AddWebsiteModule('b1ffe6dfda364ebcaad07eb2fd4aeae9', 'BestManga', 'https://bestmanga.club')

	cat = 'Adult'
	AddWebsiteModule('53ea37e05bb6487e8bfe0dde6248fdc6', 'Milftoon', 'https://milftoon.xxx')

	cat = 'Portuguese-Scanlation'
	AddWebsiteModule('b2395d7d5e5244fea800216c12cac7dd', 'NeoxScanlator', 'https://neoxscans.net')

	cat = 'Adult'
	AddWebsiteModule('8760050d777d432dab8e776e3f1a6474', 'PornComix', 'https://porncomixinfo.net')

	cat = 'Spanish'
	AddWebsiteModule('22c481c279c740b897ffc906ab808502', 'TuMangaNet', 'https://tumanga.net')
	AddWebsiteModule('e4a3a8ed254e4e9a9113693c098bb743', 'MangaFenix', 'https://manga-fenix.com')
	AddWebsiteModule('a8ad2a81768742caa7135047f28c2b00', 'SwordManga', 'https://swordmanga.com')
	AddWebsiteModule('rrad2a81768742caa7135047f282b777', 'MangaMonarca', 'https://mangamonarca.xyz')
	AddWebsiteModule('48822a81768742caa7135047f282bf57', 'TiempodeWebeo', 'https://tiempodewebeo.com')
	AddWebsiteModule('123fa1ed637e469b8cb4a154965a6423', 'DragonTranslation', 'https://dragontranslation.com')
end
