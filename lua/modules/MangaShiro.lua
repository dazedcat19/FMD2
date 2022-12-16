function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = getTitle(x)
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, getCover(x))
		MANGAINFO.Authors   = getAuthors(x)
		MANGAINFO.Artists   = getArtists(x)
		MANGAINFO.Genres    = getGenres(x)
		MANGAINFO.Status    = MangaInfoStatusIfPos(getStatus(x))
		MANGAINFO.Summary   = getSummary(x)
		getMangas(x)
		reverseLinksAndChapters()
		HTTP.Reset()
		HTTP.Headers.Values['Referer'] = MANGAINFO.URL
		return no_error
	else
		return net_problem
	end
end

function getTitle(x)
	local title = ''
	if title == '' then title = x.XPathString('//div[@class="thumb"]/img/@alt') end
	if title == '' then title = x.XPathString('//*[@id="judul"]/h1') end
	if title == '' then title = x.XPathString('//*[@id="judul_komik"]/h1') end
	if title == '' then title = x.XPathString('//div[@class="infox"]/h1') end
	if title == '' then title = x.XPathString('//h1[@itemprop="headline"]') end
	if title == '' then title = x.XPathString('//h1[@itemprop="name"]') end
	if title == '' then title = x.XPathString('//div[@class="info1"]/*') end
	if title == '' then title = x.XPathString('//div[@class="mangainfo"]/h1') end
	if title == '' then title = x.XPathString('//h2[@class="entry-title"]') end
	if title == '' then title = x.XPathString('//div[@class="series-title"]/h2') end
	if title == '' then title = x.XPathString('//h1/text()') end
	if title == '' then title = x.XPathString('//h2') end
	title = title:gsub('Bahasa Indonesia$', ''):gsub(' Indonesia|Baca"', ''):gsub('Bahasa Indonesia', ''):gsub('Komik', ''):gsub(' Raw', ''):gsub(' Indonesia Terbaru','')
	title = title:gsub('Indonesia', ''):gsub('Baca', ''):gsub('bahasa', ''):gsub('indonesia', ''):gsub('|', ''):gsub('-', '')
	title = title:gsub(string.gsub(MODULE.Name, 'https://', ''), '')
	return title
end

function getCover(x)
	local img = ''
	if img == '' then img = x.XPathString('//div[@class="series-thumb"]/img/@data-src') end
	if img == '' then img = x.XPathString('//div[@class="series-thumb"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@class="thumb"]/img/@data-lazy-src') end
	if img == '' then img = x.XPathString('//div[@class="thumb"]/img/@data-src') end
	if img == '' then img = x.XPathString('//div[@class="thumb"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@class="imgdesc"]/img/@src') end
	if img == '' then img = x.XPathString('//div[contains(@class,"leftImage")]/img/@src') end
	if img == '' then img = x.XPathString('//div[@class="imgseries"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@itemprop="image"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@class="mangainfo"]//div[@class="topinfo"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@id="m-cover"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@itemprop="image"]/img/@data-lazy-src') end
	if img == '' then img = x.XPathString('//div[@class="img"]/img[@itemprop="image"]/@src') end
	if img == '' then img = x.XPathString('//div[@class="ims"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@class="komik_info-content-thumbnail"]/img/@src') end
	if img == '' then img = x.XPathString('//img[@class="shadow"]/@src') end
	if img == '' then img = x.XPathString('//div[@class="wrapper"]//figure[@class="thumbnail"]//img/@src') end
	return img
end

function getAuthors(x)
	local authors = ''
	if authors == '' then authors = x.XPathStringAll('//span[@class="author"]') end
	if authors == '' then authors = x.XPathString('//div[@class="spe"]//span[starts-with(.,"المؤلف")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//li[starts-with(.,"Komikus")]/b') end
	if authors == '' then authors = x.XPathString('//div[@class="listinfo"]//li[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//span[@class="details"]//div[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//div[@class="preview"]//li[starts-with(.,"Komikus")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//div[@class="spe"]//span[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//table[@class="attr"]//tr[contains(th, "Author")]/td') end
	if authors == '' then authors = x.XPathString('//table[@class="listinfo"]//tr[contains(th, "Penulis")]/td') end
	if authors == '' then authors = x.XPathString('//*[@class="anf"]//li[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//div[@class="listinfo"]//li[starts-with(.,"Pengarang")]/substring-after(.," ")') end
	if authors == '' then authors = x.XPathString('//span[@id="m-author"]') end
	if authors == '' then authors = x.XPathString('//ul[@class="baru"]/li[2][starts-with(.,"Mangaka")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//table[@class="listinfo"]//tr[contains(th, "Author")]/following-sibling::td') end
	if authors == '' then authors = x.XPathString('//tr[contains(td, "Komikus")]//following-sibling::td') end
	if authors == '' then authors = x.XPathString('//div[@class="fmed"]/b[starts-with(.,"Author")]//following-sibling::span') end
	if authors == '' then authors = x.XPathString('//td[@itemprop="creator"]') end
	if authors == '' then authors = x.XPathString('//td[contains(., "Author")]/following-sibling::td') end
	if authors == '' then authors = x.XPathString('//li[contains(b, "Author")]//following-sibling::span') end
	if authors == '' then authors = x.XPathString('//div[@class="spe"]/span[contains(., "Author")]/substring-after(., "Author")') end
	if authors == '' then authors = x.XPathString('//span[contains(., "Author")]/substring-after(., ":")') end
	return authors
end

function getArtists(x)
	local artists = ''
	if artists == '' then artists = x.XPathString('//div[@class="spe"]//span[starts-with(.,"Artist")]/substring-after(.,":")') end
	if artists == '' then artists = x.XPathString('//div[@class="fmed"]/b[starts-with(.,"Artist")]//following-sibling::span') end
	if artists == '' then artists = x.XPathString('//td[contains(., "Artist")]/following-sibling::td') end
	return artists
end

function getGenres(x)
	local genre = ''
	if genre == '' then genre = x.XPathStringAll('//div[@class="seriestugenre"]/a') end
	if genre == '' then genre = x.XPathStringAll('//span[@class="mgen"]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="spe"]//span[contains(.,"التصنيفات")]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="spe"]//span[starts-with(.,"Genres:")]/substring-after(.,":")') end
	if genre == '' then genre = x.XPathStringAll('//div[contains(@class,"animeinfo")]/div[@class="gnr"]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="gnr"]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[contains(@class,"mrgn animeinfo")]/div[@class="gnr"]/a') end
	if genre == '' then genre = x.XPathStringAll('//span[@id="m-genre"]') end
	if genre == '' then genre = x.XPathStringAll('//table[@class="listinfo"]//tr[contains(th, "Genre")]/td/a') end
	if genre == '' then genre = x.XPathStringAll('//table[@class="attr"]//tr[contains(th, "Genres")]/td/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="spe"]//span[starts-with(.,"Genre")]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="spe"]//span[starts-with(.,"Genres")]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="genrex"]/a') end
	if genre == '' then genre = x.XPathStringAll('//ul[@class="genre"]/li') end
	if genre == '' then genre = x.XPathStringAll('//span[@class="details"]//div[starts-with(.,"Genre")]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="listinfo"]//li[starts-with(.,"Genre")]/substring-after(.,":")') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="genre-info"]/a') end
	if genre == '' then genre = x.XPathStringAll('//table[@class="inftable"]//tr[contains(td, "Genres")]/td/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="series-genres"]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="tags"]/a') end
	return genre
end

function getStatus(x)
	local status = ''
	if status == '' then status = x.XPathString('//span[@class="status Ongoing"]') end
	if status == '' then status = x.XPathString('//div[@class="spe"]//span[starts-with(.,"الحالة")]/substring-after(.,":")') end
	if status == '' then status = x.XPathString('//div[@class="spe"]//span[starts-with(.,"Status:")]/substring-after(.,":")') end
	if status == '' then status = x.XPathString('//div[@class="listinfo"]//li[starts-with(.,"Status")]/substring-after(.," ")') end
	if status == '' then status = x.XPathString('//*[@class="anf"]//li[starts-with(.,"Status")]/substring-after(.,":")') end
	if status == '' then status = x.XPathString('//span[@id="m-status"]') end
	if status == '' then status = x.XPathString('//table[@class="listinfo"]//tr[contains(th, "Status")]/td') end
	if status == '' then status = x.XPathString('//table[@class="attr"]//tr[contains(th, "Status")]/td') end
	if status == '' then status = x.XPathString('//div[@class="preview"]//li[starts-with(.,"Tanggal Rilis")]/substring-after(.,"-")') end
	if status == '' then status = x.XPathString('//span[@class="details"]//div[starts-with(.,"Status")]') end
	if status == '' then status = x.XPathString('//ul[@class="baru"]/li[3]') end
	if status == '' then status = x.XPathString('//div[@class="imptdt" and contains(.,"Status")]/i') end
	if status == '' then status = x.XPathString('//td[contains(., "Status")]//following-sibling::td') end
	if status == '' then status = x.XPathString('//div[@class="spe"]/span[starts-with(., "Status")]/substring-after(., "Status")') end
	if status == '' then status = x.XPathString('//span[contains(., "Status")]/substring-after(., ":")') end
	status = status:gsub('Finished', 'Completed'):gsub('Publishing', 'Ongoing')
	status = status:gsub('Berjalan', 'Ongoing'):gsub('Tamat', 'Completed')
	return status
end

function getSummary(x)
	local summary = ''
	if summary == '' then summary = x.XPathString('//div[@class="series-synops"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//div[@class="sinopsis"]/p') end
	if summary == '' then summary = x.XPathString('//*[@class="desc"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//*[@class="sinopsis"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//*[@id="m-synopsis"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//*[@class="sin"]/p') end
	if summary == '' then summary = x.XPathString('//*[@class="description"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//div[contains(@class,"animeinfo")]/div[@class="rm"]/span/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//*[@class="jds"]/p') end
	if summary == '' then summary = x.XPathString('//*[@itemprop="description"]/string-join(.//text()[not(parent::script)],"")') end
	if summary == '' then summary = x.XPathString('//*[@class="komik_info-description-sinopsis"]') end
	summary = summary:gsub('.fb_iframe_widget_fluid_desktop iframe', ''):gsub('width: 100%% !important;', ''):gsub('{', ''):gsub('}', '')
	return summary
end

function getMangas(x)
	if MODULE.ID == '7a74b2abda1d4b329ee1d1fa58866c03' or MODULE.ID == '46dcfabe757140e7980ec34c65bdb30f' then -- MaidMangaID, SekteKomik
		local v for v in x.XPath('//ul[@class="series-chapterlist"]//a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('span[1]/text()', v))
		end		
	elseif MODULE.ID == '5af0f26f0d034fb2b42ee65d7e4188ab' then -- Komiku
		x.XPathHREFTitleAll('//td[@class="judulseries"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	elseif MODULE.ID == '421be2f0d918493e94f745c71090f359' then -- Mangafast
		local v for v in x.XPath('//a[@class="chapter-link"]').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('div/span[@class="left"]', v))
		end	
	elseif MODULE.ID == '13c6434a0c2541b18abee83a2c72e8f5' then -- MangaKane
		x.XPathHREFTitleAll('//div[@class="flexch-infoz"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	elseif MODULE.ID == 'b8206e754d4541689c1d367f7e19fd64' then -- KomikCast
		x.XPathHREFAll('//div[@class="komik_info-chapters"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	elseif MODULE.ID == 'fb34a56c83f54b19b57a9a92070fe899' then -- FlameScans
		local v for v in x.XPath('//*[@id="chapterlist"]//li/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'):gsub("%/[0-9]+-", "/"))
			MANGAINFO.ChapterNames.Add(x.XPathString('div/div/span[@class="chapternum"]',v))
		end
	elseif MODULE.ID == 'f8c92a9a83f64deebee5ec58b7b15bdf' then -- xCaliBRScans
		local v for v in x.XPath('//*[@id="chapterlist"]//*[@class="eph-num"]/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('../span[@class="chapternum"]',v))
		end
	else
		-- common
		local v for v in x.XPath('//*[@id="chapterlist"]//*[@class="eph-num"]/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('span[@class="chapternum"]',v))
		end
		
		if MANGAINFO.ChapterLinks.Count == 0 then x.XPathHREFAll('//li//span[@class="leftoff"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count == 0 then x.XPathHREFAll('//div[@class="bxcl"]//li//*[@class="lchx"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count == 0 then x.XPathHREFAll('//div[@class="bxcl"]//li//div[@class="lch"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count == 0 then x.XPathHREFAll('//div[@class="bxcl nobn"]//li//div[@class="lch"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count == 0 then x.XPathHREFAll('//ul[@class="lcp_catlist"]//li/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count == 0 then x.XPathHREFAll('//div[contains(@class, "bxcl")]//li//*[contains(@class,"lchx")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count == 0 then x.XPathHREFAll('//div[contains(@class, "lchx")]//li//*[contains(@class,"bxcl")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count == 0 then x.XPathHREFAll('//*[@class="lchx"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		if MODULE.ID == '5af0f26f0d034fb2b42ee65d7e4188ab' then -- Komiku
			x.XPathStringAll('//*[@id="Baca_Komik"]/img/@src', TASK.PageLinks)
		elseif MODULE.ID == '421be2f0d918493e94f745c71090f359' then -- Mangafast
			x.XPathStringAll('//*[contains(@class, "content-comic")]/img/@src', TASK.PageLinks)
		elseif MODULE.ID == 'c8e02b7aaac1412180db86374fc799a8' then -- ManhwasNet
			x.XPathStringAll('//*[@class="reader-area"]/img/@data-src', TASK.PageLinks)
		elseif MODULE.ID == 'ca571825056b4850bd3693e4e1437997' then -- Mangacan
			local duktape = require 'fmd.duktape'
			local execute = x.XPathStringAll('//script[contains(.,"var fff")]'):match('(.*)abcd%(fff%)')
			local result = duktape.ExecJS('var CryptoJS = require("utils/crypto-js.min.js");' .. execute ..'abcd(fff);ffff;')
			x.ParseHTML(result)
			x.XPathStringAll('//img/@src', TASK.PageLinks)
		elseif MODULE.ID == 'b8206e754d4541689c1d367f7e19fd64' then -- KomikCast
			x.ParseHTML(GetBetween('"nrm":', '} |', x.XPathString('//script[contains(., "chapterImages")]')):gsub('\\/', '/'):gsub('\\"', ''))
			x.XPathStringAll('//img/@src', TASK.PageLinks)
		elseif MODULE.ID == '7103ae6839ea46ec80cdfc2c4b37c803' then -- AsuraScans
			local v for v in x.XPath('//*[@id="readerarea"]/p/img').Get() do
				if string.find(v.GetAttribute('src'), "panda") == nil then
					TASK.PageLinks.Add(v.GetAttribute('src'))
				end
			end
		elseif MODULE.ID == 'edf6b037808442508a3aaeb1413699bf' then -- KomikIndoID
			x.XPathStringAll('//*[@id="Baca_Komik"]//img/@src', TASK.PageLinks)
		elseif MODULE.ID == 'ec1a1ad5301f414592f0ba0402024813' then -- Doujindesu
			x.XPathStringAll('//div[@class="main"]//img/@src', TASK.PageLinks)
		else
			-- common
			x.ParseHTML(GetBetween('run(', ');', x.XPathString('//script[contains(., "ts_reader")]')))
			x.XPathStringAll('json(*).sources()[1].images()', TASK.PageLinks)

			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@class="reader-area"]//img/@data-src', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@class="reader-area"]//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@id="readerarea"]//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@id="readerarea"]/p//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@id="readerarea"]/div//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@id="readerarea"]//a/@href', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@id="readerareaimg"]//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@id="imgholder"]//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@class="entry-content"]//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@class="bc"]/img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@id="chimg"]/img/@data-lazy-src', TASK.PageLinks) end
			if TASK.PageLinks.Count == 0 then x.XPathStringAll('//*[@id="readerarea"]/img/@data-src', TASK.PageLinks) end
		end
		for i = 0, TASK.PageLinks.Count - 1 do -- Bypass 'i0.wp.com' image CDN to ensure original images are loaded directly from host
			TASK.PageLinks[i] = TASK.PageLinks[i]:gsub("i%d.wp.com/", "")
			i = i + 1
		end
		return true
	else
		return false
	end
end

local AlphaList = '.ABCDEFGHIJKLMNOPQRSTUVWXYZ'
local dirpageskraw = {'list-manhwa', 'list-manhua', 'list-manga'}

function GetNameAndLink()
	if MODULE.ID == '421be2f0d918493e94f745c71090f359' then -- Mangafast
		local dirurl = MODULE.RootURL .. '/list-manga/'
		if not HTTP.GET(dirurl) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		local v for v in x.XPath('//div[@class="kan"]/a[1]').Get() do
			LINKS.Add(v.GetAttribute('href'))
			NAMES.Add(x.XPathString('h3', v))
		end
	elseif MODULE.ID == 'fb34a56c83f54b19b57a9a92070fe899' then -- FlameScan
		local dirurl = MODULE.RootURL .. '/series/list-mode/'
		if not HTTP.GET(dirurl) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		local v for v in x.XPath('//*[@class="blix"]//a').Get() do
			LINKS.Add(v.GetAttribute('href'):gsub("%/[0-9]+-", "/"))
			NAMES.Add(x.XPathString('normalize-space(.)', v))
		end
	elseif MODULE.ID == 'b8206e754d4541689c1d367f7e19fd64' then -- KomikCast
		local dirurl = MODULE.RootURL .. '/daftar-komik/?list'
		if not HTTP.GET(dirurl) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//*[@class="list-update"]//ul//a', LINKS, NAMES)
	elseif MODULE.ID == '5c06401129894099bb6fc59c08a878d4' then -- Ngomik
		local s, i, x
		if MODULE.CurrentDirectoryIndex == 0 then
			s = '0-9'
		else
			i = MODULE.CurrentDirectoryIndex + 1
			s = AlphaList:sub(i, i)
		end
		if not HTTP.GET(MODULE.RootURL .. '/all-komik/page/' .. (URL + 1) .. '/?show=' .. s) then return net_problem end
		x = CreateTXQuery(HTTP.Document)
		x.XPathHREFTitleAll('//div[@class="bsx"]/a', LINKS, NAMES)
		UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//div[@class="pagination"]/a[last()-1]')) or 1
	elseif MODULE.ID == 'ec1a1ad5301f414592f0ba0402024813' then -- Doujindesu
		local dirurl = MODULE.RootURL .. '/manga/page/' .. (URL + 1)
		if not HTTP.GET(dirurl) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFTitleAll('//div[@class="entries"]//a', LINKS, NAMES)
		UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//nav[@class="pagination"]//li[last()-1]/a')) or 1
	elseif MODULE.ID == 'abe29b9f5f9e4fff94068fe547a93cef' then -- Kraw
		if not HTTP.GET(MODULE.RootURL .. '/' .. dirpageskraw[MODULE.CurrentDirectoryIndex + 1] .. '/page/' .. (URL + 1)) then return net_problem end
		x = CreateTXQuery(HTTP.Document)
		x.XPathHREFTitleAll('//div[@class="bsx"]/a', LINKS, NAMES)
		UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//div[@class="pagination"]/a[last()-1]')) or 1
	else
		-- full text based list
		local dirs = {
			['49602ce189e844f49bfe78f7a1484dbe'] = '/manga-lists/', -- MangaKid
			['ca571825056b4850bd3693e4e1437997'] = '/daftar-komik-manga-bahasa-indonesia.html', -- Mangacan
			['fb5bd3aa549f4aefa112a8fe7547d2a9'] = '/manga-list/', -- MangaIndo
			['7a74b2abda1d4b329ee1d1fa58866c03'] = '/manga-list/', -- MaidMangaID
			['1f1ec10a248c4a4f838c80b3e27fc4c7'] = '/daftar-komik/?list', -- Sekaikomik
			['f68bb6ee00e442418c8c05eb00759ae1'] = '/daftar-manga/?list', -- BacaKomik
			['5af0f26f0d034fb2b42ee65d7e4188ab'] = '/daftar-komik/', -- Komiku
			['13c6434a0c2541b18abee83a2c72e8f5'] = '/daftar-komik/', -- MangaKane
			['c8e02b7aaac1412180db86374fc799a8'] = '/manga-list/?list', -- ManhwasNet
			['b5586745030a45bba05d0c360caa6d1a'] = '/manga/?list', -- KomikStation
			['009bf49bc17a4a2a8e1c79cce6867651'] = '/manga/?list', -- KomikIndo
			['56f905ea80e24c4f8bbc37e05de2ad9a'] = '/manga/?list', -- Mangakyo
			['0e45db2650604f74a0caeb7c1d69a749'] = '/manga/?list', -- MangaSWAT
			['41294a121062494489adfa601c442ef8'] = '/manga-list/?list', -- LegionAsia
			['b53534f8443e420ea088594c53a3ff39'] = '/series/list-mode', -- Manhwaland
			['ff17b64aa945403dae45706753235872'] = '/latest-update/?list', -- KomikNesia
			['5474e31b24ab4908a5258176d1f24f67'] = '/komik/list-mode/', -- ManhwaTaro
			['f794803973af4e5daab21683d4de873a'] = '/series/list-mode/', -- LuminousScans
			['edf6b037808442508a3aaeb1413699bf'] = '/daftar-komik/?list', -- KomikIndoID
			['b09c78407f8046bf94cb587541d4cb45'] = '/series/list-mode/' -- ManhwaIndo
		}
		local dirurl = '/manga/list-mode/'
		if dirs[MODULE.ID] ~= nil then
			dirurl = dirs[MODULE.ID]
		end
		local dirurl = MODULE.RootURL .. dirurl
		if not HTTP.GET(dirurl) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		
		x.XPathHREFAll('//*[@class="blix"]//a', LINKS, NAMES)
		if LINKS.Count == 0 then x.XPathHREFAll('//*[@class="daftarkomik"]//a', LINKS, NAMES) end
		if LINKS.Count == 0 then x.XPathHREFAll('//*[@class="jdlbar"]//a', LINKS, NAMES) end
		if LINKS.Count == 0 then x.XPathHREFAll('//*[@class="soralist"]//a', LINKS, NAMES) end
		if LINKS.Count == 0 then x.XPathHREFAll('//*[@id="a-z"]//h4/a', LINKS, NAMES) end
		if LINKS.Count == 0 then x.XPathHREFAll('//*[@class="manga-list"]/a', LINKS, NAMES) end
		if LINKS.Count == 0 then x.XPathHREFAll('//*[@class="ls4j"]//a', LINKS, NAMES) end
		if LINKS.Count == 0 then x.XPathHREFAll('//*[@class="listttl"]//a', LINKS, NAMES) end
		if LINKS.Count == 0 then x.XPathHREFAll('//*[@class="Manga" or @class="Manhwa"]/a', LINKS, NAMES) end
	end
	return no_error
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	return true
end

function reverseLinksAndChapters()
	local doNotReverse = {
		['4657d79e63dc4a9082a46b7981bde1b9'] = '', -- MangaBoruto
		['4420b10f54ea4d3d812318bdd10cd729'] = '' -- ElarcPage
	}
	if doNotReverse[MODULE.ID] == nil then
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
	end
	return true
end

function Init()
	local cat = 'Indonesian'
	local function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                    = id
		m.Name                  = name
		m.RootURL               = url
		m.Category              = cat
		m.OnGetInfo             = 'GetInfo'
		m.OnGetPageNumber       = 'GetPageNumber'
		m.OnGetNameAndLink      = 'GetNameAndLink'
		m.OnBeforeDownloadImage = 'BeforeDownloadImage'
		return m
	end
	AddWebsiteModule('5eb57a1843d8462dab0fdfd0efc1eca5', 'MangaShiro', 'https://mangashiro.co')
	AddWebsiteModule('b543e37b656e43ffb3faa034eee6c945', 'MangaKita', 'https://mangakita.net')
	AddWebsiteModule('b5586745030a45bba05d0c360caa6d1a', 'KomikStation', 'https://www.komikstation.co')
	AddWebsiteModule('49602ce189e844f49bfe78f7a1484dbe', 'MangaKid', 'https://mangakid.site')
	AddWebsiteModule('b8206e754d4541689c1d367f7e19fd64', 'KomikCast', 'https://komikcast.me')
	AddWebsiteModule('35e1b3ff5dbf428889d0f316c3d881e6', 'WestManga', 'https://westmanga.info')
	AddWebsiteModule('031f3cc0ae3346ad9b8c33d5377891e9', 'Kiryuu', 'https://kiryuu.id')
	AddWebsiteModule('009bf49bc17a4a2a8e1c79cce6867651', 'KomikIndo', 'https://komikindo.co')
	AddWebsiteModule('5af0f26f0d034fb2b42ee65d7e4188ab', 'Komiku', 'https://komiku.id')
	AddWebsiteModule('ca571825056b4850bd3693e4e1437997', 'Mangacan', 'http://www.mangacanblog.com')
	AddWebsiteModule('fb5bd3aa549f4aefa112a8fe7547d2a9', 'MangaIndo', 'https://mangaindo.web.id')
	AddWebsiteModule('6f8182f08d5444dbb5244ec882430db1', 'KomikMama', 'https://komikmama.net')
	AddWebsiteModule('7a74b2abda1d4b329ee1d1fa58866c03', 'MaidMangaID', 'https://www.maid.my.id')
	AddWebsiteModule('a70859360a2a474ba2abdb86bc48616c', 'KomikAV', 'https://komikav.com')
	AddWebsiteModule('435c514990664207beb4b6a5aaa9c83b', 'SekteDoujin', 'https://sektedoujin.club')
	m = AddWebsiteModule('5c06401129894099bb6fc59c08a878d4', 'Ngomik', 'https://ngomik.net')
	m.TotalDirectory = AlphaList:len()
	AddWebsiteModule('56f905ea80e24c4f8bbc37e05de2ad9a', 'Mangakyo', 'https://www.mangakyo.me')
	AddWebsiteModule('f68bb6ee00e442418c8c05eb00759ae1', 'BacaKomik', 'https://bacakomik.co')
	AddWebsiteModule('755ce08dc1a74f69b283cb45b7af56c1', 'Boosei', 'https://boosei.com')
	AddWebsiteModule('13c6434a0c2541b18abee83a2c72e8f5', 'MangaKane', 'https://mangakane.com')
	AddWebsiteModule('c69cbc947a6a42e194b2e097bba15047', 'MangaSusuBiz', 'https://mangasusu.biz')
	AddWebsiteModule('f8251e330c8044029d97dec382459eab', 'SheaManga', 'https://sheamanga.my.id')
	AddWebsiteModule('46dcfabe757140e7980ec34c65bdb30f', 'SekteKomik', 'https://sektekomik.com')
	AddWebsiteModule('1f1ec10a248c4a4f838c80b3e27fc4c7', 'SekaiKomik', 'https://www.sekaikomik.live')
	AddWebsiteModule('f9adee01635a4ff48fdff5164a65d6dd', 'Komiktap', 'https://komiktap.in')
	AddWebsiteModule('ec1a1ad5301f414592f0ba0402024813', 'Doujindesu', 'https://212.32.226.234')
	AddWebsiteModule('cbb62ba41ad6440a8d112c3c30edc6f5', 'KomikSave', 'https://komiksave.me')
	AddWebsiteModule('deb2a310668a40ebbbe3aaa45f78edc2', 'GuruKomik', 'https://gurukomik.com')
	AddWebsiteModule('adb6ae3e4d7c49fb89bb8d17bfbc9486', 'KlanKomik', 'https://klankomik.com')
	AddWebsiteModule('06b9c968ec8c4c89b7d28b7d461d84e3', 'Masterkomik', 'https://masterkomik.com')
	AddWebsiteModule('b53534f8443e420ea088594c53a3ff39', 'Manhwaland', 'https://manhwaland.me')
	AddWebsiteModule('ff17b64aa945403dae45706753235872', 'KomikNesia', 'https://komiknesia.com')
	AddWebsiteModule('489dff6a5f894b6a9c1eed46feeec72e', 'WordHero', 'https://wordhero.my.id')
	AddWebsiteModule('5474e31b24ab4908a5258176d1f24f67', 'ManhwaTaro', 'https://manhwataro.xyz')
	AddWebsiteModule('180a930232614f81816720cefeea7954', 'KoMBatch', 'https://kombatch.com')
	AddWebsiteModule('4657d79e63dc4a9082a46b7981bde1b9', 'MangaBoruto', 'https://mangaboruto.xyz')
	AddWebsiteModule('edf6b037808442508a3aaeb1413699bf', 'KomikIndoID', 'https://komikindo.id')
	AddWebsiteModule('55cefd61a4144b56874108a666857ff0', 'WorldRomanceTranslation', 'https://wrt.my.id')
	AddWebsiteModule('b09c78407f8046bf94cb587541d4cb45', 'ManhwaIndo', 'https://manhwaindo.id')

	cat = 'Raw'
	AddWebsiteModule('21b0dfcb262d4ae28520679165282666', 'Rawkuma', 'https://rawkuma.com')
	AddWebsiteModule('5c3737434b964df7b76e5b27c2ad442c', 'MangasRaw', 'https://mangas-raw.com')
	AddWebsiteModule('452f504992704f11add27f30f47e388a', 'J9jp', 'https://j9jp.com')

	cat = 'Arabic'
	AddWebsiteModule('0e45db2650604f74a0caeb7c1d69a749', 'SWATManga', 'https://swatmanga.me')
	
	cat = 'English-Scanlation'
	AddWebsiteModule('7103ae6839ea46ec80cdfc2c4b37c803', 'AsuraScans', 'https://asura.gg')
	AddWebsiteModule('fb34a56c83f54b19b57a9a92070fe899', 'FlameScans', 'https://flamescans.org')
	AddWebsiteModule('a3455b2005f8457f821acc93c2e821ab', 'PMScans', 'https://reader.pmscans.com')
	AddWebsiteModule('568f2731188d4f058c5542e89603a030', 'AzureManga', 'https://azuremanga.com')
	AddWebsiteModule('f8c92a9a83f64deebee5ec58b7b15bdf', 'xCaliBRScans', 'https://xcalibrscans.com')
	AddWebsiteModule('f794803973af4e5daab21683d4de873a', 'LuminousScans', 'https://luminousscans.com')
	AddWebsiteModule('86588503fd9e4277802c998cbccbc983', 'AlphaScans', 'https://alpha-scans.org')
	AddWebsiteModule('275b85bdaafb47fdbc40f51d2bea99e8', 'TheApolloTeam', 'https://theapollo.team')
	AddWebsiteModule('15fc68c57ce141f497b872af157d72ac', 'ShimadaScans', 'https://shimadascans.com')
	AddWebsiteModule('f291e782dab54867a26a161934277177', 'ShiniScan', 'https://shiniscan.com')
	AddWebsiteModule('3593adad980d454abe489c42e7158032', 'RealmScans', 'https://realmscans.com')
	AddWebsiteModule('a51ebfb8979045d589cd867c48a095c0', 'ManhwaFreak', 'https://manhwafreak.com')
	AddWebsiteModule('f53627f1cad44232ac9dbc02a613aeb5', 'MajesticScans', 'https://majesticscans.com')
	AddWebsiteModule('f01040ee781d4ae1929031419b97d2e0', 'VoidScans', 'https://void-scans.com')
	AddWebsiteModule('752cda75b5e24f6ab4256079c564eba2', 'OmegaScans', 'https://omegascans.org')
	AddWebsiteModule('e8ef6a7e02bd405785d504b60d5ee55e', 'ImperfectComic', 'https://imperfectcomic.org')
	AddWebsiteModule('3b9b01c5fde14e00a540dda2c60ada36', 'NightScans', 'https://nightscans.org')

	cat = 'Spanish'
	AddWebsiteModule('41294a121062494489adfa601c442ef8', 'LegionAsia', 'https://legionasia.com')
	AddWebsiteModule('363066add92f4043b39d2009b442ab32', 'PhoenixFansub', 'https://phoenixfansub.com')
	AddWebsiteModule('9f756fcbfa114ea4a9abb578004edf31', 'SkyMangas', 'https://skymangas.com')
	AddWebsiteModule('c8e02b7aaac1412180db86374fc799a8', 'ManhwasNet', 'https://manhwas.net')
	AddWebsiteModule('61e0a07a01ea45cf8909a2cfc5442659', 'SenpaiEdiciones', 'https://senpaiediciones.com')
	AddWebsiteModule('ad851ecfe6824a01a30625b58d105de8', 'Ikifang', 'https://ikimei.com')
	AddWebsiteModule('b015f20e82d44b268928124cc2c1a4b0', 'MartialManga', 'https://martialmanga.com')
	AddWebsiteModule('7872813350a44e11a301816c2911c87a', 'SDLScans', 'https://sdlscans.com')
	AddWebsiteModule('7872813350a45e11a301816c2911c87a', 'FusionScanlation', 'https://fusionscanlation.com')
	AddWebsiteModule('529038945bc84174b4be556b922bfb4a', 'OniScans', 'https://www.oniscans.com')
	AddWebsiteModule('529038924bc84174b4be556b922bfb4a', 'ShadowMangas', 'https://shadowmangas.com')
	AddWebsiteModule('a08tter98y97k9er008971c0c1b55705', 'RaikiScan', 'https://raikiscan.com')

	cat = 'English'
	AddWebsiteModule('421be2f0d918493e94f745c71090f359', 'Mangafast', 'https://mangafast.net')
	AddWebsiteModule('b5512eeeebbe4aa1a9194f58e8401ca2', 'KumaScans', 'https://kumascans.com')

	cat = 'H-Sites'
	m = AddWebsiteModule('abe29b9f5f9e4fff94068fe547a93cef', 'Kraw', 'https://kraw.org')
	m.TotalDirectory = #dirpageskraw

	cat = 'Webcomics'
	AddWebsiteModule('4420b10f54ea4d3d812318bdd10cd729', 'ElarcPage', 'https://elarcpage.com')
end
