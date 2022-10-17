function Init()
	local function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                         = id
		m.Name                       = name
		m.RootURL                    = url
		m.Category                   = 'English'
		m.OnGetNameAndLink           = 'GetNameAndLink'
		m.OnGetInfo                  = 'GetInfo'
		m.OnGetPageNumber            = 'GetPageNumber'
		m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
		m.OnBeforeDownloadImage      = 'BeforeDownloadImage'
	end
	AddWebsiteModule('74674292e13c496699b8c5e4efd4b583', 'MangaKakalot', 'https://mangakakalot.com')
	AddWebsiteModule('fa8bb4d1ceea4c8fa0e98c00755f95d4', 'Manganato', 'https://manganato.com')
	AddWebsiteModule('fa8bb4d1ceea4c8fa0e98c00755f95d4', 'Manganato', 'https://chapmanganato.com')
	AddWebsiteModule('ed4175a390e74aedbe4b4f622f3767c6', 'MangaKakalots', 'https://mangakakalots.com')
	AddWebsiteModule('2234588abb544fc6a279c7811f2a9733', 'MangaBat', 'https://m.mangabat.com')
	AddWebsiteModule('0625a58a9af94a99a8a5cff216ce27f0', 'ReadMangaBat', 'https://readmangabat.com')
end

function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if u:find('https://manganato.com') then
		u = u:gsub('https://manganato.com', 'https://chapmanganato.com')
	end
	if HTTP.GET(u) then
		local s = string.match(HTTP.Document.ToString(), 'window%.location%.assign%([\'"]([^\'"]+)')
		if s ~= nil then u = s;
			if not HTTP.GET(u) then
				return net_problem
			end
		end
		MANGAINFO.URL = u
		local id = MODULE.ID
		local x = CreateTXQuery(HTTP.Document)
		if (id == '74674292e13c496699b8c5e4efd4b583')	-- mangakakalot
			or (id == 'ed4175a390e74aedbe4b4f622f3767c6')	-- mangakakalots
		then
			MANGAINFO.Title     = x.XPathString('//meta[@property="og:title"]/@content'):gsub(' Manga %- Mangakakalot.*$','')
			MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="manga-info-pic"]/img/@src'))
			MANGAINFO.Authors   = x.XPathStringAll('//ul[@class="manga-info-text"]/li[contains(., "Author")]/a')
			MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="manga-info-text"]/li[contains(., "Genre")]/a')
			MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="manga-info-text"]/li[contains(., "Status")]'))
			MANGAINFO.Summary   = x.XPathStringAll('//div[@id="noidungm"]/text()', '')
			x.XPathHREFAll('//div[@class="chapter-list"]/div[@class="row"]/span/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		else
			MANGAINFO.Title     = x.XPathString('//h1')
			MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//span[@class="info-image"]/img/@src'))
			MANGAINFO.Authors   = x.XPathStringAll('//td[contains(., "Author(s)")]/following-sibling::td/a')
			MANGAINFO.Genres    = x.XPathStringAll('//td[contains(., "Genres")]/following-sibling::td/a')
			MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//td[contains(., "Status")]/following-sibling::td'))
			MANGAINFO.Summary   = x.XPathStringAll('//div[@class="panel-story-info-description"]/text()', '')
			x.XPathHREFAll('//ul[@class="row-content-chapter"]/li/a[contains(@class, "chapter-name")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	TASK.PageNumber=0
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if u:find('https://manganato.com') then
		u = u:gsub('https://manganato.com', 'https://chapmanganato.com')
	end
	HTTP.Cookies.Values['content_server'] = 'server2'
	if HTTP.GET(u) then
		local s = string.match(HTTP.Document.ToString(), 'window%.location%.assign%([\'"]([^\'"]+)')
		if s ~= nil then
			local function splitURL(url) return url:match('(https://[^/]+)') or '' end
			local h, p = splitURL(s), splitURL(u)
			if not HTTP.GET(h .. p) then return false end
		end
		local x = CreateTXQuery(HTTP.Document)
		x.XPathStringAll('//div[@id="vungdoc"]/img[@title]/@src', TASK.PageLinks)
		if TASK.PageLinks.Count == 0 then
			x.XPathStringAll('//div[@class="vung_doc"]/img[@title]/@src', TASK.PageLinks)
			if TASK.PageLinks.Count == 0 then
				x.XPathStringAll('//div[@class="container-chapter-reader"]/img[@title]/@src', TASK.PageLinks)
				if TASK.PageLinks.Count == 0 then
					x.XPathStringAll('//div[@id="vungdoc"]/img[@title]/@data-src', TASK.PageLinks)
				end
			end
		end
		return true
	else
		return false
	end
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])
	return true
end

function GetDirectoryPageNumber()
	if (MODULE.ID == '74674292e13c496699b8c5e4efd4b583')	-- mangakakalot
		or (MODULE.ID == 'ed4175a390e74aedbe4b4f622f3767c6')	-- mangakakalots
	then
		if HTTP.GET(MODULE.RootURL .. '/manga_list?type=newest&category=all&state=all&page=' .. '1') then
			PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//a[contains(@class, "page_last")]/@href'):match('page=(%d+)'))
			return no_error
		else
			return net_problem
		end
	elseif (MODULE.ID == '2234588abb544fc6a279c7811f2a9733') -- mangabat
			or (MODULE.ID == '0625a58a9af94a99a8a5cff216ce27f0') -- readmangabat
	then
		if HTTP.GET(MODULE.RootURL .. '/manga-list-all?type=newest') then
			PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//a[contains(@class, "page-last")]/@href'):match('.-//.-/.-/(%d+)'))
			return no_error
		else
			return net_problem
		end
	else
		if HTTP.GET(MODULE.RootURL .. '/genre-all/') then
			PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//a[contains(@class, "page-last")]/@href'):match('.-//.-/.-/(%d+)'))
			return no_error
		else
			return net_problem
		end
	end
end

function GetNameAndLink()
	local id = MODULE.ID
	if (id == '74674292e13c496699b8c5e4efd4b583')	-- mangakakalot
		or (id == 'ed4175a390e74aedbe4b4f622f3767c6')	-- mangakakalots
	then
		if HTTP.GET(MODULE.RootURL .. '/manga_list?type=newest&category=all&state=all&page=' .. (URL + 1)) then
			local x = CreateTXQuery(HTTP.Document)
			x.XPathHREFAll('//div[@class="truyen-list"]/div[@class="list-truyen-item-wrap"]/h3/a', LINKS, NAMES)
			return no_error
		else
			return net_problem
		end
	elseif (MODULE.ID == '2234588abb544fc6a279c7811f2a9733') -- mangabat
			or (MODULE.ID == '0625a58a9af94a99a8a5cff216ce27f0') -- readmangabat
	then
		if HTTP.GET(MODULE.RootURL .. '/manga-list-all/' .. (URL + 1)) then
			local x = CreateTXQuery(HTTP.Document)
			x.XPathHREFTitleAll('//div[@class="panel-list-story"]/div[@class="list-story-item"]/a', LINKS, NAMES)
			return no_error
		else
			return net_problem
		end
	else
		local url = MODULE.RootURL .. '/genre-all/'
		if URL ~= '0' then url = url .. (URL + 1) end
		if HTTP.GET(url) then
			local x = CreateTXQuery(HTTP.Document)
			x.XPathHREFAll('//div[@class="panel-content-genres"]//div[@class="genres-item-info"]/h3/a', LINKS, NAMES)
			return no_error
		else
			return net_problem
		end
	end
end
