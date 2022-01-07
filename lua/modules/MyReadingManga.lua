function Init()
	local m = NewWebsiteModule()
	m.ID                         = '84451c35b7764bb5a5e3dd8692e84682'
	m.Name                       = 'MyReadingManga'
	m.RootURL                    = 'https://myreadingmanga.info'
	m.Category                   = 'H-Sites'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnBeforeDownloadImage      = 'BeforeDownloadImage'
	m.SortedList                 = true
end

function GetInfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title   = x.XPathString('//h1[@class="entry-title"]')
		MANGAINFO.Authors = Trim(x.XPathString('//*[contains(@class,"entry-content")]/p[starts-with(.,"Author")]/substring-after(.,":")'))
		MANGAINFO.Genres  = x.XPathString('//header[@class="entry-header"]/string-join(./p[position()>1]//a,", ")')
		MANGAINFO.Status  = MangaInfoStatusIfPos(x.XPathString('//*[@class="entry-terms" and contains(., "Status")]/a'))
		MANGAINFO.Summary = x.XPathString('//*[@class="info-class"]/following-sibling::p/text()')
		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		local v; for v in x.XPath('//*[contains(@class,"entry-pagination")]/a').Get() do
			if string.match(v.ToString(), '^Next') == nil then
				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(MANGAINFO.Title .. ' - ' .. v.ToString())
			end
		end
		if MANGAINFO.ChapterNames.Count > 1 then
			MANGAINFO.ChapterNames[0] = MANGAINFO.ChapterNames[0] .. ' - 1'
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x=CreateTXQuery(HTTP.Document)
		x.XPathStringAll('//*[contains(@class,"entry-content")]//img/@data-lazy-src', TASK.PageLinks)
		if TASK.PageLinks.Count == 0 then
			x.XPathStringAll('//div[@class="separator" and @style]//img/@data-lazy-src', TASK.PageLinks)
		end
	else
		return false
	end
	return true
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/page/' .. (URL + 1) .. '/') then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//h2[@class="entry-title"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL) then
		local x = CreateTXQuery(HTTP.Document)
		PAGENUMBER = tonumber(x.XPathString('//*[contains(@class,"archive-pagination")]/ul/li[last()-1]')) or 1
		return no_error
	else
		return net_problem
	end
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = ' ' .. MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])
	return true
end
