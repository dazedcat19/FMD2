local alphalist = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'

function GetNameAndLink()
	local s, i, j, x, v
	print('here')
	if MODULE.CurrentDirectoryIndex == 0 then
		s = '0-9'
	else
		i = MODULE.CurrentDirectoryIndex + 1
		s = alphalist:sub(i, i)
	end
	print(MODULE.RootURL .. '/category/' .. s .. '_latest_' .. (URL + 1) .. '.html')
	if HTTP.GET(MODULE.RootURL .. '/category/' .. s .. '_latest_' .. (URL + 1) .. '.html') then
		i = 1
		x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('//*[@class="page-all-count"]').Get() do
			j = tonumber(v.ToString()) or 1
			if j > i then i = j end
		end
		UPDATELIST.CurrentDirectoryPageNumber = i
		x.XPathHREFTitleAll('//*[@class="book-right-td"]/a', LINKS, NAMES)
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)

		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[contains(@class, "bk-cover")]//img/@lazy_url'))
		MANGAINFO.Title     = x.XPathString('//div[@class="bk-name"]')
		MANGAINFO.Authors   = x.XPathString('//div[./div="Author(s):"]/div[@class="attr-val"]')
		MANGAINFO.Genres    = x.XPathStringAll('//div[contains(@class, "bk-info-tags")]/a')
		MANGAINFO.Summary   = x.XPathString('//div[contains(@class, "bk-info-summary")]/div')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[contains(@class, "bk-status")]/a'), 'Ongoing', 'Completed')

		local chapters = x.XPath('//div[@class="chp-item"]')
		for ic = 1, chapters.Count do
			MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', chapters.Get(ic)))
			MANGAINFO.ChapterNames.Add(x.XPathString('.//td[@class="chp-idx"]/text()', chapters.Get(ic)))
		end
		MANGAINFO.ChapterLinks.Reverse()
		MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		TASK.PageNumber = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@option_name="page_head"]/count(./div)')) or 0
		return true
	else
		return false
	end
end

function GetImageURL()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL):gsub('/+$', '-' .. (WORKID + 1))) then
		local x = CreateTXQuery(HTTP.Document)
		TASK.PageLinks[WORKID] = x.XPathString('//img[contains(@class, "manga_pic")]/@src')
		return true
	else
		return false
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID                         = '05ebc869b7e0466690041551612fee1c'
	m.Name                       = 'TenManga'
	m.RootURL                    = 'https://www.tenmanga.com'
	m.Category                   = 'English'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.OnGetImageURL              = 'GetImageURL'
	m.TotalDirectory             = alphalist:len()
end
