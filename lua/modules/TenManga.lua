local AlphaList = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'
local DirectoryPagination = '/category/'

function GetNameAndLink()
	local s, i, x
	if MODULE.CurrentDirectoryIndex == 0 then
		s = '0-9'
	else
		i = MODULE.CurrentDirectoryIndex + 1
		s = AlphaList:sub(i, i)
	end
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. s .. '_latest_' .. (URL + 1) .. '.html') then return net_problem end
	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFTitleAll('//*[@class="book-right-td"]/a[1]', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//*[@class="page-all-count"]'):match('(%d+) pages')) or 1

	return no_error
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
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
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
		TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[contains(@class, "manga_pic")]/@src')
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
	m.TotalDirectory             = AlphaList:len()
end
