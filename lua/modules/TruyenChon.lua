function Init()
	local cat = 'Vietnamese'
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = cat
		m.SortedList               = true
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	end
	AddWebsiteModule('ef7f922bd45f4f9d9c559a55f987004d', 'TruyenChon', 'http://truyenchon.com')

	cat = 'English'
	AddWebsiteModule('d2f24dec90e841b1aab4bea145ffb638', 'MangaToro', 'https://www.mangatoro.com')
end

local dirurl = {
	['ef7f922bd45f4f9d9c559a55f987004d'] = '/the-loai?status=-1&sort=15&page=%s', -- truyenchon
	['d2f24dec90e841b1aab4bea145ffb638'] = '/genres?status=-1&sort=15&page=%s' -- mangatoro
}

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//h1[@class="title-detail"]')
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[contains(@class, "col-image")]/img/@src'))
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//li[contains(@class, "status")]/p[2]'), 'Đang Cập Nhật', 'Hoàn Thành')
		if MANGAINFO.Status == '' then
			MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//p[contains(., "status")]/following-sibling::p'))
		end
		MANGAINFO.Authors   = x.XPathString('//li[contains(@class, "author")]/p[2]')
		if MANGAINFO.Authors == '' then
			MANGAINFO.Authors = x.XPathStringAll('//p[contains(., "Author(s)")]/following-sibling::p/a')
		end
		MANGAINFO.Artists   = x.XPathString('//h4[starts-with(./label,"Artista")]/substring-after(.,":")')
		MANGAINFO.Genres    = x.XPathStringAll('//li[contains(@class, "kind")]/p[2]/a')
		MANGAINFO.Summary   = x.XPathString('//div[@class="detail-content"]/div')

		x.XPathHREFAll('//div[@class="list-chapter"]//ul/li/div[contains(@class, "chapter")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="page-chapter"]/img/@data-src', TASK.PageLinks)
		return false
	end
	return true
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = ' ' .. MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])
	return true
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. dirurl[MODULE.ID]:format((URL + 1))) then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="item"]//h3/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. dirurl[MODULE.ID]:format('1')) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()]/a/@href'):match('&page=(%d+)')) or 1
		return no_error
	else
		return net_problem
	end
end
