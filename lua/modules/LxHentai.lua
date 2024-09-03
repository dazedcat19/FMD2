----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'a25c1b1030ee41e49018e1811bdf7085'
	m.Name                     = 'LxHentai'
	m.RootURL                  = 'https://lxmanga.life'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/danh-sach?sort=-created_at&filter%5Bstatus%5D=2,1&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="flex gap-2"]/a[last()]/@href'):match('page=(%d+)$')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="p-2 w-full truncate"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('(//span[contains(@class, "text-lg ml-1")])[1]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="rounded-lg cover"]/@style'):match("background%-image:%surl%('(.-)'%)")
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="grow"]/div[contains(., "Tác giả")]//a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="grow"]/div[contains(., "Thể loại")]//a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="grow"]/div[contains(., "Tình trạng")]//a'), 'Đang tiến hành', 'Đã hoàn thành')
	MANGAINFO.Summary   = x.XPathString('//div[contains(., "Tóm tắt")]/p[3]')

	for v in x.XPath('//ul[@class="overflow-y-auto overflow-x-hidden"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('li/div[1]/span', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="lazy max-w-full my-0 mx-auto"]/@src', TASK.PageLinks)

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end