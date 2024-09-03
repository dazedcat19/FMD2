----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'b7b246f0e759481fab638b2717a2b5db'
	m.Name                     = 'ManhuaVN'
	m.RootURL                  = 'https://manhuavn.top'
	m.Category                 = 'Vietnamese'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/danhsach/P1/index.html?status=0&sort=0'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local next_url, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	while true do
		x.XPathHREFAll('//li[@class="story_item"]/a[@class="story_title"]', LINKS, NAMES)
		next_url = x.XPathString('//ul[@class="pager"]/li[last()]/a/@href')
		if next_url == '#' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('P(%d+)/') or ''))
		if HTTP.GET(MODULE.RootURL .. next_url) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="wrap-content-image"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//li[contains(., "Tác Giả")]//a')
	MANGAINFO.Genres    = x.XPathStringAll('(//li[@class="clearfix"])[1]//a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//li[contains(., "Tình Trạng")]//span'), 'Đang tiến hành', 'Hoàn thành')
	MANGAINFO.Summary   = x.XPathString('//li[contains(., "Giới Thiệu")]/p')

	x.XPathHREFTitleAll('//ul[@class="lst-chapter"]//a[not(i)]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="page-chapter"]/img/@data-original', TASK.PageLinks)

	return no_error
end