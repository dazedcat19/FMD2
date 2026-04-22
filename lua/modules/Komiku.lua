----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5af0f26f0d034fb2b42ee65d7e4188ab'
	m.Name                     = 'Komiku'
	m.RootURL                  = 'https://komiku.org'
	m.Category                 = 'Indonesian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/daftar-komik/?halaman='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="pagination"]/a[last()-1]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//article[@class="manga-card"]//h4/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@id="Judul"]//span[@itemprop="name"]')
	MANGAINFO.AltTitles = x.XPathString('//div[@id="Judul"]/span')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="ims"]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//tr[td="Author:"]/td[2]')
	MANGAINFO.Genres    = x.XPathStringAll('(//ul[@class="genre"]//a, //tr[td="Tipe:"]//strong)')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//tr[td="Status:"]/td[2]'), 'Ongoing|Berjalan', 'Completed|End')
	MANGAINFO.Summary   = x.XPathString('//p[@class="desc"]')

	for v in x.XPath('//td[@class="judulseries"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span/b', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="Baca_Komik"]/img/@src', TASK.PageLinks)

	return true
end