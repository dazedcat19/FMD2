----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'English'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.TotalDirectory           = AlphaList:len()
	end
	AddWebsiteModule('8212f7c50ebe478bb344d16e8ab20adc', 'MangaHereToday', 'http://mangahere.today')
	AddWebsiteModule('d1958f8b85cb494abe69deb151d1a89d', 'MangaNelos', 'http://manganelos.com')

	cat = 'Spanish'
	AddWebsiteModule('c67d163c51b24bc498e777e2b0d810d2', 'LeerCapitulo', 'https://www.leercapitulo.com')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

AlphaList = '#abcdefghijklmnopqrstuvwxyz'
DirectoryPagination = '/alpha/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local s, i, x
	if MODULE.CurrentDirectoryIndex == 0 then
		s = '9'
	else
		i = MODULE.CurrentDirectoryIndex + 1
		s = AlphaList:sub(i, i)
	end
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. s .. '/?page=' .. (URL + 1)) then return net_problem end
	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFTitleAll('//div[@class="cate-manga"]//div[@class="media-body"]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//div[@class="pagination"]//li[last()-1]')) or 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="title-manga"]'):gsub(' Manga$', '')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "cover-detail")]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//p[@class="description-update"]/span[contains(., "Author")]/following-sibling::text()[1]')
	MANGAINFO.Artists   = x.XPathString('//p[@class="description-update"]/span[contains(., "Artist")]/following-sibling::text()[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//p[@class="description-update"]/span[contains(., "Genre")]/following-sibling::a/text()')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[@class="description-update"]/span[contains(., "Status")]/following-sibling::text()[1]'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="manga-content"]/p')

	x.XPathHREFAll('//div[@class="total-chapter"]//div[@class="chapter-list"]//h4/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local json = x.XPathString('//p[@id="arraydata"]')
	for i in json:gmatch('(.-),') do
		TASK.PageLinks.Add(i:gsub("cdn.statically.io/img/", ""))
	end

	return no_error
end
