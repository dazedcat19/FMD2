----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '29826228ef9e4d1bad17f4c22e8d9951'
	m.Name                     = 'MangaKatana'
	m.RootURL                  = 'https://mangakatana.com'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['svrpref'] = 'Server preference:',
			['server'] = 'Server 1\nServer 2\nServer 3'
		},
		['id_ID'] = {
			['svrpref'] = 'Peladen pilihan:',
			['server'] = 'Peladen 1\Peladen 2\Peladen 3'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionComboBox('svrpref', lang:get('svrpref'), lang:get('server'), 0)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/manga/page/%s?filter=1&order=new'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination:format(1)

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="uk-pagination"]/li[last()-1]/a/@href'):match('/(%d+)?')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination:format((URL + 1))

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@id="book_list"]//h3[@class="title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="alt_name"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="cover"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//ul[@class="meta d-table"]//a[@class="author"]')
	MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="meta d-table"]//div[@class="genres"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="meta d-table"]/li[(./div="Status:")]'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="summary"]/p')

	x.XPathHREFAll('//div[@class="chapters"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local x = nil
	local svrpref = {'', '?sv=mk', '?sv=3'}
	local sel_svrpref = (MODULE.GetOption('svrpref') or 0) + 1
	local u = MaybeFillHost(MODULE.RootURL, URL) .. svrpref[sel_svrpref]

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('var thzq=', ';function', x.XPathString('//script[contains(., "thzq")]')))
	x.XPathStringAll('json(*)()', TASK.PageLinks)

	return true
end