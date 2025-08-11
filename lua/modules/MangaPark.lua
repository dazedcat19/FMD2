----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, url, cat)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = 'MangaPark'
		m.RootURL                  = url
		m.Category                 = cat
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.SortedList               = true
	end
	AddWebsiteModule('32b25f04931b414a803286a6f8cabd8d', 'https://mangapark.net', 'English')
	AddWebsiteModule('09d49c7760904bc2817dbc80757de44a', 'https://mangapark.me')
	AddWebsiteModule('273268fd7b1a42b38e50c010a630bfad', 'https://mangapark.com')
	AddWebsiteModule('2784b193e1794dcdac1f68eb59e9ad0c', 'https://mangapark.org')
	AddWebsiteModule('885424a90b764a0894dad0d0a1c4e545', 'https://mangapark.io')
	AddWebsiteModule('b14a022f3c4e4a72b47cb89345adf9c4', 'https://mangapark.to')
	AddWebsiteModule('44f8204959644aeb98d14f66f8f7d817', 'https://comicpark.org')
	AddWebsiteModule('d197cecff2f443debdc925d567c345e5', 'https://comicpark.to')
	AddWebsiteModule('4dd7f91c86be48b38284e4c47e0f3c7a', 'https://readpark.org')
	AddWebsiteModule('1ab8d1a539e445bc8e5c0f533875ee9f', 'https://readpark.net')
	AddWebsiteModule('15994152f69b4b8188cc93e8b13d4f34', 'https://parkmanga.com')
	AddWebsiteModule('600f0a75aada4b68b11efd98cc2f9c4a', 'https://parkmanga.net')
	AddWebsiteModule('cbf375bed7fa423ead7cc2764b2ff514', 'https://parkmanga.org')
	AddWebsiteModule('15b0c0c7dbe84382928e1ea2ccae076b', 'https://mpark.to')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/apo/'
local DirectoryPagination = '/search?sortby=field_create&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[contains(@class, "flex items-center flex-wrap")]/a[last()]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//h3[@class="font-bold space-x-1"]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('span', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"{ get_comicNode( id: ' .. URL:match('(%d+)') .. ' ) { data { name altNames urlCoverOri authors artists genres originalStatus uploadStatus summary } } }"}' 
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('json(*).data.get_comicNode.data')
	MANGAINFO.Title     = x.XPathString('name', json)
	MANGAINFO.AltTitles = x.XPathString('string-join(altNames?*, ", ")', json)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('urlCoverOri', json))
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*, ", ")', json)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*, ", ")', json)
	MANGAINFO.Genres    = x.XPathString('string-join(genres?*, ", ")', json):gsub("_", " "):gsub("(%l)(%w*)", function(first, rest) return first:upper() .. rest end)
	MANGAINFO.Summary   = x.XPathString('summary', json)

	local status = x.XPathString('uploadStatus', json)
	if status == 'null' then status = x.XPathString('originalStatus', json) end
	MANGAINFO.Status = MangaInfoStatusIfPos(status)

	HTTP.Reset()
	local s = '{"query":"{ get_comicChapterList( comicId: ' .. URL:match('(%d+)') .. ' ) { data { id dname title } } }"}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.get_comicChapterList().data').Get() do
		local chapter = v.GetProperty('dname').ToString()
		local title = v.GetProperty('title').ToString()
		title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

		MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(chapter .. title)
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"{ get_chapterNode( id: ' .. URL:match('(%d+)') .. ' ) { data { imageFile { urlList } } } }"}' 
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.get_chapterNode.data.imageFile.urlList()', TASK.PageLinks)

	return no_error
end