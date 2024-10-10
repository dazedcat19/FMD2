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
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.SortedList               = true
	end
	AddWebsiteModule('32b25f04931b414a803286a6f8cabd8d', 'MangaParkNet', 'https://mangapark.net')
	AddWebsiteModule('09d49c7760904bc2817dbc80757de44a', 'MangaParkMe', 'https://mangapark.me')
	AddWebsiteModule('273268fd7b1a42b38e50c010a630bfad', 'MangaParkCom', 'https://mangapark.com')
	AddWebsiteModule('2784b193e1794dcdac1f68eb59e9ad0c', 'MangaParkOrg', 'https://mangapark.org')
	AddWebsiteModule('885424a90b764a0894dad0d0a1c4e545', 'MangaParkIo', 'https://mangapark.io')
	AddWebsiteModule('b14a022f3c4e4a72b47cb89345adf9c4', 'MangaParkTo', 'https://mangapark.to')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = '/apo/'
DirectoryPagination = '/search?sortby=field_create&page='

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
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//h3[@class="font-bold space-x-1"]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('span', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local crypto = require 'fmd.crypto'
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"{ get_comicNode( id: ' .. URL:match('(%d+)') .. ' ) { data { id name artists authors genres originalStatus uploadStatus summary urlCoverOri } } }"}' 
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
	MANGAINFO.Title     = x.XPathString('json(*).data.get_comicNode.data.name')
	MANGAINFO.CoverLink = x.XPathString('json(*).data.get_comicNode.data.urlCoverOri')
	MANGAINFO.Authors   = x.XPathStringAll('json(*).data.get_comicNode.data.authors()')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).data.get_comicNode.data.artists()')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).data.get_comicNode.data.genres()'):gsub("_", " ")
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).data.get_comicNode.data.originalStatus'), 'ongoing|hiatus|pending', 'completed|cancelled')
	MANGAINFO.Summary   = x.XPathString('json(*).data.get_comicNode.data.summary')

	HTTP.Reset()
	s = '{"query":"{ get_comicChapterList( comicId: ' .. URL:match('(%d+)') .. ' ) { data { id dname title } } }"}'
	HTTP.MimeType = 'application/json'
	if HTTP.POST(u, s) then
		x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).data.get_comicChapterList()').Get() do
			chapter = x.XPathString('data/dname', v)
			title = x.XPathString('data/title', v)

			title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

			MANGAINFO.ChapterLinks.Add(x.XPathString('data/id', v))
			MANGAINFO.ChapterNames.Add(chapter .. title)
		end
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