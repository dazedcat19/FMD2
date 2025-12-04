----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '019addd69b6276a7845e96f33cd6795b'
	m.Name                     = 'MandaloAsiNoma'
	m.RootURL                  = 'https://mandaloasinoma.com'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()


	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()


	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="md:col-span-2 space-y-4"]/h1')
	MANGAINFO.AltTitles	= x.XPathString('//div[@class="md:col-span-2 space-y-4"]/h2')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Authors   = x.XPathString('//div[./span="Autor:"]/span[2]')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[./span="Géneros:"]/div/span, //div[./span="Tipo:"]/span[2])')
	MANGAINFO.Summary   = x.XPathString('//p[contains(@class, "text-zinc-900")]')

	MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

    local body = HTTP.Document.ToString():gsub('\\"', '"')

    for url in body:gmatch('"src":"(http.-)"') do
		if not url:match('/blob%.') then
			TASK.PageLinks.Add(url)
		end
    end

    return true
end
