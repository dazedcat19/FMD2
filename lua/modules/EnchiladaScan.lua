----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'dfff1035a7e647729abe8c36fe6ff271'
	m.Name                     = 'EnchiladaScan'
	m.RootURL                  = 'https://enchiladascan.github.io'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

--local Template = require 'templates.Madara'
-- XPathTokenAuthors = 'Author(s)'
-- XPathTokenArtists = 'Artist(s)'
-- XPathTokenGenres  = 'Genre(s)'
-- XPathTokenStatus  = 'Status'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()


	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()

	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	
	MANGAINFO.Title 	= x.XPathStringAll('//h1[@class="manga-title"]/text()[normalize-space()]')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="manga-cover"]/img/@src'))
	MANGAINFO.Summary   = x.XPathString('//p[@class="manga-sinopsis"]/text()[normalize-space()]')

    for v in x.XPath('//ul[@id="chaptersList"]/li/a').Get() do
        local cap_number = x.XPathString('.//span[@class="cap-number"]', v)
        local cap_title  = x.XPathString('.//span[@class="cap-title"]', v)
        cap_title        = cap_number .. (cap_title ~= '' and ' - ' .. cap_title or '')

        MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
        MANGAINFO.ChapterNames.Add(cap_title)
    end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
    local u = MODULE.RootURL .. '/enchiladaweb/assets/mangas/' .. URL:match('/[^/]+/([^/]+/cap%d+)/') .. '/images.json'

    if not HTTP.GET(u) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)

    for v in x.XPath('json(*)()').Get() do
        TASK.PageLinks.Add(v.ToString())
    end

    return no_error
end
