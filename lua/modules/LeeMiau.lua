----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'f3098381220841f8b13928e682de4d7e'
	m.Name                     = 'LectorMiau'
	m.RootURL                  = 'https://leemiau.com'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
XPathTokenStatus = 'Estado'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
    local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
    
    MANGAINFO.Title     = x.XPathString('//h1[contains(@class,"entry-title")]')
    MANGAINFO.Summary   = x.XPathString('//p[contains(@class,"lm4-summary-short")]')
    MANGAINFO.CoverLink = x.XPathString('//div[@itemprop="image"]//img/@src')
   
    -- Estado
    local status = x.XPathString('//span[contains(@class,"lm4-poster-status")]')
    MANGAINFO.Status = MangaInfoStatusIfPos(status, 'Publicando|Ongoing|Publishing', 'Completado|Completed|Finished', 'Hiatus', 'Dropped')
    
    -- Capítulos
    for v in x.XPath('//div[@id="chapterlist"]//a[contains(@class,"lm4-chapter-link")]').Get() do
        MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
        MANGAINFO.ChapterNames.Add(x.XPathString('.//div[contains(@class,"lm4-chapter-name")]', v))
    end
    
    MANGAINFO.ChapterLinks.Reverse()  MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return true
end
