----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/'   --> Override template variable by uncommenting this line.

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
    local json = require 'utils.json'
    local u = MaybeFillHost(MODULE.RootURL, URL)

    if not HTTP.GET(u) then return net_problem end

    local x = json.decode(HTTP.Document.ToString():match('window.__data = (.-);'))
    MANGAINFO.Title   = x.category.name
    MANGAINFO.Status  = MangaInfoStatusIfPos(x.category.status, '0')
    MANGAINFO.Summary = x.category.description

    local entry_slug = x.category.entry_slug
    for _, chapter in ipairs(x.entries) do
    	if chapter.pages > 0 then
	        MANGAINFO.ChapterLinks.Add('manga/' .. entry_slug .. '/' .. chapter.number)
	        MANGAINFO.ChapterNames.Add(chapter.number .. ' - ' .. chapter.name)
        end
    end
    MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

    return no_error
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
-- DirectoryPagination = RootURL + Manga List
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end
	x = CreateTXQuery(HTTP.Document)
	v = x.XPath('(//a[contains(@class,"nav-link dropdown-toggle")]/following-sibling::ul)[2]//a')

	for i = 1, v.Count do
		LINKS.Add(x.XPathString('@href', v.Get(i)))
		NAMES.Add(x.XPathString('text()', v.Get(i)))
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL) .. '/1') then
	    local json = require 'utils.json'
	    local x = json.decode(HTTP.Document.ToString():match('window.__data = (.-);'))
	 
	    for _,v in ipairs(x.chapter.pages) do
	    	TASK.PageLinks.Add(v.url)
    	end

		return true
	else
		return false
	end
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '4c3fb549e0de4a1cbad85869d3d79ef7'
	m.Name                     = 'OnePiece-Tube'
	m.RootURL                  = 'https://onepiece.tube'
	m.Category                 = 'German'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end
