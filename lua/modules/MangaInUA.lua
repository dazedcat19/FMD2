----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '28063f2a01fa44418c2b274aeeee890b'
	m.Name                     = 'MangaInUA'
	m.RootURL                  = 'https://manga.in.ua'
	m.Category                 = 'Ukrainian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeUpdateList       = 'BeforeUpdateList'
	m.AccountSupport           = false
	
	-- Initialize storage values
	m.Storage['UserHash'] = ''
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryList  = '/mangas/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Called before updating manga list to ensure UserHash is available
function BeforeUpdateList()
    if MODULE.Storage['UserHash'] == '' then
        if not GetUserHash() then
            print("Failed to obtain UserHash in BeforeUpdateList")
            return false
        end
    end
    return true
end

-- Get UserHash from the main page
function GetUserHash()
    local u = MODULE.RootURL

    if not HTTP.GET(u) then 
        return false
    end

    local x = CreateTXQuery(HTTP.Document)
    local userHash = GetBetween("var site_login_hash = '", "';", x.XPathString('//script[contains(., "site_login_hash =")]'))

    if userHash and userHash ~= '' then
        MODULE.Storage['UserHash'] = userHash
        print("UserHash obtained:", userHash)
        return true
    else
        print("Failed to obtain UserHash")
        return false
    end
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryList 

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//div[contains(@class, "page-navigation")])[1]/a[last()-1]')) or 1

    return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryList .. 'page/' .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//h3[@class="card__title title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

    -- Ensure UserHash is available before proceeding
    local userHash = MODULE.Storage['UserHash']
    if not userHash or userHash == '' then
        if not GetUserHash() then
            print("Error: Failed to obtain UserHash in GetInfo")
            return net_problem
        end
    end

	if not HTTP.GET(u) then return net_problem end
    local x = CreateTXQuery(HTTP.Document)

	MANGAINFO.Title     = x.XPathString('//span[@class="UAname"]/text()')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="item__full-sidebar--sub" and text()="Жанри:"]/following-sibling::span/a/text()')
	MANGAINFO.Summary   = x.XPathString('//div[@class="item__full-description"]')
    MANGAINFO.Status    = MangaInfoStatusIfPos(
        x.XPathString('(//div[@class="item__full-sidebar--sub" and text()="Статус перекладу:"]/following-sibling::span/a/text())[1]'), 
        'Триває', 'Завершено|Закінчено', 'Призупинено|Пауза', 'Скасовано|Відмінено'
    )

    -- Get chapters for the current manga.
    local newsID = x.XPathString('//div[@id="linkstocomics"]/@data-news_id')
    local newsCategory = x.XPathString('//div[@id="linkstocomics"]/@data-news_category')
    
    if not newsID or newsID == '' or not newsCategory or newsCategory == '' then
        print("Error: NewsID or NewsCategory is empty")
        print("NewsID:", newsID, "NewsCategory:", newsCategory)
        return net_problem
    end
    
    local postData = string.format(
        "action=show&news_id=%s&news_category=%s&this_link=&user_hash=%s",
        newsID, newsCategory, userHash
    )

    HTTP.Reset()
    HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, URL)

    if not HTTP.POST(MODULE.RootURL .. '/engine/ajax/controller.php?mod=load_chapters', postData) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)
    x.XPathHREFAll('//div[@class="ltcitems"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)

    return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
    local chapID = URL:match("/(%d+)-")
    local userHash = MODULE.Storage['UserHash']

    -- Ensure UserHash is available
    if not userHash or userHash == '' then
        if not GetUserHash() then
            print("Error: Failed to obtain UserHash")
            return false 
        end
        userHash = MODULE.Storage['UserHash']
    end

	local u = MaybeFillHost(
        MODULE.RootURL, 
        '/engine/ajax/controller.php?mod=load_chapters_image&news_id=' .. chapID .. '&action=show&user_hash=' .. userHash
    )

	if not HTTP.GET(u) then return false  end

	local  x = CreateTXQuery(HTTP.Document)
    x.XPathStringAll('//ul[contains(@class,"xfieldimagegallery")]//img/@data-src', TASK.PageLinks)

	return true 
end