----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local DirectoryPages = {'fantasy', 'action', 'romance', 'horror', 'slice_of_life', 'comedy', 'sports', 'sf', 'mystery', 'others'}

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/api/genres/' .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. '/titles?s=latest_desc&l=en&page=' .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local series = CreateTXQuery(HTTP.Document).XPath('json(*).titles()')
	if series.Count == 0 then return no_error end

	for v in series.Get() do
		LINKS.Add('titles/' .. v.GetProperty('contents_id').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="cover"]/img/@data-src')
	MANGAINFO.Authors   = x.XPathString('//div[contains(@class, "author")]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genre-area"]//a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="book-submit-type"]/div'), 'Series', 'One-shot')
	MANGAINFO.Summary   = x.XPathString('//div[contains(@class, "summary")]/p')

	while true do
		for v in x.XPath('//div[@class="mod-ct-series"]/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('.//div[@class="number"]', v))
		end
		local n = x.XPathString('//div[@class="js_pagination mod-pagination"]//li[@class="pageAnchor"]/a[@class="next"]/@href')
		if (n ~= '') and HTTP.GET(MANGAINFO.URL .. n) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(//div[@data-pages]/@data-pages).pc().image_url', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '7123b76f128641e68284cd4354675825'
	m.Name                     = 'MangaPlus Creators'
	m.RootURL                  = 'https://mangaplus-creators.jp'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.TotalDirectory           = #DirectoryPages
end