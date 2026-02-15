----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'Spanish'
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
		m.SortedList               = true
	end
	AddWebsiteModule('73cfa250c661470c81428d99cdb8a140', 'MangaCrab', 'https://mangacrab.org')
	AddWebsiteModule('f1883499b8e640778f0f0c1a4358774f', 'MangaCrabEros', 'https://eros.mangacrab.org')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/series/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//nav[@class, "mv-pagination"]/span[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//article[@class="catalog-card"]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('h2', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local json = require 'utils.json'
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "wp-post-image")]/@src')
	MANGAINFO.Genres    = x.XPathStringAll('(//a[contains(@href, "manga-genre")], //span[contains(., "Capitulos")]/substring-after(., ":"))')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(., "Estado")]'), 'En curso', 'Finalizado', 'En Hiatus', 'Cancelada')
	MANGAINFO.Summary   = x.XPathString('//div[@class="mb-3"]/p')

	local nonce = x.XPathString('//script[contains(., "mvTheme")]/substring-before(substring-after(., "nonce"":"""), """,")')
	local mid = x.XPathString('//div[@data-manga-id]/@data-manga-id')
	local page = 1
	local pages = nil
	while true do
		HTTP.Reset()
		local timestamp = os.time() * 1000
		local s = 'action=mv_get_chapters&nonce=' .. nonce .. '&manga_id=' .. mid .. '&page=' .. page .. '&_ts=' .. timestamp
		if not HTTP.POST(MODULE.RootURL .. '/wp-admin/admin-ajax.php', s) then return net_problem end
		local w = HTTP.Document.ToString()
		if not pages then
			pages = tonumber(json.decode(w).data.total_pages) or 1
		end
		x.ParseHTML(json.decode(w).data.list)
		x.XPathHREFAll('//article[@class="chapter-item"]/div/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		page = page + 1
		if page > pages then
			break
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	if MODULE.ID == '73cfa250c661470c81428d99cdb8a140' then
		MODULE.Storage['Img-X'] = x.XPathString('//script[contains(., "imgHeader")]/substring-before(substring-after(., "imgHeader"":"""), """}")')
		for v in x.XPath('//div[@class="reader-body"]/img/@data-sec-src').Get() do
			TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
		end
	else
		x.XPathStringAll('//div[@class="reader-body"]/p/img/@src', TASK.PageLinks)
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	if MODULE.ID == '73cfa250c661470c81428d99cdb8a140' then
		HTTP.Headers.Values['Img-X'] = MODULE.Storage['Img-X']
	end

	return true
end