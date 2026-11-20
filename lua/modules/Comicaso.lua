----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local DirectoryPageLimit = 24

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. '/browse?scope=all'

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = math.ceil(CreateTXQuery(HTTP.Document).XPathString('//p[@class="muted"]'):match('%d+') / DirectoryPageLimit) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/api/catalog-more?scope=all&view=browse&sort=new&offset=' .. (DirectoryPageLimit * URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('json(*).html'))

	for v in x.XPath('//a[@class="card"]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('.//h3', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="detail-title"]')
	MANGAINFO.AltTitles = x.XPathString('//p[@class="detail-alternative"]')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="detail-cover"]/@src')
	MANGAINFO.Authors   = x.XPathString('//div[@class="detail-facts"]//div[dt="Author"]/dd')
	MANGAINFO.Artists   = x.XPathString('//div[@class="detail-facts"]//div[dt="Artist"]/dd')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="detail-capsules"]/span[2]'), 'Berjalan|Ongoing', 'Tamat|Completed', 'Hiatus')
	MANGAINFO.Summary   = x.XPathString('//p[@class="synopsis"]')

	if MANGAINFO.Authors == '—' then MANGAINFO.Authors = '' end
	if MANGAINFO.Artists == '—' then MANGAINFO.Artists = '' end

	local genres = {}
	for v in x.XPath('//div[contains(@class, "detail-genres")]/a').Get() do
		local g = v.InnerText():gsub('^%l', string.upper)
		if g ~= '' then
			table.insert(genres, g)
		end
	end
	local gtype = x.XPathString('//div[@class="detail-capsules"]/span[1]')
	if gtype ~= '' then
		table.insert(genres, gtype)
	end
	MANGAINFO.Genres = table.concat(genres, ', ')

	for v in x.XPath('//a[@class="detail-chapter-row"]').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('.//strong', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[@data-media-sources]/@src', TASK.PageLinks)

	return true
end

-- Verify the module's functionality by checking a specific manga and chapter.
function CheckSite()
	MANGACHECK.MangaURL              = '/manga/one-piece'
	MANGACHECK.MangaTitle            = 'One Piece'
	MANGACHECK.ChapterURL            = '/read/one-piece/91822'
	MANGACHECK.ChapterTitle          = 'Chapter 1'

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ab4a91f79df84a8583f6b806eccd1d87'
	m.Name                     = 'Comicaso'
	m.RootURL                  = 'https://comicaso.site'
	m.Category                 = 'Indonesian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnCheckSite              = 'CheckSite'
	m.SortedList               = true
end
