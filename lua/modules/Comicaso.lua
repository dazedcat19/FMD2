----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ab4a91f79df84a8583f6b806eccd1d87'
	m.Name                     = 'Comicaso'
	m.RootURL                  = 'https://v3.comicaso.pro'
	m.Category                 = 'Indonesian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/home.php?source=all&q=&mode=new&type=all&limit=120&offset='
local DirectoryPageLimit = 120

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(math.ceil(CreateTXQuery(HTTP.Document).XPathString('json(*).total') / DirectoryPageLimit)) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (DirectoryPageLimit * URL)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('?page=manga&source=' .. v.GetProperty('source').ToString() .. '&slug=' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local source, slug = URL:match('source=([^&]+).*slug=([^&]+)')
	local u = MODULE.RootURL .. '/api/manga.php?source=' .. source .. '&slug=' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(*).data')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('alternative', info)
	MANGAINFO.CoverLink = x.XPathString('thumbnail', info)
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Artists   = x.XPathString('artist', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*, concat(upper-case(substring(type, 1, 1)), lower-case(substring(type, 2)))), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info), 'on-going', 'end')
	MANGAINFO.Summary   = x.XPathString('synopsis', info)

	local chapters = {}
	for v in x.XPath('chapters?*', info).Get() do
		table.insert(chapters, {
			slug = v.GetProperty('slug').ToString(),
			title = v.GetProperty('title').ToString()
		})
	end

	table.sort(chapters, function(a, b) return (tonumber(a.slug:match('(%d+)')) or 0) < (tonumber(b.slug:match('(%d+)')) or 0) end)

	for _, chapter in ipairs(chapters) do
		MANGAINFO.ChapterLinks.Add(source .. '/' .. slug .. '/' .. chapter.slug)
		MANGAINFO.ChapterNames.Add(chapter.title)
	end

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local source, slug, cid = URL:match('^/([^/]+)/([^/]+)/([^/]+)$')
	local u = MODULE.RootURL .. '/api/chapter.php?source=' .. source .. '&manga=' .. slug .. '&chapter=' .. cid

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.images()', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end