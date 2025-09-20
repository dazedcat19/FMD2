----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5b0c4c616c7f4a759d40d5b6924e535b'
	m.Name                     = 'SetsuScans'
	m.RootURL                  = 'https://manga.saytsu.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.saytsu.com'
local DirectoryPagination = '/mangaloaded?page_size=100'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*)()').Get() do
		LINKS.Add('manga/' .. x.XPathString('slug', v))
		NAMES.Add(x.XPathString('title', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. URL:match('(/manga/.-)$')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.AltTitles = x.XPathString('json(*).alternate_title')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('json(*).cover_image_url'))
	MANGAINFO.Authors   = x.XPathString('json(*).author.name')
	MANGAINFO.Artists   = x.XPathString('json(*).artist.name')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).status'))
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	local slug = x.XPathString('json(*).slug') .. '/'
	local chapters = {}
	for v in x.XPath('json(*).chapters()').Get() do
		table.insert(chapters, {
			index = tonumber(v.GetProperty('index').ToString()),
			slug = v.GetProperty('slug').ToString(),
			number = v.GetProperty('chapter_number').ToString(),
			title = v.GetProperty('title').ToString()
		})
	end

	table.sort(chapters, function(a, b) return a.index < b.index end)

	for _, chapter in ipairs(chapters) do
		chapter.title = chapter.title ~= '' and string.format(' - %s', chapter.title) or ''
		MANGAINFO.ChapterLinks.Add('manga/' .. slug .. chapter.slug)
		MANGAINFO.ChapterNames.Add('Chapter ' .. chapter.number .. chapter.title)
	end

	HTTP.Reset()
	HTTP.Headers.Values['Accept'] = 'human/ok'

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = API_URL .. URL

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).full_image_paths()').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Accept'] = 'human/ok'

	return true
end