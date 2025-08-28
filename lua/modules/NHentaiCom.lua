----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'H-Sites'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.OnGetImageURL            = 'GetImageURL'
		m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
		m.SortedList               = true
	end
	AddWebsiteModule('093e8ef9ad3542f68bbfd7aec0652773', 'NHentaiCom', 'https://nhentai.com')
	AddWebsiteModule('0052cb4aabe0443ca0c97e1eb217728a', 'HentaiHand', 'https://hentaihand.com')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/comics?sort=uploaded_at&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('en/comic/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MODULE.RootURL .. '/api/comics/' .. URL:match('/comic/(.+)')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.CoverLink = x.XPathString('json(*).thumb_url')
	MANGAINFO.Authors   = x.XPathStringAll('json(*).authors().name')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).artists().name')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name') .. ', ' .. x.XPathString('json(*).category.name')

	local alttitles = x.XPathString('json(*).alternative_title')
	MANGAINFO.AltTitles = alttitles ~= 'null' and alttitles or ''

	local desc = {}

	local parodies = {}
	local parody = x.XPath('json(*).parodies().name')
	for i = 1, parody.Count do
		table.insert(parodies, parody.Get(i).ToString())
	end

	local characters = {}
	local character = x.XPath('json(*).characters().name')
	for i = 1, character.Count do
		table.insert(characters, character.Get(i).ToString())
	end

	local relationships = {}
	local relationship = x.XPath('json(*).relationships().name')
	for i = 1, relationship.Count do
		table.insert(relationships, relationship.Get(i).ToString())
	end

	local groups = {}
	local group = x.XPath('json(*).groups().name')
	for i = 1, group.Count do
		table.insert(groups, group.Get(i).ToString())
	end

	local pages = x.XPathString('json(*).pages')
	local language = x.XPathString('json(*).language.name')
	local summary = x.XPathString('json(*).description')

	if #parodies > 0 then table.insert(desc, 'Parodies: ' .. table.concat(parodies, ', ')) end
	if #characters > 0 then table.insert(desc, 'Characters: ' .. table.concat(characters, ', ')) end
	if #relationships > 0 then table.insert(desc, 'Relationships: ' .. table.concat(relationships, ', ')) end
	if #groups > 0 then table.insert(desc, 'Groups: ' .. table.concat(groups, ', ')) end
	if pages ~= 'null' then table.insert(desc, 'Pages: ' .. pages) end
	if language ~= '' then table.insert(desc, 'Language: ' .. language) end
	if summary ~= 'null' then table.insert(desc, '\r\n \r\n' .. summary) end
	MANGAINFO.Summary = table.concat(desc, '\r\n')

	MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. '/api/comics/' .. URL:match('/comic/(.+)') .. '/images'

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).images().source_url', TASK.PageLinks)

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end