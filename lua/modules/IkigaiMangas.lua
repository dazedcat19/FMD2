----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ds42a85566244b7e836679491ce679e8'
	m.Name                     = 'Ikigai Mangas'
	m.RootURL                  = 'https://visorikigai.tvsin.com'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://panel.ikigaimangas.com/api/swf'
DirectoryPagination = '/series?type=comic&nsfw=true&direction=desc&column=created_at&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. v.GetProperty('slug').ToString() .. '/')
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local chapter, pages, title, v, x = nil
	local u = API_URL .. '/series/' .. URL:match('/series/(.-)$')
	local page = 1
	
	if not HTTP.GET(u) then return net_problem end
	
	x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	MANGAINFO.Title     = x.XPathString('json(*).series.name')
	MANGAINFO.CoverLink = x.XPathString('json(*).series.cover')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).series.genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).series.status.name'), 'En Curso|Hiatus', 'Abandonada|Cancelada|Completa')
	summary = x.XPathString('json(*).series.summary')
	if summary ~= 'null' then MANGAINFO.Summary = summary end

	while true do
		if not HTTP.GET(u .. 'chapters?page=' .. tostring(page)) then return net_problem end
		x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).data()').Get() do
			chapter = x.XPathString('name', v)
			title = x.XPathString('title', v)

			title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

			MANGAINFO.ChapterLinks.Add(x.XPathString('id', v))
			MANGAINFO.ChapterNames.Add('Capítulo ' .. chapter .. title)
		end
		page = page + 1
		pages = tonumber(x.XPathString('json(*).meta.last_page')) or 1
		if page > pages then
			break
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. '/capitulo' .. URL

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "img")]/img/@src', TASK.PageLinks)

	return no_error
end