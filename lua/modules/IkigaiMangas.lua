----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'Spanish'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
	end
	AddWebsiteModule('ds42a85566244b7e836679491ce679e8', 'Nocbro', 'https://nocbro.xyz')
	AddWebsiteModule('c67d163c51b24bct98ertb8hh0d8sdtt', 'Ikigaiweb', 'https://es.ikigaiweb.lat/')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://panel.ikigaimangas.com/api/swf'
DirectoryPagination = '/series?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	if not HTTP.GET(API_URL .. DirectoryPagination .. (URL + 1) .. '&type=comic') then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('series/comic-' .. x.XPathString('slug', v))
		NAMES.Add(x.XPathString('name', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('json(*).last_page')) or 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local chapter, next_page, title, v, x = nil
	local u = API_URL .. '/series/' .. URL:match('/series/(.-)$')
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	
	if not HTTP.GET(u) then return net_problem end
	
	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).series.name')
	MANGAINFO.CoverLink = x.XPathString('json(*).series.cover')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).series.genres().name')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).series.status.name'), 'En Curso', 'Completa')
	MANGAINFO.Summary   = x.XPathString('json(*).series.summary')

	u = u .. '/chapters'  
	while u do
		if not HTTP.GET(u) then return net_problem end
		
		x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).data()').Get() do
			chapter = x.XPathString('name', v)
			title = x.XPathString('title', v)

			if title == 'null' then title = '' end
			if title ~= '' then title = ': ' .. title end

			MANGAINFO.ChapterLinks.Add(x.XPathString('id', v))
			MANGAINFO.ChapterNames.Add('Capítulo ' .. chapter .. title)
		end  

		next_page = x.XPathString('json(*).links.next')
		u = next_page ~= 'null' and next_page or nil
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	HTTP.Cookies.Values['data-saving'] = '0'
	HTTP.Cookies.Values['nsfw-mode'] = '1'
	if not HTTP.GET(MODULE.RootURL .. '/capitulo/' .. URL) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "img")]/img/@src', TASK.PageLinks)

	return no_error
end
