----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'rwtua85566244b7e836679491ce67787'
	m.Name                     = 'SenshimangaCom'
	m.RootURL                  = 'https://senshimanga.com'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------


API_URL = 'https://lat-manga.com/api/manga-custom'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local u = API_URL ..'/'.. URL:match('/manga/(.-)$')

	HTTP.Headers.Values['Referer'] = 'https://senshimanga.com/'
	HTTP.Headers.Values['Cache-Control'] = 'no-cache'
	HTTP.Headers.Values['Pragma'] = 'no-cache'
	HTTP.Headers.Values['Origin'] = 'https://senshimanga.com/'
	HTTP.Headers.Values['Accept'] = '*/*'
	HTTP.Headers.Values['Organization-Domain'] = 'senshimanga.com'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	
	MANGAINFO.Title     = x.XPathString('json(*).data.title')
	MANGAINFO.CoverLink = x.XPathString('json(*).data.imageUrl')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).data.genres()')
	MANGAINFO.Status    = x.XPathString('json(*).data.status')
	MANGAINFO.Summary   = x.XPathString('json(*).data.description')

	local v for v in x.XPath('json(*).data.chapters()').Get() do
		MANGAINFO.ChapterLinks.Add(URL:match('/manga/(.-)$') .. '/chapter/' .. x.XPathString('number', v) .. '/pages')
		MANGAINFO.ChapterNames.Add('Capítulo ' .. x.XPathString('number', v) .. ' ' .. x.XPathString('title', v))
	end  
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
	return no_error
end

-- Get the page count for the current chapter. 
function GetPageNumber()
	HTTP.Headers.Values['Referer'] = 'https://senshimanga.com/'
	HTTP.Headers.Values['Cache-Control'] = 'no-cache'
	HTTP.Headers.Values['Pragma'] = 'no-cache'
	HTTP.Headers.Values['Origin'] = 'https://senshimanga.com/'
	HTTP.Headers.Values['Accept'] = '*/*'
	HTTP.Headers.Values['Organization-Domain'] = 'senshimanga.com'

	if not HTTP.GET(API_URL .. URL) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*).data()').Get() do
		TASK.PageLinks.Add(x.XPathString('imageUrl', v))
	end

	return no_error
end
