----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '760d177b1f6d4763a08971c0c1b5572b'
	m.Name                     = 'OlympusScanlation'
	m.RootURL                  = 'https://olympusscans.com'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://dashboard.olympusscans.com/api'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MaybeFillHost(API_URL, '/series?direction=asc&type=comic')
	if not HTTP.GET(u) then return net_problem end
	
	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*).data().series().data()').Get() do
		LINKS.Add(MODULE.RootURL .. '/series/comic-' .. x.XPathString('slug', v))
		NAMES.Add(x.XPathString('name', v))
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local script = x.XPathString('//script[contains(., "window.__NUXT__")]')
	local scripts = script:match('data:(%b{})')
	local s = scripts:match('data:(%b{})')

	MANGAINFO.Title     = s:match('name:"(.-)"')
	MANGAINFO.CoverLink = s:match('cover:"(.-)"'):gsub("\\u002F", "/")
	MANGAINFO.Authors   = s:match('slug:"(.-)"')
	MANGAINFO.Summary   = s:match('summary:"(.-)"')

	local genres = {}
	for name in s:match('genres:%[(.-)%]'):gmatch('{name:"(.-)"') do
	    table.insert(genres, name)
	end
	MANGAINFO.Genres    = table.concat(genres, ', ')

	local slug = s:match('slug:"(.-)"')
	local id = s:match('id:(%d+)')
	local slug_read = URL:match("/series/(.*)")
	local u = API_URL .. '/series/' .. slug .. '/chapters?page=1' 

	while u do
	  if not HTTP.GET(u) then return net_problem end
	  x = CreateTXQuery(HTTP.Document)

	  local v for v in x.XPath('json(*).data()').Get() do
	    MANGAINFO.ChapterLinks.Add(slug .. '/chapters/' .. x.XPathString('id', v))
	    MANGAINFO.ChapterNames.Add(x.XPathString('name', v))
	  end   

	  local next_page = x.XPathString('json(*).links.next')
	  u = next_page ~= 'null' and next_page or nil
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/series' .. URL .. '?type=comic'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*).chapter.pages()').Get() do
		TASK.PageLinks.Add(v.ToString())
	end

	return no_error
end
