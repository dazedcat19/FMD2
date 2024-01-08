----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '760d177b1f6d4763a08971c0c1b5572b'
	m.Name                     = 'OlympusScanlation'
	m.RootURL                  = 'https://olympusvisor.com'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://dashboard.olympusvisor.com/api'

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
	local slug = string.gsub(u, "https://olympusvisor%.com/series/comic%-", "") 
	local jd = API_URL .. '/search?name=' .. slug

	if not HTTP.GET(jd) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local desiredSlug = slug 
	local foundSlug = nil
	local numIterations = 0
	local v for v in x.XPath('json(*).data()').Get() do
		MANGAINFO.Title     = x.XPathString('name', v)
		MANGAINFO.CoverLink = x.XPathString('cover', v)
	    foundSlug = x.XPathString('slug', v)
	    slug = foundSlug -- Actualizar slug con el valor de foundSlug en cada iteración
	    numIterations = numIterations + 1
	    if foundSlug == desiredSlug then
	        break
	    end	
	end

	if numIterations == 1 then
	    MANGAINFO.Title = MANGAINFO.Title or x.XPathString('name', x)
	    MANGAINFO.CoverLink = MANGAINFO.CoverLink or x.XPathString('cover', x)
	end	

	if slug == nil then
		return net_problem
	end

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
