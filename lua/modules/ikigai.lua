----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ds42a85566244b7e836679491ce679e8'
	m.Name                     = 'IkigaiMangas'
	m.RootURL                  = 'https://ikigaimangas.com'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://panel.ikigaimangas.com/api/swf'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	HTTP.MimeType = 'application/json'
	if not HTTP.GET(API_URL .. '/series') then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. x.XPathString('slug', v))
		NAMES.Add(x.XPathString('name', v))
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	local slug = string.gsub(u, "https://ikigaimangas%.com/series/comic%-", "")  
	u = API_URL .. '/series/' .. slug

	if not HTTP.GET(u) then return net_problem end
	
	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).series.name')
	MANGAINFO.CoverLink = x.XPathString('json(*).series.cover')
	MANGAINFO.Authors   = x.XPathString('json(*).series.author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).series.genres().name')
	MANGAINFO.Summary   = x.XPathString('json(*).series.summary')

	local u = u .. '/chapters?page=1'  
	while u do
	  if not HTTP.GET(u) then return net_problem end
	  x = CreateTXQuery(HTTP.Document)

	  local v for v in x.XPath('json(*).data()').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('id', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('name', v))
	  end  

	  local next_page = x.XPathString('json(*).links.next')
	  u = next_page ~= 'null' and next_page or nil
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(API_URL .. '/chapters' .. URL) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*).chapter.pages()').Get() do
		TASK.PageLinks.Add(v.ToString())
	end

	return no_error
end
