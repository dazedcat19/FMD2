----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'r042a85566244b7e836679491ce67540'
	m.Name                     = 'SkyManhuas'
	m.RootURL                  = 'https://www.skymanhuas.net'
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

API_URL = 'https://api.skymanhuas.net/api'
DirectoryPagination = '/widgets/sort_and_filter/?order=desc&sort=date&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).total_pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).results()').Get() do
		LINKS.Add('series/' .. v.GetProperty('code').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga. 
function GetInfo()
	local v, x = nil
	local u = API_URL .. '/manhua/' .. URL:match('manhuas/(%d+)/') .. '/reading'
	HTTP.MimeType = 'application/json'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).manhua().mh_title')
	MANGAINFO.CoverLink = API_URL .. '/get-image/' .. x.XPathString('json(*).manhua().image') .. '/manhuas/false'
	MANGAINFO.Authors   = x.XPathString('json(*).manhua().mh_writer')
	MANGAINFO.Artists   = x.XPathString('json(*).manhua().mh_translator')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).manhua().genres()')
	MANGAINFO.Status    = x.XPathString('json(*).manhua().mh_status')
	MANGAINFO.Summary   = x.XPathString('json(*).manhua().mh_content')
		
	for v in x.XPath('json(*).manhua().volumes().chapters()').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('chp_index_title').ToString())
	end	

	return no_error
end
 
-- Get the page count for the current chapter. 
function GetPageNumber()
	HTTP.Reset() 
	local v = nil
	local u = API_URL .. '/manhua-chapter' .. URL
	HTTP.MimeType = 'application/json'

	if not HTTP.GET(u) then return net_problem end
		
	CreateTXQuery(HTTP.Document).XPathStringAll('//img/@src', TASK.PageLinks)

	return no_error
end
