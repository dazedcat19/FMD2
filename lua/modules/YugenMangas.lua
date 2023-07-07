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
	AddWebsiteModule('ac42a85566244b7e836679491ce679e6', 'YugenMangas', 'https://yugenmangas.net')
	AddWebsiteModule('ac42a85566244b7e836679491ce679ey', 'Punuprojects', 'https://punuprojects.com')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------
local API_URL = 'https://api.yugenmangas.net'

function getapi(website)
	local apis  = {
		['YugenMangas']        = API_URL,
		['Punuprojects']       = 'https://api.punuprojects.com'
	}
	if apis[website] ~= nil then
		return apis[website]
	else
		return API_URL
	end
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	HTTP.MimeType = 'application/json'
	if not HTTP.POST(getapi(MODULE.Name) .. '/series/querysearch', '{"order":"asc", "type":"Comic"}') then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*)()').Get() do
		LINKS.Add('series/' .. x.XPathString('series_slug', v))
		NAMES.Add(x.XPathString('title', v))
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = getapi(MODULE.Name) .. '/series/' .. URL:match('/series/(.-)$')
	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.CoverLink = getapi(MODULE.Name) .. '/' .. x.XPathString('json(*).thumbnail')
	MANGAINFO.Authors   = x.XPathString('json(*).author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name')
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	local v for v in x.XPath('json(*).chapters()').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('id', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('chapter_name', v))
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(getapi(MODULE.Name) .. '/series/chapter' .. URL) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*).content.images()').Get() do
		if string.find(v.ToString(), "yugenmangas.com") then
			TASK.PageLinks.Add(v.ToString())
	    	elseif string.find(v.ToString(), "yugenmangas.net") then
			TASK.PageLinks.Add(v.ToString())
		else
			TASK.PageLinks.Add(getapi(MODULE.Name) .. '/' .. v.ToString())
		end
	end

	return no_error
end
