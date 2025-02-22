----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fb042c961d06479582edb2fa582e3a41'
	m.Name                     = 'Reaper Scans'
	m.RootURL                  = 'https://reaperscans.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.HeanCms'
API_URL = 'https://api.reaperscans.com'
CDN_URL = 'https://media.reaperscans.com/file/4SRBHm'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	Template.GetDirectoryPageNumber()

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	local page = 1
	local u = API_URL .. '/series/' .. URL:match('/series/(.-)$')
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	id = x.XPathString('json(*).id')
	slug = '/' .. x.XPathString('json(*).series_slug') .. '/'
	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	while true do
		if not HTTP.GET(API_URL .. '/chapters/' .. id .. '?page=' .. tostring(page) .. '&perPage=100&query=&order=asc') then return net_problem end
		x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).data()').Get() do
			title = v.GetProperty('chapter_title').ToString()
			title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

			MANGAINFO.ChapterLinks.Add('chapter' .. slug .. v.GetProperty('chapter_slug').ToString())
			MANGAINFO.ChapterNames.Add(v.GetProperty('chapter_name').ToString() .. title)
		end
		page = page + 1
		pages = tonumber(x.XPathString('json(*).meta.last_page')) or 1
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end