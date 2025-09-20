----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '?page='

----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

-- Set the required http header for making a request.
local function SetRequestHeaders()
    HTTP.Headers.Values['Origin'] = MODULE.RootURL
    HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 1
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	PAGENUMBER = tonumber(math.ceil(x.XPathString('json(*).total') / x.XPathString('json(*).limit'))) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (URL + 1)
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).entries()').Get() do
		LINKS.Add('g/' .. v.GetProperty('id').ToString() .. '/' .. v.GetProperty('key').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = API_URL .. '/detail/' .. URL:match('g/(.-)$')
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.AltTitles = x.XPathString('json(*).subtitle')
	MANGAINFO.CoverLink = x.XPathString('json(*).thumbnails.base') .. x.XPathString('json(*).thumbnails.main.path')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).tags()[namespace="1"].name')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags()[not(namespace="1") and not(namespace="2") and not(namespace="3") and not(namespace="4") and not(namespace="5") and not(namespace="7") and not(namespace="11")].name')
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	local desc = {}

	local parodies = {}
	local parody = x.XPath('json(*).tags()[namespace="3"].name')
	for i = 1, parody.Count do
		table.insert(parodies, parody.Get(i).ToString())
	end

	local characters = {}
	local character = x.XPath('json(*).tags()[namespace="5"].name')
	for i = 1, character.Count do
		table.insert(characters, character.Get(i).ToString())
	end

	local circle = x.XPathString('json(*).tags()[namespace="2"].name')
	local magazine = x.XPathString('json(*).tags()[namespace="4"].name')
	local language = x.XPathString('json(*).tags()[namespace="11" and not(name="translated")].name')
	local page = x.XPathCount('json(*).thumbnails.entries()')

	if #parodies > 0 then table.insert(desc, 'Parodies: ' .. table.concat(parodies, ', ')) end
	if circle ~= '' then table.insert(desc, 'Circle: ' .. circle) end
	if #characters > 0 then table.insert(desc, 'Characters: ' .. table.concat(characters, ', ')) end
	if magazine ~= '' then table.insert(desc, 'Magazine: ' .. magazine) end
	if page > 0 then table.insert(desc, 'Pages: ' .. page) end
	if language ~= '' then table.insert(desc, 'Language: ' .. language) end
	MANGAINFO.Summary   = table.concat(desc, '\r\n')

	local id = x.XPathString('json(*).id')
	local key = x.XPathString('json(*).key')
	HTTP.Reset()
	SetRequestHeaders()

	if HTTP.POST(u .. '?crt=' .. MODULE.GetOption('clearance')) then
		local x = CreateTXQuery(HTTP.Document)
		local imagesize = MODULE.GetOption('datasaver') and {'780', '980', '1280', '1600', '0'} or {'0', '1600', '1280', '980', '780'}
		for _, size in ipairs(imagesize) do
			local data_id = x.XPathString('json(*).data.' .. size .. '.id')
			local data_key = x.XPathString('json(*).data.' .. size .. '.key')
			local link = id .. '/' .. key .. '/' .. data_id .. '/' .. data_key .. '/' .. size
			if data_id ~= '' then
				MANGAINFO.ChapterLinks.Add(link)
				MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
				break
			end
		end

		return no_error
	else
		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add('Clearance value missing or refresh required.')

		return no_error
	end
	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = API_URL .. '/data' .. URL .. '?crt=' .. MODULE.GetOption('clearance')
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local base = x.XPathString('json(*).base')
	local entries = x.XPath('json(*).entries()')
	for i = 1, entries.Count do
		local dimensions = x.XPath('json(*).entries()[' .. i .. '].dimensions()')
		for j = 1, dimensions.Count, 2 do
			TASK.PageLinks.Add(base .. entries.Get(i).GetProperty('path').ToString() .. '?w=' .. dimensions.Get(j).ToString())
		end
	end

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	SetRequestHeaders()

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M