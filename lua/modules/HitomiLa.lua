function Init()
	local m = NewWebsiteModule()
	m.ID                    = '1972cec9c85b43f6b10b11920a7aafef'
	m.Name                  = 'Hitomi'
	m.RootURL               = 'https://hitomi.la'
	m.Category              = 'H-Sites'
	m.OnGetNameAndLink      = 'GetNameAndLink'
	m.OnGetInfo             = 'GetInfo'
	m.OnGetPageNumber       = 'GetPageNumber'
	m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local CDN_URL = 'gold-usergeneratedcontent.net'
local gg_data = nil

----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

-- Fetches and parses gg.js, caching the result for efficiency.
function GetGgData()
	if gg_data then return gg_data end

	if not HTTP.GET('https://ltn.' .. CDN_URL .. '/gg.js') then return nil end
	local gg_script = HTTP.Document.ToString()

	-- Store parsed data in the module-level cache
	gg_data = {
		subdomain_offset_default = tonumber(gg_script:match('var o = (%d)')),
		subdomain_offset_map = {},
		common_image_id = gg_script:match("b: '(.+)'")
	}

	local o = tonumber(gg_script:match('o = (%d); break;'))
	for case in gg_script:gmatch('case (%d+):') do
		gg_data.subdomain_offset_map[tonumber(case)] = o
	end

	return gg_data
end

-- Calculates a numeric ID from an image hash.
function GetImageIdFromHash(hash)
	if not hash or hash:len() < 3 then return 0 end
	local last_three = hash:sub(-3)
	local part1 = last_three:sub(1, 2)
	local part2 = last_three:sub(3, 3)
	return tonumber(part2 .. part1, 16)
end

-- Determines the subdomain offset using the parsed gg.js data.
function GetSubdomainOffset(image_id, gg)
	return (image_id and gg.subdomain_offset_map[image_id]) or gg.subdomain_offset_default
end

-- Generates the path for a thumbnail image.
function ThumbPathFromHash(hash)
	return hash:gsub('^.*(..)(.)$', '%2/%1')
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = 'https://ltn.' .. CDN_URL .. '/index-all.nozomi'

	if not HTTP.GET(u) then return net_problem end

	local s = HTTP.Document.ToString()

	-- The .nozomi file contains gallery IDs as 32-bit big-endian integers.
	for i = 1, s:len(), 4 do
		local id = (s:byte(i) * 16777216) + (s:byte(i+1) * 65536) + (s:byte(i+2) * 256) + s:byte(i+3)
		LINKS.Add('-' .. id .. '.html')
		NAMES.Add(id)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local gg = GetGgData()

	if not gg then return net_problem end

	if not HTTP.GET('https://ltn.' .. CDN_URL .. '/galleries/' .. URL:match('-(%d+)%.html') .. '.js') then return net_problem end

	local x = CreateTXQuery()
	x.ParseHTML(HTTP.Document.ToString():match('^var galleryinfo = (.*)'))

	local first_file_hash = x.XPathString('json(*).files(1).hash')
	local image_id = GetImageIdFromHash(first_file_hash)
	local subdomain_offset = GetSubdomainOffset(image_id, gg)
	local thumb_subdomain = string.char(string.byte('a') + subdomain_offset) .. 'tn'

	local desc = {}

	local parodies = {}
	local parody_nodes = x.XPath('json(*).parodys().parody')
	for i = 1, parody_nodes.Count do
		table.insert(parodies, parody_nodes.Get(i).ToString())
	end

	local characters = {}
	local chara_nodes = x.XPath('json(*).characters().character')
	for i = 1, chara_nodes.Count do
		table.insert(characters, chara_nodes.Get(i).ToString())
	end

	local page_count = x.XPathCount('json(*).files()')
	local language = x.XPathString('json(*).language')

	if #parodies > 0 then table.insert(desc, 'Series: ' .. table.concat(parodies, ', ')) end
	if #characters > 0 then table.insert(desc, 'Characters: ' .. table.concat(characters, ', ')) end
	if page_count > 0 then table.insert(desc, 'Pages: ' .. page_count) end
	if language ~= 'null' then table.insert(desc, 'Language: ' .. language) end

	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.CoverLink = string.format('https://%s.%s/webpbigtn/%s/%s.webp',
		thumb_subdomain, CDN_URL, ThumbPathFromHash(first_file_hash), first_file_hash)
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().tag') .. ', ' .. x.XPathString('json(*).type')
	MANGAINFO.Summary   = table.concat(desc, '\r\n')

	local AltTitles = x.XPathString('json(*).japanese_title')
	if AltTitles == 'null' then AltTitles = '' end
	MANGAINFO.AltTitles = AltTitles

	local groups = x.XPathStringAll('json(*).groups().group')
	if #groups > 0 then
		MANGAINFO.Authors = groups
	else
		MANGAINFO.Authors = x.XPathStringAll('json(*).artists().artist')
	end

	MANGAINFO.ChapterLinks.Add(x.XPathString('json(*).galleryurl'))
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local gg = GetGgData()

	if not gg then return false end

	if not HTTP.GET('https://ltn.' .. CDN_URL .. '/galleries/' .. URL:match('-(%d+)%.html') .. '.js') then return false end

	local x = CreateTXQuery()
	x.ParseHTML(HTTP.Document.ToString():match('^var galleryinfo = (.*)'))

	for image_node in x.XPath('json(*).files()').Get() do
		local hash = x.XPathString('hash', image_node)
		local image_id = GetImageIdFromHash(hash)
		local subdomain_offset = GetSubdomainOffset(image_id, gg)

		TASK.PageLinks.Add('https://w' .. (subdomain_offset + 1) .. '.' .. CDN_URL .. '/' .. gg.common_image_id .. image_id .. '/' .. hash .. '.webp')
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end