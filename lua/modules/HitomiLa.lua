----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

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

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Fetches and parses gg.js, caching the result for efficiency.
local function GetGgData()
	local gg = MODULE.Storage['gg']

	if gg == '' then
		if not HTTP.GET('https://ltn.' .. CDN_URL .. '/gg.js') then return net_problem end
		gg = HTTP.Document.ToString()
		MODULE.Storage['gg'] = gg
	end

	local data = {
		subdomain_offset_default = tonumber(gg:match('var o = (%d)')),
		subdomain_offset_map = {},
		common_image_id = gg:match("b: '(.+)'")
	}

	local o = tonumber(gg:match('o = (%d); break;'))
	for case in gg:gmatch('case (%d+):') do
		data.subdomain_offset_map[tonumber(case)] = o
	end

	return data
end

-- Calculate a numeric ID from an image hash.
local function GetImageIdFromHash(hash)
	if not hash or hash:len() < 3 then return 0 end
	local last_three = hash:sub(-3)
	local part1 = last_three:sub(1, 2)
	local part2 = last_three:sub(3, 3)
	return tonumber(part2 .. part1, 16)
end

-- Determine the subdomain offset using the parsed gg.js data.
local function GetSubdomainOffset(image_id, gg)
	return (image_id and gg.subdomain_offset_map[image_id]) or gg.subdomain_offset_default
end

-- Generate the path for a thumbnail image.
local function ThumbPathFromHash(hash)
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
		local id = (s:byte(i) * 16777216) + (s:byte(i + 1) * 65536) + (s:byte(i + 2) * 256) + s:byte(i + 3)
		LINKS.Add('galleries/' .. id .. '.html')
		NAMES.Add(id)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = 'https://ltn.' .. CDN_URL .. '/galleries/' .. URL:match('(%d+)%.html') .. '.js'
	local gg = GetGgData()

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('json(substring-after(., "var galleryinfo = "))')
	local first_file_hash = x.XPathString('?files(1)?hash', json)
	local image_id = GetImageIdFromHash(first_file_hash)
	local subdomain_offset = GetSubdomainOffset(image_id, gg)
	local thumb_subdomain = string.char(string.byte('a') + subdomain_offset) .. 'tn'

	local desc = {}

	local parodies = {}
	local parody = x.XPath('parodys?*?parody', json)
	for i = 1, parody.Count do
		table.insert(parodies, parody.Get(i).ToString())
	end

	local characters = {}
	local chara = x.XPath('characters?*?character', json)
	for i = 1, chara.Count do
		table.insert(characters, chara.Get(i).ToString())
	end

	local page_count = x.XPathCount('files?*', json)
	local language = x.XPathString('language', json)

	if #parodies > 0 then table.insert(desc, 'Series: ' .. table.concat(parodies, ', ')) end
	if #characters > 0 then table.insert(desc, 'Characters: ' .. table.concat(characters, ', ')) end
	if page_count > 0 then table.insert(desc, 'Pages: ' .. page_count) end
	if language ~= '' and language ~= 'null' then table.insert(desc, 'Language: ' .. language) end

	local Title = x.XPathString('title', json)
	local AltTitles = x.XPathString('japanese_title', json)
	if Title:lower() == 'null' and AltTitles ~= 'null' then Title = AltTitles end
	if AltTitles == 'null' or AltTitles == Title then AltTitles = '' end

	MANGAINFO.Title     = Title
	MANGAINFO.AltTitles = AltTitles
	MANGAINFO.CoverLink = string.format('https://%s.%s/webpbigtn/%s/%s.webp', thumb_subdomain, CDN_URL, ThumbPathFromHash(first_file_hash), first_file_hash)
	MANGAINFO.Artists   = x.XPathString('string-join((artists?*?artist, groups?*?group), ", ")', json)
	MANGAINFO.Genres    = x.XPathString('string-join((tags?*?tag, type), ", ")', json)
	MANGAINFO.Summary   = table.concat(desc, '\r\n')

	MANGAINFO.ChapterLinks.Add(x.XPathString('galleryurl', json))
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = 'https://ltn.' .. CDN_URL .. '/galleries/' .. URL:match('(%d+)%.html') .. '.js'
	local gg = GetGgData()

	if not HTTP.GET(u) then return false end

	for image in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(substring-after(., "var galleryinfo = ")).files()').Get() do
		local hash = image.GetProperty('hash').ToString()
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