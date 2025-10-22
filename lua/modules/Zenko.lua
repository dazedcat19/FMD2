----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2041ccec443f49ddb07bfbcbdac37ac5'
	m.Name                     = 'Zenko'
	m.RootURL                  = 'https://zenko.online'
	m.Category                 = 'Ukrainian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true

	m.AddOptionCheckBox('showscangroup', 'Show scanlation group', false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://zenko-api.onrender.com'
local CDN_URL = 'https://zenko.b-cdn.net'
local DirectoryPagination = '/titles?limit=100&sortBy=releaseYear&order=DESC&offset='
local DirectoryOffset = 100
local separator = '@#%&;№%#&**#!@'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Parse input into parts
local function Parse(input)
	if not input or input == '' then return {} end

	local parts, pos = {}, 1
	while true do
		local i, j = string.find(input, separator, pos, true)
		if not i then
			table.insert(parts, string.sub(input, pos))
			break
		end
		table.insert(parts, string.sub(input, pos, i - 1))
		pos = j + 1
	end

	return { part = parts[1] or '', chapter = parts[2] or '', name = parts[3] or '' }
end

-- Generate numeric ID for sorting
local function GenerateID(input)
	if not input or input == '' then return -1 end
	local p = Parse(input)
	local part = tonumber(p.part) or 0
	local chapter = p.chapter
	if chapter == '' then return part > 0 and part or -1 end

	-- Pad single-digit chapter numbers
	local formatted = chapter
	if chapter:find('%.') then
		formatted = chapter:gsub('^(%d)%.', '0%1.')
	elseif #chapter == 1 then
		formatted = string.format('%02d', tonumber(chapter))
	end

	local id = part > 0 and (tostring(part) .. formatted) or formatted

	return tonumber(id) or -1
end

-- Format readable chapter title
local function Format(input)
	local p = Parse(input)
	local pieces = {}

	if p.part ~= '' then table.insert(pieces, 'Том ' .. p.part) end
	if p.chapter ~= '' then table.insert(pieces, 'Розділ ' .. p.chapter .. (p.name ~= '' and ' - ' or '')) end
	if p.name ~= '' then table.insert(pieces, p.name) end

	return table.concat(pieces, ' ')
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 0

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = (tonumber(CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*).meta.totalPages')) + 1) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. (DirectoryOffset * URL)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).data()').Get() do
		LINKS.Add('titles/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local crypto = require 'fmd.crypto'
	local u = API_URL .. URL:gsub('%?.*$', '')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
	MANGAINFO.Title     = x.XPathString('json(*).name')
	MANGAINFO.AltTitles = x.XPathStringAll('(json(*).engName, json(*).originalName)')
	MANGAINFO.CoverLink = CDN_URL .. '/' .. x.XPathString('json(*).coverImg') .. '?optimizer=image&width=560&quality=70'
	MANGAINFO.Authors   = x.XPathStringAll('json(*).writers().name')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).painters().name')
	MANGAINFO.Genres    = x.XPathStringAll('(json(*).genres().name, json(*).tags().name, concat(upper-case(substring(json(*).category, 1, 1)), lower-case(substring(json(*).category, 2))))')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).translationStatus'), 'ONGOING', 'FINISHED', 'PAUSED')
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	if not HTTP.GET(u .. '/chapters') then return net_problem end

	local chapters = {}
	local optgroup = MODULE.GetOption('showscangroup')
	for v in CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPath('json(*)()').Get() do
		local name = v.GetProperty('name').ToString()
		local scanlator = ' [' .. v.GetProperty('publisher').GetProperty('name').ToString() .. ']'
		local id = GenerateID(name)
		if optgroup then
			if scanlator == 'null' then scanlator = ' [no group]' end
		else
			scanlator = ''
		end
		table.insert(chapters, { id = id, link = v.GetProperty('id').ToString(), name = name, scanlator = scanlator })
	end
	table.sort(chapters, function(a, b) return a.id < b.id end)

	for _, c in ipairs(chapters) do
		MANGAINFO.ChapterLinks.Add(c.link)
		MANGAINFO.ChapterNames.Add(Format(c.name) .. c.scanlator)
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/chapters' .. URL

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).pages()').Get() do
		TASK.PageLinks.Add(CDN_URL .. '/' .. v.GetProperty('content').ToString())
	end

	return true
end