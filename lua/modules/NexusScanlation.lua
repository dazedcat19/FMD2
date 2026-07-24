----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.nexusscanlation.com/api/v1'
local DirectoryPages = { 'manga', 'manhwa', 'manhua' }
local PAGE_LIMIT = 50

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'a1f3c7d9e0b64a2c8d5f1e93b7a4c6d2'
	m.Name                     = 'Nexus Scanlation'
	m.RootURL                  = 'https://nexusscanlation.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.TotalDirectory           = #DirectoryPages
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Mulberry32 PRNG
local function Mulberry32(seed)
	local state = math.floor(seed) & 0xFFFFFFFF
	return function()
		state = (state + 0x6D2B79F5) & 0xFFFFFFFF
		local t = state

		t = ((t ~ (t >> 15)) * (1 | t)) & 0xFFFFFFFF

		t = t ~ (t + ((((t ~ (t >> 7)) * (61 | t)) & 0xFFFFFFFF))) & 0xFFFFFFFF

		return ((t ~ (t >> 14)) & 0xFFFFFFFF) / 4294967296.0
	end
end

-- Shuffles indices based on the PRNG seed
local function ShuffledIndices(count, seed)
	local result = {}
	for i = 0, count - 1 do
		result[i] = i
	end

	local rng = Mulberry32(seed)

	for i = count - 1, 1, -1 do
		local j = math.floor(rng() * (i + 1))
		local tmp = result[i]
		result[i] = result[j]
		result[j] = tmp
	end

	return result
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. '/catalog?tipo=' .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. '&orden=nuevo&page=' .. (URL + 1) .. '&limit=' .. PAGE_LIMIT

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('titulo').ToString())
	end
	UPDATELIST.CurrentDirectoryPageNumber = math.ceil(x.XPathString('json(*).meta.total') / PAGE_LIMIT) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('/([^/]+)$')
	local u = API_URL .. '/series/' .. slug

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local serie = x.XPath('json(*).serie')
	MANGAINFO.Title     = x.XPathString('titulo', serie)
	MANGAINFO.AltTitles = x.XPathString('string-join(titulos_alt?*, ", ")', serie)
	MANGAINFO.CoverLink = x.XPathString('portada_url', serie)
	MANGAINFO.Authors   = x.XPathString('string-join(autores?*?nombre, ", ")', serie)
	MANGAINFO.Genres    = x.XPathString('string-join(generos?*?nombre, ", ")', serie)
	MANGAINFO.Summary   = x.XPathString('descripcion', serie)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('estado', serie), 'en_emision', 'finalizado', 'pausado')

	for v in x.XPath('json(*).capitulos()').Get() do
		MANGAINFO.ChapterLinks.Add(slug .. '/' .. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add('Capítulo ' .. v.GetProperty('numero').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local mangaslug, chapslug = URL:match('^/([^/]+)/([^/]+)$')
	local u = API_URL .. '/series/' .. mangaslug .. '/capitulos/' .. chapslug

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.paginas()').Get() do
		local img_url = v.GetProperty('url').ToString()
		local sc = v.GetProperty('sc')
		if sc ~= '' then
			local c = sc.GetProperty('c').ToString()
			local r = sc.GetProperty('r').ToString()
			local s = sc.GetProperty('s').ToString()
			img_url = img_url .. '#seed=' .. s .. '&cols=' .. c .. '&rows=' .. r
		end
		TASK.PageLinks.Add(img_url)
	end

	return true
end

-- Download and decrypt and/or descramble image given the image URL.
function DownloadImage()
	if not HTTP.GET(URL) then return false end

	local fragment = URL:match('[^#]+(#.+)')

	if fragment then
		local seed = tonumber(fragment:match('seed=([^&]+)'))
		local cols = tonumber(fragment:match('cols=([^&]+)'))
		local rows = tonumber(fragment:match('rows=([^&]+)'))

		if seed and cols and rows then
			local count = cols * rows
			local permutation = ShuffledIndices(count, seed)

			local puzzle = require 'fmd.imagepuzzle'.Create(cols, rows)

			for i = 0, count - 1 do
				puzzle.Matrix[i] = permutation[i]
			end

			puzzle.DeScramble(HTTP.Document, HTTP.Document)
		end
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end
