----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.nexusscanlation.com/api/v1'
local DirectoryPages = { 'manga', 'manhwa', 'manhua' }
local PAGE_LIMIT = 50

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

-- Fisher-Yates shuffle of 0..count-1 driven by the given PRNG instance
local function ShuffledIndices(rng, count)
	local result = {}
	for i = 0, count - 1 do
		result[i] = i
	end

	for i = count - 1, 1, -1 do
		local j = math.floor(rng() * (i + 1))
		local tmp = result[i]
		result[i] = result[j]
		result[j] = tmp
	end

	return result
end

local function BuildFlipVerticalMatrix(cols, rows, width, height, flip_at)
	local m = {}
	local tile_h = height // rows

	for i = 0, cols * height - 1 do
		m[i] = i
	end

	for p = 0, cols * rows - 1 do
		if (flip_at[p] & 2) ~= 0 then
			local r0 = (p // cols) * tile_h
			local pc = p % cols
			for k = 0, tile_h - 1 do
				m[(r0 + k) * cols + pc] = (r0 + tile_h - 1 - k) * cols + pc
			end
		end
	end

	return m
end

local function BuildFlipHorizontalMatrix(cols, rows, width, height, flip_at)
	local m = {}
	local tile_w = width // cols

	for i = 0, width * rows - 1 do
		m[i] = i
	end

	for p = 0, cols * rows - 1 do
		if (flip_at[p] & 1) ~= 0 then
			local pr = p // cols
			local x0 = (p % cols) * tile_w
			local base = pr * width
			for x = 0, tile_w - 1 do
				m[base + x0 + x] = base + x0 + (tile_w - 1 - x)
			end
		end
	end

	return m
end

local function ApplyFlips(cols, rows, width, height, flip_at)
	local need_v, need_h = false, false
	for p = 0, cols * rows - 1 do
		if (flip_at[p] & 2) ~= 0 then need_v = true end
		if (flip_at[p] & 1) ~= 0 then need_h = true end
	end

	if (width % cols) ~= 0 or (height % rows) ~= 0 then
		return
	end

	local imagepuzzle = require 'fmd.imagepuzzle'

	if need_v then
		local m = BuildFlipVerticalMatrix(cols, rows, width, height, flip_at)
		local puzzle = imagepuzzle.Create(cols, height)
		for i = 0, cols * height - 1 do
			puzzle.Matrix[i] = m[i]
		end
		puzzle.DeScramble(HTTP.Document, HTTP.Document)
	end

	if need_h then
		local m = BuildFlipHorizontalMatrix(cols, rows, width, height, flip_at)
		local puzzle = imagepuzzle.Create(width, rows)
		for i = 0, width * rows - 1 do
			puzzle.Matrix[i] = m[i]
		end
		puzzle.DeScramble(HTTP.Document, HTTP.Document)
	end
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
		if x.XPathString('es_premium', v) ~= 'true' then
			MANGAINFO.ChapterLinks.Add(slug .. '/' .. v.GetProperty('slug').ToString())
			MANGAINFO.ChapterNames.Add('Capítulo ' .. v.GetProperty('numero').ToString())
		 end
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
			local ver = sc.GetProperty('v').ToString()
			if ver ~= '' then
				local w = v.GetProperty('w').ToString()
				local h = v.GetProperty('h').ToString()
				img_url = img_url .. '&v=' .. ver .. '&w=' .. w .. '&h=' .. h
			end
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
			local version = tonumber(fragment:match('&v=([^&]+)')) or 1
			local width = tonumber(fragment:match('&w=([^&]+)'))
			local height = tonumber(fragment:match('&h=([^&]+)'))

			local count = cols * rows
			local rng = Mulberry32(seed)
			local permutation = ShuffledIndices(rng, count)

			local puzzle = require 'fmd.imagepuzzle'.Create(cols, rows)

			for i = 0, count - 1 do
				puzzle.Matrix[i] = permutation[i]
			end

			puzzle.DeScramble(HTTP.Document, HTTP.Document)

			if version >= 2 and width and height then
				local flips = {}
				for t = 0, count - 1 do
					flips[t] = math.floor(rng() * 4)
				end

				local inverse = {}
				for t = 0, count - 1 do
					inverse[permutation[t]] = t
				end

				local flip_at = {}
				for p = 0, count - 1 do
					flip_at[p] = flips[inverse[p]]
				end

				ApplyFlips(cols, rows, width, height, flip_at)
			end
		end
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end

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