----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '23eb3a472201427e8824ecdd5223bad7'
	m.Name                     = 'MangaFire'
	m.RootURL                  = 'https://mangafire.to'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['lang'] = 'Language:',
		},
		['id_ID'] = {
			['lang'] = 'Bahasa:',
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}

	local items = 'None'
	local t = GetLangList()
	for k, v in ipairs(t) do items = items .. '\r\n' .. v end
	m.AddOptionComboBox('lang', lang:get('lang'), items, 1)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api'
local DirectoryPagination = '/titles?order[chapter_updated_at]=desc&hot=1&page='

local Langs = {
	["en"] = "English",
	["fr"] = "French",
	["ja"] = "Japanese",
	["pt-br"] = "Portuguese (Br)",
	["pt"] = "Portuguese (Pt)",
	["es-la"] = "Spanish (LATAM)",
	["es"] = "Spanish (Es)"
}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

function GetLangList()
	local t = {}
	for k, v in pairs(Langs) do table.insert(t, v) end
	table.sort(t)
	return t
end

local function FindLanguage(lang)
	local t = GetLangList()
	for i, v in ipairs(t) do
		if i == lang then
			lang = v
			break
		end
	end
	for k, v in pairs(Langs) do
		if v == lang then return k end
	end
	return nil
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. 1 .. '&limit=30'

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.Headers.Values['Accept'] = 'application/json'
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'

	if not HTTP.GET(u) then return net_problem end

	local crypto = require 'fmd.crypto'
	local dx = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
	PAGENUMBER = tonumber(dx.XPathString('json(*).meta/lastPage')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. (URL + 1) .. '&limit=30'

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.Headers.Values['Accept'] = 'application/json'
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'

	if not HTTP.GET(u) then return net_problem end

	local crypto = require 'fmd.crypto'
	local dx = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
	local items = dx.XPath('json(*).items()')
	for ic = 1, (items and items.Count or 0) do
		local v = items.Get(ic)
		LINKS.Add(dx.XPathString('url', v) or '')
		NAMES.Add(dx.XPathString('title', v) or '')
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local hid = URL:match('/title/(.-)%-') or URL:match('^(%w+)%-') or URL:match('%.(%w+)$')
	local u = MODULE.RootURL .. API_URL .. '/titles/' .. hid

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.Headers.Values['Accept'] = 'application/json'
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'

	if not HTTP.GET(u) then
		print('MangaFire HTTP.GET failed for: ' .. u)
		return net_problem
	end
	local doc_size = 'unknown'
	if HTTP.Document ~= nil and HTTP.Document.Size ~= nil then
		doc_size = tostring(HTTP.Document.Size)
	end

	if HTTP.Document == nil or HTTP.Document.Size == 0 then
		print('MangaFire HTTP.Document is empty')
		return net_problem
	end

	local crypto = require 'fmd.crypto'
	local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('json(*)')
	if json == nil or json.Count == 0 then
		print('MangaFire failed to parse JSON data')
		return net_problem
	end

	MANGAINFO.Title     = x.XPathString('data/title', json) or ''
	MANGAINFO.AltTitles = x.XPathString('string-join(data/altTitles(), ", ")', json) or ''
	MANGAINFO.CoverLink = x.XPathString('data/poster/large', json) or ''
	MANGAINFO.Authors   = x.XPathStringAll('data/authors()/title', json) or ''
	MANGAINFO.Artists   = x.XPathStringAll('data/artists()/title', json) or ''
	MANGAINFO.Genres    = x.XPathStringAll('data/genres()/title', json) or ''
	local demographics = x.XPathStringAll('data/demographics()/title', json) or ''
	local themes = x.XPathStringAll('data/themes()/title', json) or ''
	if demographics ~= '' then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. demographics end
	if themes ~= '' then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. themes end
	MANGAINFO.Genres    = MANGAINFO.Genres:gsub('^, ', ''):gsub(', , ', ', '):gsub(', $', '')
	local status_text = x.XPathString('data/status', json) or ''
	MANGAINFO.Status = MangaInfoStatusIfPos(status_text, 'Ongoing|Releasing', 'Completed|Finished', 'Hiatus|On Hold', 'Canceled|Dropped')

	local slug         = x.XPathString('data/slug', json) or ''
	local synopsis     = x.XPathString('data/synopsisHtml', json) or ''
	if synopsis ~= '' and synopsis ~= 'null' then
		x.ParseHTML(synopsis)
		MANGAINFO.Summary = x.XPathString('string-join(//text(), "\r\n")')
	end

	local title_url = '/title/' .. hid .. '-' .. slug

	local optlang      = MODULE.GetOption('lang')
	local optlangid    = FindLanguage(optlang)
	local langparam    = optlangid and ('&language=' .. optlangid) or ''

	local page = 1
	while true do
		local chapter_u = MODULE.RootURL .. API_URL .. '/titles/' .. hid .. '/chapters?sort=number&order=asc&page=' .. tostring(page) .. '&limit=100' .. langparam

		HTTP.Reset()
		HTTP.Headers.Values['Referer'] = MODULE.RootURL
		HTTP.Headers.Values['Accept'] = 'application/json'
		HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'

		if not HTTP.GET(chapter_u) then break end

		local cx = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
		local chapters = cx.XPath('json(*).items()')
		for ic = 1, (chapters and chapters.Count or 0) do
			local v = chapters.Get(ic)
			local chapter_id = cx.XPathString('id', v) or ''
			local number     = cx.XPathString('number', v) or ''
			local name       = cx.XPathString('name', v) or ''

			local chapter_name = 'Ch. ' .. number
			if name ~= '' and name ~= 'null' then
				chapter_name = chapter_name .. ' - ' .. name
			end

			MANGAINFO.ChapterLinks.Add(title_url .. '/' .. chapter_id)
			MANGAINFO.ChapterNames.Add(chapter_name)
		end

		local last_page = tonumber(cx.XPathString('json(*).meta.lastPage')) or 1
		if page >= last_page then break end
		page = page + 1
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local chapter_id = URL:match('/(%d+)$') or URL:match('^(%d+)$')
	local u = MODULE.RootURL .. API_URL .. '/chapters/' .. chapter_id

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.Headers.Values['Accept'] = 'application/json'
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'

	if not HTTP.GET(u) then return false end

	local crypto = require 'fmd.crypto'
	local px = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
	px.XPathStringAll('json(*).data/pages()/url', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end
