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
	m.SortedList               = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['lang'] = 'Language:',
			['listtype'] = 'List type:',
			['ltype'] = 'Chapter\nVolume',
			['chaptertype'] = 'Chapter type:',
			['ctype'] = 'All\nOfficial\nUnofficial'
		},
		['id_ID'] = {
			['lang'] = 'Bahasa:',
			['listtype'] = 'Tipe daftar:',
			['ltype'] = 'Bab\nJilid',
			['chaptertype'] = 'Tipe bab:',
			['ctype'] = 'Semua\nResmi\nTidak resmi'
		}
	}
	local lang = translations[slang] or translations.en
	local items = table.concat(GetLangList(), '\r\n')
	m.AddOptionComboBox('lang', lang.lang, items, 1)
	m.AddOptionComboBox('listtype', lang.listtype, lang.ltype, 0)
	m.AddOptionComboBox('chaptertype', lang.chaptertype, lang.ctype, 0)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api'
local DirectoryPagination = '/titles?order[created_at]=desc&limit=100&page='

local Langs = {
	{   nil, 'All' },
	{  'en', 'English' },
	{  'fr', 'French' },
	{  'ja', 'Japanese' },
	{ 'pt-br', 'Portuguese (Br)' },
	{  'pt', 'Portuguese (Pt)' },
	{ 'es-la', 'Spanish (LATAM)' },
	{  'es', 'Spanish (Es)' }
}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Return language names in defined order
function GetLangList()
	local t = {}
	for _, v in ipairs(Langs) do
		table.insert(t, v[2])
	end
	return t
end

-- Return language key by index
local function FindLanguage(lang)
	return Langs[lang + 1][1]
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).meta.lastPage')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local crypto = require 'fmd.crypto'
	for v in CreateTXQuery(HTTP.Document).XPath('json(*).items()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local hid = URL:match('/(%w+)%-') or URL:match('%.(%w+)$')
	local u = MODULE.RootURL .. API_URL .. '/titles/' .. hid

	if not HTTP.GET(u) then	return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('json(*).data')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(altTitles?*, ", ")', info)
	MANGAINFO.CoverLink = x.XPathString('poster?large', info)
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*?title, ", ")', info)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*?title, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?title, themes?*?title, demographics?*?title), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info), 'releasing', 'finished', 'on_hiatus', 'discontinued')

	local synopsis = x.XPathString('synopsisHtml', info)
	if synopsis ~= '' then
		MANGAINFO.Summary = CreateTXQuery(synopsis).XPathString('string-join(//text(), "\r\n")')
	end
	local slug = x.XPathString('slug', info)

	local chaptertype     = {nil, 'official', 'unofficial'}
	local listtype        = {'chapters', 'volumes'}
	local sel_chaptertype = (MODULE.GetOption('chaptertype') or 0) + 1
	local sel_listtype    = (MODULE.GetOption('listtype') or 0) + 1
	local optlang         = MODULE.GetOption('lang')
	local optlangid       = FindLanguage(optlang)
	local langparam       = optlangid and (sel_listtype == 1) and '&language=' .. optlangid or ''

	local page = 1
	local pages = nil
	while true do
		local urlparam = (sel_listtype == 1) and '?sort=number&order=desc&page=' .. page .. '&limit=200' or ''
		if not HTTP.GET(MODULE.RootURL .. API_URL .. '/titles/' .. hid .. '/' .. listtype[sel_listtype] .. urlparam .. langparam) then return net_problem end

		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).items()').Get() do
			local cid    = v.GetProperty('id').ToString()
			local number = v.GetProperty('number').ToString()
			local name   = v.GetProperty('name').ToString()
			local ctype  = v.GetProperty('type').ToString()
			local lang   = v.GetProperty('language').ToString()

			if not optlangid or optlangid == lang then
				if not chaptertype[sel_chaptertype] or chaptertype[sel_chaptertype] == ctype then
					local chapter_name = (sel_listtype == 1) and 'Ch. ' .. number or 'Vol. ' .. number
					if name ~= '' then
						chapter_name = chapter_name .. ' - ' .. name
					end

					if not chaptertype[sel_chaptertype] and ctype == 'official' then
						chapter_name = chapter_name .. ' (Official)'
					end

					lang = not optlangid and ' [' .. lang .. ']' or ''

					MANGAINFO.ChapterLinks.Add(hid .. '/' .. slug .. '/' .. cid)
					MANGAINFO.ChapterNames.Add(chapter_name .. lang)
				end
			end
		end

		if not pages then
			pages = tonumber(x.XPathString('json(*).meta.lastPage')) or 1
		end
		if page >= pages then break end
		page = page + 1
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local cid = URL:match('/(%d+)$')
	local listtype = {'chapters', 'volumes'}
	local sel_listtype = (MODULE.GetOption('listtype') or 0) + 1
	local u = MODULE.RootURL .. API_URL .. '/' .. listtype[sel_listtype] .. '/' .. cid

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.pages().url', TASK.PageLinks)

	return true
end
