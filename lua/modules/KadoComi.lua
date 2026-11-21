----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local crypto = require 'fmd.crypto'
local domain = 'comic-walker.com'
local API_URL = 'https://' .. domain .. '/api'
local ImageSize = 'width:1284'
local DirectoryPage = '/series/new'
local DirectoryPageLimit = 100

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the total number of pages of the current manga list.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPage .. '?limit=' .. DirectoryPageLimit .. '&offset=0'

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = math.ceil(CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*).total') / DirectoryPageLimit) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPage .. '?limit=' .. DirectoryPageLimit .. '&offset=' .. (URL * DirectoryPageLimit)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPath('json(*)?result?*').Get() do
		LINKS.Add('detail/' .. v.GetProperty('code').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local work_code = URL:match('/detail/([^/?#]+)')
	local u = API_URL .. '/contents/details/work?workCode=' .. work_code

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('parse-json(.)')
	MANGAINFO.Title     = x.XPathString('work?title', info)
	MANGAINFO.CoverLink = x.XPathString('work?bookCover', info)
	MANGAINFO.Authors   = x.XPathStringAll('work?authors?*[role=("原作","著者")]?name', ', ', info)
	MANGAINFO.Artists   = x.XPathStringAll('work?authors?*[role=("漫画","作画")]?name', ', ', info)
	MANGAINFO.Genres    = x.XPathStringAll('(work?genre?name, work?subGenre?name, work?tags?*?name)', ', ', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('work?serializationStatus', info), 'ongoing', 'finished')

	if MANGAINFO.CoverLink == '' then
		MANGAINFO.CoverLink = x.XPathString('work?originalThumbnail', info)
	end

	local summary = x.XPathString('work?summary', info)
	local next_update = x.XPathString('work?next_updateDateText', info)
	if next_update ~= '' then
		if summary ~= '' then summary = summary .. '\r\n' end
		summary = summary .. 'Next Update: ' .. next_update
	end
	MANGAINFO.Summary = summary

	local show_paid = MODULE.GetOption('showpaid')
	for v in x.XPath('latestEpisodes?result?*', info).Get() do
		local id  = v.GetProperty('id').ToString()
		local active = v.GetProperty('isActive').ToString() == 'true'
		if id ~= '' and (active or show_paid) then
			local code  = v.GetProperty('code').ToString()
			local title = v.GetProperty('title').ToString()
			local sub   = v.GetProperty('subTitle').ToString()
			local name  = title
			if not active then name = '🔒 ' .. name end
			if sub ~= '' then name = name .. ' - ' .. sub end
			MANGAINFO.ChapterLinks.Add('detail/' .. work_code .. '/episodes/' .. code .. '#' .. id)
			MANGAINFO.ChapterNames.Add(name)
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/contents/viewer?episodeId=' .. URL:match('#([^#]+)$') .. '&imageSizeType=' .. ImageSize

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPath('parse-json(.)?manuscripts?*').Get() do
		TASK.PageLinks.Add(v.GetProperty('drmImageUrl').ToString() .. '#' .. v.GetProperty('drmHash').ToString())
	end

	return true
end

-- Download, decrypt and/or descramble image given the image URL.
function DownloadImage()
	if not HTTP.GET(URL:match('^(.-)#')) then return false end

	local key_hex = URL:match('[^#]+#(.+)$')
	local key = crypto.HexToStr(key_hex)
	if key == '' then return true end

	local data = HTTP.Document.ToString()
	local out  = {}
	local klen = key:len()
	for i = 1, data:len() do
		out[i] = string.char(string.byte(data, i) ~ string.byte(key, ((i - 1) % klen) + 1))
	end
	HTTP.Document.WriteString(table.concat(out, ''))

	return true
end

-- Provide a known-good manga and chapter for the built-in module checker.
function CheckSite()
	MANGACHECK.MangaURL     = '/detail/KC_007033_S'
	MANGACHECK.MangaTitle   = 'モブ司祭だけど、この世界が乙女ゲームだと気づいたのでヒロインを育成します'
	MANGACHECK.ChapterURL   = '/detail/KC_007033_S/episodes/KC_0070330001800021_E#01a1819e-52d5-7518-8136-2d89896e0d10'
	MANGACHECK.ChapterTitle = '第14話-2：もうすぐ1年'
	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '9398095f450f4a1f9f0cc5c3ea145f0b'
	m.Name                     = 'カドコミ (KadoComi)'
	m.RootURL                  = 'https://' .. domain
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
	m.OnCheckSite              = 'CheckSite'

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showpaid'] = 'Show paid chapters'
		},
		['id_ID'] = {
			['showpaid'] = 'Tampilkan bab berbayar'
		}
	}
	local lang = translations[slang] or translations['en']
	m.AddOptionCheckBox('showpaid', lang.showpaid, false)
end
