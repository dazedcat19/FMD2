----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'f239e87c7a1248d29cdd2ea8a77df36c'
	m.Name                     = 'MangaPlus'
	m.RootURL                  = 'https://mangaplus.shueisha.co.jp'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['imageresolution'] = 'Page resolution:',
			['resolution'] = 'Low\nMedium\nHigh'
		},
		['es'] = {
			['imageresolution'] = 'Resolución de página:',
			['resolution'] = 'Bajo\nMedio\nAlto'
		},
		['fr'] = {
			['imageresolution'] = 'Résolution de la page:',
			['resolution'] = 'Basse\nMoyenne\nHaute'
		},
		['id_ID'] = {
			['imageresolution'] = 'Resolusi halaman:',
			['resolution'] = 'Rendah\nSedang\nTinggi'
		},
		['pt_BR'] = {
			['imageresolution'] = 'Resolução da Página:',
			['resolution'] = 'Baixa\nMédia\nAlta'
		},
		['ru_RU'] = {
			['imageresolution'] = 'Разрешение страницы:',
			['resolution'] = 'Низкое\nСреднее\nВысокое'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionComboBox('imageresolution', lang:get('imageresolution'), lang:get('resolution'), 2)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://jumpg-webapi.tokyo-cdn.com/api'
local separator = '↣' -- Save Encryption key in the URL and separate it using obscure char (U+21A3)

-- Local Functions
local function splitString(s, delimiter)
	local result = {}
	for match in (s .. delimiter):gmatch("(.-)" .. delimiter) do
		table.insert(result, match)
	end
	return result
end

local function hexToStr(str)
	return str:gsub("%x%x", function(c) return c.char(tonumber(c, 16)) end)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local lang, v = nil
	local u = API_URL .. '/title_list/allV2?format=json'

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).success.allTitlesViewV2.AllTitlesGroup().titles()').Get() do
		lang = GetLang(v.GetProperty('language').ToString())
		LINKS.Add('titles/' .. v.GetProperty('titleId').ToString())
		NAMES.Add(v.GetProperty('name').ToString() .. lang)
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local name, json, lang, v, x = nil
	local u = API_URL .. '/title_detailV3?title_id=' .. URL:match('(%d+)') .. '&format=json'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	json = x.XPath('json(*).success.titleDetailView')
	lang = GetLang(x.XPathString('title/language', json))
	MANGAINFO.Title     = x.XPathString('title/name', json) .. lang
	MANGAINFO.CoverLink = x.XPathString('titleImageUrl', json)
	MANGAINFO.Authors   = x.XPathString('title/author', json)
	MANGAINFO.Summary   = x.XPathString('overview', json)

	local function addChapter(chapterlist)
		if chapterlist ~= nil then
			local name = chapterlist.GetProperty('subTitle').ToString()
			if name == '' then name = chapterlist.GetProperty('name').ToString() end
			MANGAINFO.ChapterNames.Add(name)
			MANGAINFO.ChapterLinks.Add(chapterlist.GetProperty('chapterId').ToString())
		end
	end

	for v in x.XPath('json(*).success.titleDetailView.chapterListGroup().firstChapterList()').Get() do
		addChapter(v)
	end
	for v in x.XPath('json(*).success.titleDetailView.chapterListGroup().lastChapterList()').Get() do
		addChapter(v)
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local crypto = require 'fmd.crypto'
	local encryption_key, image_url, v = nil
	local imageresolution = {'low', 'high', 'super_high'}
	local sel_imageresolution = (MODULE.GetOption('imageresolution') or 2) + 1
	local u = API_URL .. '/manga_viewer?chapter_id=' .. URL:match('(%d+)') .. '&img_quality=' .. imageresolution[sel_imageresolution] .. '&split=yes&format=json'

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).success.mangaViewer.pages().mangaPage').Get() do
		image_url = v.GetProperty('imageUrl').ToString()
		encryption_key = v.GetProperty('encryptionKey').ToString()
		TASK.PageLinks.Add(image_url .. separator .. encryption_key)
	end

	return no_error
end

-- Download and decrypt image given the image URL.
function DownloadImage()
	local t = splitString(URL, separator)
	local url = t[1]
	local key = hexToStr(t[2])

	if not HTTP.GET(url) then return false end

	local data = HTTP.Document.ToString()
	local parsed = {}
	for i = 1, data:len() do
		parsed[i] = string.char(string.byte(data, i) ~ string.byte(key, ((i - 1)%string.len(key)) + 1))
	end
	HTTP.Document.WriteString(table.concat(parsed, ""))
	return true
end

function GetLang(lang)
	local langs = {
		["SPANISH"] = " [ES]",
		["FRENCH"] = " [FR]",
		["GERMAN"] = " [DE]",
		["INDONESIAN"] = " [ID]",
		["PORTUGUESE_BR"] = " [PT-BR]",
		["RUSSIAN"] = " [RU]",
		["THAI"] = " [TH]",
		["VIETNAMESE"] = " [VI]"
	}
	if langs[lang] ~= nil then
		return langs[lang]
	else
		return " [EN]"
	end
end