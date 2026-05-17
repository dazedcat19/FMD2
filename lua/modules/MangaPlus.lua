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

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
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
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionComboBox('imageresolution', lang.imageresolution, lang.resolution, 2)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://jumpg-webapi.tokyo-cdn.com/api'
local separator = '↣' -- Save Encryption key in the URL and separate it using obscure char (U+21A3)

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Seed random number generator once.
math.randomseed(os.time())

local function GetLang(lang)
	local langs = {
		['SPANISH'] = ' [ES]',
		['FRENCH'] = ' [FR]',
		['GERMAN'] = ' [DE]',
		['INDONESIAN'] = ' [ID]',
		['PORTUGUESE_BR'] = ' [PT-BR]',
		['RUSSIAN'] = ' [RU]',
		['THAI'] = ' [TH]',
		['VIETNAMESE'] = ' [VI]'
	}
	if langs[lang] ~= nil then
		return langs[lang]
	else
		return ' [EN]'
	end
end

local function SplitString(s, delimiter)
	local result = {}
	for match in (s .. delimiter):gmatch('(.-)' .. delimiter) do
		table.insert(result, match)
	end
	return result
end

local function GenerateUUID()
    local template = 'xxxxxxxx-xxxx-1xxx-yxxx-xxxxxxxxxxxx'
    return string.gsub(template, '[xy]', function (c)
        local v = (c == 'x') and math.random(0, 15) or math.random(8, 11)
        return string.format('%x', v)
    end)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. '/title_list/allV2?format=json'
	HTTP.Headers.Values['Session-Token'] = MODULE.GetOption('session')

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).success.allTitlesViewV2.AllTitlesGroup().titles()').Get() do
		LINKS.Add('titles/' .. v.GetProperty('titleId').ToString())
		NAMES.Add(v.GetProperty('name').ToString() .. GetLang(v.GetProperty('language').ToString()))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. '/title_detailV3?title_id=' .. URL:match('(%d+)') .. '&format=json'
	HTTP.Headers.Values['Session-Token'] = GenerateUUID()
	
	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(*).success.titleDetailView')
	MANGAINFO.Title     = x.XPathString('title/name', info) .. GetLang(x.XPathString('title/language', info))
	MANGAINFO.CoverLink = x.XPathString('titleImageUrl', info)
	MANGAINFO.Authors   = x.XPathString('title/author', info)
	MANGAINFO.Summary   = x.XPathString('overview', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('titleLabels/releaseSchedule', info), 'monthly|weekly')

	local function addChapter(chapter)
		local name = chapter.GetProperty('subTitle').ToString()
		if name == '' then name = chapter.GetProperty('name').ToString() end
		
		MANGAINFO.ChapterNames.Add(name)
		MANGAINFO.ChapterLinks.Add(chapter.GetProperty('chapterId').ToString())
	end

	for group in x.XPath('chapterListGroup?*', info).Get() do
		for v in x.XPath('firstChapterList?*', group).Get() do
			addChapter(v)
		end

		for v in x.XPath('lastChapterList?*', group).Get() do
			addChapter(v)
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local crypto = require 'fmd.crypto'
	local imageresolution = {'low', 'high', 'super_high'}
	local sel_imageresolution = (MODULE.GetOption('imageresolution') or 2) + 1
	local u = API_URL .. '/manga_viewer?chapter_id=' .. URL:match('(%d+)') .. '&img_quality=' .. imageresolution[sel_imageresolution] .. '&split=yes&format=json'
	HTTP.Reset()
	HTTP.Headers.Values['Session-Token'] = GenerateUUID()

	if not HTTP.GET(u) then return false end

	for v in CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).success.mangaViewer.pages().mangaPage').Get() do
		local image_url = v.GetProperty('imageUrl').ToString()
		local encryption_key = v.GetProperty('encryptionKey').ToString()
		TASK.PageLinks.Add(image_url .. separator .. encryption_key)
	end

	return true
end

-- Download and decrypt image given the image URL.
function DownloadImage()
	local crypto = require 'fmd.crypto'
	local t = SplitString(URL, separator)
	local url = t[1]
	local key = crypto.HexToStr(t[2])

	if not HTTP.GET(url) then return false end

	local data = HTTP.Document.ToString()
	local parsed = {}
	for i = 1, data:len() do
		parsed[i] = string.char(string.byte(data, i) ~ string.byte(key, ((i - 1) % string.len(key)) + 1))
	end
	HTTP.Document.WriteString(table.concat(parsed, ''))

	return true
end