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
local protoc = require 'utils.protoc'
local pb = require 'pb'
local proto_file = 'MangaPlus.proto'

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
	if langs[lang] then
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

local function ReadFile(file)
	local f = assert(io.open(file, 'rb'))
	local content = f:read('*all')
	f:close()
	return content
end

-- Read File and load it to proto
local curr_path = debug.getinfo(1, 'S').source
local curr_script = curr_path:match('[^\\/]*.lua$')
local target_file = curr_path:gsub(curr_script, proto_file):gsub('@', '')
protoc.proto3_optional = true
protoc:load(ReadFile(target_file))

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. '/title_list/allV2'
	HTTP.Headers.Values['Session-Token'] = GenerateUUID()

	if not HTTP.GET(u) then return net_problem end

	local manga = pb.decode('Response', HTTP.Document.ToString()).success.allTitlesViewV2.AllTitlesGroup
	if not manga then return net_problem end

	for _, group in ipairs(manga) do
		for _, v in ipairs(group.titles) do
			LINKS.Add('titles/' .. v.titleId)
			NAMES.Add(v.name .. GetLang(v.language))
		end
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. '/title_detailV3?title_id=' .. URL:match('(%d+)')
	HTTP.Headers.Values['Session-Token'] = GenerateUUID()
	
	if not HTTP.GET(u) then return net_problem end

	local manga = pb.decode('Response', HTTP.Document.ToString()).success.titleDetailView
	if not manga then return net_problem end

	MANGAINFO.Title     = manga.title.name .. GetLang(manga.title.language)
	MANGAINFO.CoverLink = manga.titleImageUrl
	MANGAINFO.Authors   = manga.title.author
	MANGAINFO.Status    = MangaInfoStatusIfPos(manga.titleLabels.releaseSchedule, 'day|ly|other', 'completed|one_shot')
	MANGAINFO.Summary   = manga.overview

	local genres = {}
	for _, genre in ipairs(manga.tags or {}) do
		table.insert(genres, genre.tag)
	end
	MANGAINFO.Genres = table.concat(genres, ', ')

	local function addChapter(chapter)
	for _, v in ipairs(chapter) do
		local chaptername = v.subTitle
		if chaptername == '' then chaptername = v.name end
		MANGAINFO.ChapterNames.Add(chaptername)
		MANGAINFO.ChapterLinks.Add(v.chapterId)
	end
	end

	local list_groups = manga.chapterListGroup
	if list_groups then
		for _,v in ipairs(list_groups) do
			local first_list = v.firstChapterList
			if first_list then addChapter(first_list) end
			local last_list = v.lastChapterList
			if last_list then addChapter(last_list) end
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local crypto = require 'fmd.crypto'
	local imageresolution = {'low', 'high', 'super_high'}
	local sel_imageresolution = (MODULE.GetOption('imageresolution') or 2) + 1
	local u = API_URL .. '/manga_viewer?chapter_id=' .. URL:match('(%d+)') .. '&img_quality=' .. imageresolution[sel_imageresolution] .. '&split=yes'
	HTTP.Reset()
	HTTP.Headers.Values['Session-Token'] = GenerateUUID()

	if not HTTP.GET(u) then return false end

	local manga = pb.decode('Response', HTTP.Document.ToString()).success.mangaViewer.pages
	if not manga then return false end

	for _, v in ipairs(manga) do
		if v.mangaPage then
			local image_url = v.mangaPage.imageUrl
			local encryption_key = v.mangaPage.encryptionKey
			TASK.PageLinks.Add(image_url .. separator .. encryption_key)
		end
	end
	return true
end

-- Download and decrypt image given the image URL.
function DownloadImage()
	local t = SplitString(URL, separator)
	local url = t[1]
	local key = require 'fmd.crypto'.HexToStr(t[2])

	if not HTTP.GET(url) then return false end

	local manga = HTTP.Document.ToString()
	local parsed = {}
	for i = 1, manga:len() do
		parsed[i] = string.char(string.byte(manga, i) ~ string.byte(key, ((i - 1) % string.len(key)) + 1))
	end
	HTTP.Document.WriteString(table.concat(parsed, ''))

	return true
end