----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local domain = 'inkstory.net'
local API_URL = 'https://api.' .. domain .. '/v2'
local IMAGE_NAME_LENGTH = 36
local IMAGE_MODE_INDEX = 15
local MIN_IMAGE_SIGNATURE_SIZE = 12
local SECRET_KEY = 'UySkp0BzPhwlvP2V'
local DirectoryPagination = '/books?sort=createdAt,desc&page='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Detect the codec used for a given image URL by inspecting the file name.
local function DetectImageCodec(image_url)
	local file_name = image_url:match('([^/]+)$')
	file_name = file_name:match('^[^%?]+') or file_name
	local base_name = file_name:match('(.+)%.[^%.]+$') or file_name

	if #base_name ~= IMAGE_NAME_LENGTH then return nil end

	local mode = base_name:sub(IMAGE_MODE_INDEX, IMAGE_MODE_INDEX)
	if mode == 's' then
		return 'SEC'
	elseif mode == 'x' then
		return 'XOR'
	end
	return nil
end

-- Check the first bytes of a payload against known image signatures.
local function LooksLikeImage(payload)
	if #payload < MIN_IMAGE_SIGNATURE_SIZE then return nil end

	local b = { payload:byte(1, MIN_IMAGE_SIGNATURE_SIZE) }

	-- JPEG: FF D8 FF
	if b[1] == 0xFF and b[2] == 0xD8 and b[3] == 0xFF then
		return true
	end

	-- PNG: 89 50 4E 47
	if b[1] == 0x89 and b[2] == 0x50 and b[3] == 0x4E and b[4] == 0x47 then
		return true
	end

	-- GIF: 47 49 46 38
	if b[1] == 0x47 and b[2] == 0x49 and b[3] == 0x46 and b[4] == 0x38 then
		return true
	end

	-- WEBP: 52 49 46 46 .. .. .. .. 57 45 42 50
	if b[1] == 0x52 and b[2] == 0x49 and b[3] == 0x46 and b[4] == 0x46 and b[9] == 0x57 and b[10] == 0x45 then
		local b2 = { payload:byte(9, 12) }
		if b2[1] == 0x57 and b2[2] == 0x45 and b2[3] == 0x42 and b2[4] == 0x50 then
			return true
		end
	end

	-- AVIF: bytes[4..10] == 'ftypavi'
	if b[5] == 0x66 and b[6] == 0x74 and b[7] == 0x79 and b[8] == 0x70 and b[9] == 0x61 and b[10] == 0x76 then
		return true
	end

	return false
end

-- XOR a payload against SECRET_KEY.
local function XorCipher(data, key)
	local key_len = #key
	local out = {}
	for i = 1, #data do
		local kb = key:byte(((i - 1) % key_len) + 1)
		out[i] = string.char(data:byte(i) ~ kb)
	end
	return table.concat(out)
end

local function GetContent(status)
	local content = {
		['SAFE'] = 'Безопасный',
		['UNSAFE'] = 'Неприемлемый',
		['EROTIC'] = 'Эротика',
		['PORNOGRAPHIC'] = 'Порнография'
	}
	return content[status]
end

local function GetFormats(format)
	local formats = {
		['FOURTH_KOMA'] = 'Ёнкома (4-кома)',
		['DOUJINSHI'] = 'Додзинси',
		['COMPILATION'] = 'Сборник',
		['COLORED'] = 'Цветной',
		['SINGLE'] = 'Сингл',
		['WEB'] = 'Веб',
		['WEBTOON'] = 'Вeбтун',
		['ARTBOOK'] = 'Артбук',
		['LIGHT'] = 'Печать',
	}
	return formats[format]
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 0

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = math.ceil(HTTP.Headers.Values['X-Estimated-Total-Hits'] / 20)

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = API_URL .. DirectoryPagination .. URL

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*)()').Get() do
		LINKS.Add('content/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('name').GetProperty('ru').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = API_URL .. '/books' .. URL:match('(/[^/]+)$')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(*)')
	MANGAINFO.Title     = x.XPathString('name?ru', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(altNames?*?name, ", ")', info)
	MANGAINFO.CoverLink = x.XPathString('poster', info)
	MANGAINFO.Authors   = x.XPathString('string-join(relations?*[type="AUTHOR"]?publisher?name, ", ")', info)
	MANGAINFO.Artists   = x.XPathString('string-join(relations?*[type="ARTIST"]?publisher?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join(labels?*?name, ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info), 'ONGOING', 'DONE', 'FROZEN')
	MANGAINFO.Summary   = x.XPathString('description', info)

	local content = GetContent(x.XPathString('contentStatus', info))
	if content then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. content end

	local formats = {}
	for format in x.XPath('formats?*', info).Get() do
		table.insert(formats, GetFormats(format.ToString()))
	end
	MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. table.concat(formats, ', ')

	local id = x.XPathString('id', info)
	local optgroup = MODULE.GetOption('showscangroup')
	local branches = {}

	if optgroup then
		u = API_URL .. '/branches?bookId=' .. id .. '&moderationStatus=APPROVED'

		if not HTTP.GET(u) then return net_problem end

		local x = CreateTXQuery(HTTP.Document)
		for b in x.XPath('json(*)()').Get() do
			local branch_id  = x.XPathString('id', b)
			local group_name = x.XPathString('string-join(publishers?*?name, ", ")', b)
			if group_name ~= '' and group_name ~= 'null' then
				branches[branch_id] = group_name
			end
		end
	end

	u = API_URL .. '/chapters?bookId=' .. id .. '&moderationStatus=APPROVED'

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*)()').Get() do
		local volume  = v.GetProperty('volume').ToString()
		local chapter = v.GetProperty('number').ToString()
		local title   = v.GetProperty('name').ToString()

		volume = (volume ~= 'null' and volume ~= '') and ('Том ' .. volume .. ' ') or ''
		chapter = (chapter ~= 'null' and chapter ~= '') and ('Глава ' .. chapter) or ''
		title = (title ~= 'null' and title ~= '') and (' - ' .. title) or ''

		local scanlators = ''
		if optgroup then
			local branch_id = v.GetProperty('branchId').ToString()
			local group = branches[branch_id]
			scanlators = (group ~= '') and (' [' .. group .. ']') or ''
		end

		MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. scanlators)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/chapters/' .. URL:match('[^/]+$')

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).pages().image', TASK.PageLinks)

	return true
end

-- Download and decrypt image given the image URL.
function DownloadImage()
	if not HTTP.GET(URL) then return false end

	if DetectImageCodec(URL) ~= 'XOR' then
		return true
	end

	local payload = HTTP.Document.ToString()

	if #payload < MIN_IMAGE_SIGNATURE_SIZE then
		return true
	end

	local peek = payload:sub(1, MIN_IMAGE_SIGNATURE_SIZE)

	if LooksLikeImage(peek) then
		return true
	end

	local decryptedHeader = XorCipher(peek, SECRET_KEY)
	if not LooksLikeImage(decryptedHeader) then
		return true
	end

	local decrypted = XorCipher(payload, SECRET_KEY)
	HTTP.Document.WriteString(decrypted)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '8e78adde85ad4c97ada4673c3fc11318'
	m.Name                     = 'InkStory'
	m.RootURL                  = 'https://' .. domain
	m.Category                 = 'Russian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
	m.SortedList               = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showscangroup'] = 'Show scanlation group'
		},
		['ru_RU'] = {
			['showscangroup'] = 'Показывать сканлейт-группу'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('showscangroup', lang.showscangroup, false)
end