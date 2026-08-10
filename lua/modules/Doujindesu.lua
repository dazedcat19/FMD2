----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ec1a1ad5301f414592f0ba0402024813'
	m.Name                     = 'Doujindesu'
	m.RootURL                  = 'https://doujin.desu.xxx'
	m.Category                 = 'H-Sites'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/manga?search=&genre=&status=&type=&sort=newest&limit=100000&offset=0'
local json = require 'utils.json'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local APP_SECRET = 'dfdf72051dbfdc7d76889ebd31324e74'
local SALT = 'doujindesu-scrapers-cannot-read-this-super-secret-salt-2026-v2'

local function ToSigned32(x)
	local y = x & 0xFFFFFFFF
	if y >= 0x80000000 then
		y = y - 0x100000000
	end
	return y
end

-- Generate the dynamic key for a given time block
local function WH(e)
	local t = SALT .. '_' .. tostring(e)
	local a = 0

	for i = 1, #t do
		local charCode = string.byte(t, i)
		a = ToSigned32((a << 5) - a + charCode)
	end

	local l = a ~= 0 and math.abs(a) or 123456789
	local out = {}

	for i = 1, 32 do
		l = ((l * 1664525) + 1013904223) & 0xFFFFFFFF
		table.insert(out, string.char(33 + (l % 93)))
	end

	return table.concat(out)
end

-- Get a list of current and adjacent hour keys
local function LU()
	local t = math.floor(os.time() / 3600)
	return { WH(t), WH(t - 1), WH(t + 1) }
end

-- Custom XOR decryption stream
local function Yre(encrypted, key)
	local result = {}
	local d = 42
	local key_len = #key
	local idx = 0

	for i = 1, #encrypted, 2 do
		local hex_byte = string.sub(encrypted, i, i + 1)
		local byte_val = tonumber(hex_byte, 16)
		
		if byte_val then
			local key_char = string.byte(key, (idx % key_len) + 1)

			local k = byte_val ~ key_char ~ (idx * 13) ~ d
			table.insert(result, string.char(k & 0xFF))

			d = (d + byte_val) % 256
			idx = idx + 1
		end
	end

	return table.concat(result)
end

-- Decrypt the string
function Decrypt(encrypted)
	local keys = LU()
	
	for _, key in ipairs(keys) do
		local decoded = Yre(encrypted, key)
		local result = require 'fmd.crypto'.DecodeURL(decoded)

		if result and string.match(result, '^%s*[{%[]') then
			return result
		end
	end
	
	return nil
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination
	HTTP.Headers.Values['X-App-Secret'] = APP_SECRET

	if not HTTP.GET(u) then return net_problem end

	local s = Decrypt(HTTP.Document.ToString():match('"_enc_resp_":"(.-)"'))
	local x = json.decode(s)

	for _, data in ipairs(x or {}) do
		LINKS.Add('manga/' .. data.slug)
		NAMES.Add(data.title)
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MODULE.RootURL .. '/api' .. URL
	HTTP.Headers.Values['X-App-Secret'] = APP_SECRET

	if not HTTP.GET(u) then return net_problem end

	local s = Decrypt(HTTP.Document.ToString():match('"_enc_resp_":"(.-)"'))
	local info = json.decode(s)

	local term = info.term_list
	local authors = {}
	for author in string.gmatch(term, '([^:|]+):author:') do
		table.insert(authors, author)
	end

	local genres = {}
	for genre in string.gmatch(term, '([^:|]+):genre:') do
		table.insert(genres, genre)
	end
	table.insert(genres, term:match('([^:|]+):series:'))

	MANGAINFO.Title     = info.title
	MANGAINFO.AltTitles = info.alt_titles
	MANGAINFO.CoverLink = info.cover_url
	MANGAINFO.Authors   = table.concat(authors, ', ')
	MANGAINFO.Genres    = table.concat(genres, ', ')
	MANGAINFO.Status    = MangaInfoStatusIfPos(info.status, 'ongoing|publishing', 'completed|finished')
	MANGAINFO.Summary   = info.description

	for _, chapters in ipairs(info.chapters or {}) do
		MANGAINFO.ChapterLinks.Add(chapters.id)
		MANGAINFO.ChapterNames.Add(chapters.title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	HTTP.Reset()
	local u = MODULE.RootURL .. '/api/chapters' .. URL
	HTTP.Headers.Values['X-App-Secret'] = APP_SECRET

	if not HTTP.GET(u) then return false end

	local s = Decrypt(HTTP.Document.ToString():match('"_enc_resp_":"(.-)"'))
	local data = json.decode(s).content_urls
	for _, v in ipairs(data) do
		TASK.PageLinks.Add(v)
	end

	for i = 0, TASK.PageLinks.Count - 1 do
		local link = TASK.PageLinks[i]
		if string.find(link, '/upload', 1, true) and not string.find(link, '/storage/upload', 1, true) then
			TASK.PageLinks[i] = link:gsub('/upload', '/storage/upload')
		end
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end