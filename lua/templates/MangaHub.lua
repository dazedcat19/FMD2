----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.mghcdn.com/graphql'
local CRYPTO_URL = '/api/chapter-crypto'
local CDN_URL = 'https://imgx.mghcdn.com/'
local MangaPerPage = 50
local json = require 'utils.json'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

math.randomseed(os.time())

local function RandomHex()
	local hex = ''
	for i = 1, 16 do
		hex = hex .. string.format('%02x', math.random(0, 255))
	end
	return hex
end

-- Set the required http headers for making a request.
local function SetRequestHeaders(token)
	HTTP.Headers.Values['X-Mhub-Access'] = token
	HTTP.Cookies.Values['mhub_access'] = token
	HTTP.MimeType = 'application/json'
end

-- Decode the encrypted chapter pages field.
local function DecryptPages(enc, key)
	local crypto = require 'fmd.crypto'
	if enc:sub(1, 7) ~= 'enc:v1:' then
		return enc
	end
	local parts = {}
	for v in enc:gmatch('[^:]+') do
		parts[#parts + 1] = v
	end
	local iv = crypto.DecodeBase64URL(parts[4])
	local tag = crypto.DecodeBase64URL(parts[5])
	local cip = crypto.DecodeBase64URL(parts[6])
	local key_bytes = crypto.DecodeBase64URL(key)
	return crypto.AESDecryptGCM(cip .. tag, key_bytes, iv)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local s = '{"query":"{search(x:' .. source .. ',q:\\"\\",genre:\\"all\\",mod:ALPHABET,limit:' .. MangaPerPage .. ',count:true,offset:0){count}}"}'
	SetRequestHeaders(RandomHex())

	if not HTTP.POST(API_URL, s) then return net_problem end

	PAGENUMBER = math.ceil(json.decode(HTTP.Document.ToString()).data.search.count / MangaPerPage)

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local offset = MangaPerPage * URL
	local s = '{"query":"{search(x:' .. source .. ',q:\\"\\",genre:\\"all\\",mod:ALPHABET,limit:' .. MangaPerPage .. ',count:true,offset:' .. offset .. '){rows{title,slug}}}"}'
	SetRequestHeaders(RandomHex())

	if not HTTP.POST(API_URL, s) then return net_problem end

	local data = json.decode(HTTP.Document.ToString()).data.search.rows
	for _, v in ipairs(data) do
		LINKS.Add('manga/' .. v.slug)
		NAMES.Add(v.title)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local s = '{"query":"{manga(x:' .. source .. ',slug:\\"' .. URL:match('manga/(.-)$') .. '\\"){title,slug,status,image,author,artist,genres,description,alternativeTitle,chapters{number,title}}}"}'
	SetRequestHeaders(RandomHex())

	if not HTTP.POST(API_URL, s) then return net_problem end

	local x = json.decode(HTTP.Document.ToString())
	if x.errors then MANGAINFO.Title = 'Error: ' .. x.errors[1].message return no_error end
	MANGAINFO.Title     = x.data.manga.title
	MANGAINFO.AltTitles = x.data.manga.alternativeTitle
	MANGAINFO.CoverLink = 'https://thumb.mghcdn.com/' .. x.data.manga.image
	MANGAINFO.Authors   = x.data.manga.author
	MANGAINFO.Artists   = x.data.manga.artist
	MANGAINFO.Genres    = x.data.manga.genres
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.data.manga.status)
	MANGAINFO.Summary   = x.data.manga.description

	local slug = x.data.manga.slug
	for _, v in ipairs(x.data.manga.chapters) do
		local title = v.title:gsub('[\n\t]+', ' ')
		local number = v.number

		title = title:find(number, 1, true) and title or 'Chapter ' .. number .. (title ~= '' and ' - ' .. title or '')

		MANGAINFO.ChapterLinks.Add(slug .. '/chapter-' .. number)
		MANGAINFO.ChapterNames.Add(title)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	if tonumber(require 'fmd.env'.Revision) < 6920 then
		print('Require unreleased FMD2 build')
		return false
	end
	local slug, chapter = URL:match('/([^/]+)/chapter%-([%d.]+)$')
	local token = RandomHex()

	HTTP.Reset()
	SetRequestHeaders(token)
	if not HTTP.GET(MODULE.RootURL .. CRYPTO_URL) then return false end

	local crypto = json.decode(HTTP.Document.ToString())
	if not crypto or not crypto.key then
		print('MangaHub: could not obtain chapter crypto key')
		return false
	end

	local s = '{"query":"{chapter(x:' .. source .. ',slug:\\"' .. slug .. '\\",number:' .. chapter .. '){pages}}"}'
	HTTP.Reset()
	SetRequestHeaders(token)

	if not HTTP.POST(API_URL, s) then return false end

	local x = json.decode(HTTP.Document.ToString())
	if x.errors then 
		print('Error: ' .. x.errors[1].message)
		return false
	end

	local plain = DecryptPages(x.data.chapter.pages, crypto.key)
	local w = json.decode(plain)
	local p = w.p
	for _, v in ipairs(w.i) do
		TASK.PageLinks.Add(CDN_URL .. p .. v)
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M