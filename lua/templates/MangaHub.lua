----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local API_URL = '/graphql'
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

local function log_debug(msg)
	local f = io.open("mhub_debug.log", "a")
	if f then
		f:write(os.date('%Y-%m-%d %H:%M:%S') .. ' ' .. tostring(msg) .. '\n')
		f:close()
	end
end

-- Set the required http headers for making a request.
local function SetRequestHeaders(token)
	if HTTP.UserAgent == '' then
		HTTP.UserAgent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36'
	end
	HTTP.Headers.Values['X-Mhub-Access'] = token
	HTTP.Cookies.Values['mhub_access'] = token
	HTTP.MimeType = 'application/json'
end

-- Decode the encrypted chapter pages field.
local function DecryptPages(enc, key)
	if enc:sub(1, 7) ~= 'enc:v1:' then
		return enc
	end
	local parts = {}
	for v in enc:gmatch('[^:]+') do
		parts[#parts + 1] = v
	end
	local crypto = require 'fmd.crypto'
	if crypto.AESDecryptGCM and crypto.DecodeBase64URL then
		local iv = crypto.DecodeBase64URL(parts[4])
		local tag = crypto.DecodeBase64URL(parts[5])
		local cip = crypto.DecodeBase64URL(parts[6])
		local key_bytes = crypto.DecodeBase64URL(key)
		local ok, res = pcall(crypto.AESDecryptGCM, cip .. tag, key_bytes, iv)
		if ok and res and res ~= '' then
			return res
		end
	end

	-- Fallback for FMD builds that lack native AESDecryptGCM (uses Node.js crypto)
	local subprocess = require 'fmd.subprocess'
	local js_code = "const c=require('crypto');const [k,iv,t,cp]=process.argv.slice(1);const d=c.createDecipheriv('aes-256-gcm',Buffer.from(k,'base64url'),Buffer.from(iv,'base64url'));d.setAuthTag(Buffer.from(t,'base64url'));process.stdout.write(Buffer.concat([d.update(Buffer.from(cp,'base64url')),d.final()]))"

	local ok, out, err = subprocess.RunCommandHide('node', '-e', js_code, key, parts[4], parts[5], parts[6])
	if not ok or not out or out == '' or out:find('Error') then
		ok, out, err = subprocess.RunCommandHide('cmd.exe', '/c', 'node', '-e', js_code, key, parts[4], parts[5], parts[6])
	end
	log_debug(string.format('DecryptPages Node result: ok=%s, out_len=%s, err=%s', tostring(ok), tostring(out and #out or 0), tostring(err)))
	if ok and out and out ~= '' and not out:find('Error') then
		return out
	end

	return enc
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local s = '{"query":"{search(x:' .. source .. ',q:\\"\\",genre:\\"all\\",mod:ALPHABET,limit:' .. MangaPerPage .. ',count:true,offset:0){count}}"}'
	SetRequestHeaders(RandomHex())

	if not HTTP.POST(MODULE.RootURL .. API_URL, s) then return net_problem end

	PAGENUMBER = math.ceil(json.decode(HTTP.Document.ToString()).data.search.count / MangaPerPage)

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local offset = MangaPerPage * URL
	local s = '{"query":"{search(x:' .. source .. ',q:\\"\\",genre:\\"all\\",mod:ALPHABET,limit:' .. MangaPerPage .. ',count:true,offset:' .. offset .. '){rows{title,slug}}}"}'
	SetRequestHeaders(RandomHex())

	if not HTTP.POST(MODULE.RootURL .. API_URL, s) then return net_problem end

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

	if not HTTP.POST(MODULE.RootURL .. API_URL, s) then return net_problem end

	local ok, x = pcall(json.decode, HTTP.Document.ToString())
	if not ok then return net_problem end
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
	log_debug("GetPageNumber called for URL: " .. tostring(URL))
	local slug, chapter = URL:match('([^/]+)/chapter%-([%d.]+)$')
	log_debug(string.format("Parsed slug=%s, chapter=%s", tostring(slug), tostring(chapter)))
	if not slug or not chapter then
		log_debug("Failed to parse slug or chapter!")
		return false
	end
	local token = RandomHex()

	HTTP.Reset()
	SetRequestHeaders(token)

	if not HTTP.GET(MODULE.RootURL .. CRYPTO_URL) then
		log_debug("HTTP.GET crypto failed! ResultCode: " .. tostring(HTTP.ResultCode))
		return false
	end

	local ok, crypto = pcall(json.decode, HTTP.Document.ToString())
	if not ok or not crypto or not crypto.key then
		log_debug("Failed to decode crypto key: " .. tostring(HTTP.Document.ToString():sub(1, 100)))
		return false
	end

	local s = '{"query":"{chapter(x:' .. source .. ',slug:\\"' .. slug .. '\\",number:' .. chapter .. '){pages}}"}'

	HTTP.Reset()
	SetRequestHeaders(token)

	if not HTTP.POST(MODULE.RootURL .. API_URL, s) then
		log_debug("HTTP.POST chapter failed! ResultCode: " .. tostring(HTTP.ResultCode))
		return false
	end

	local ok, x = pcall(json.decode, HTTP.Document.ToString())
	if not ok or not x or x.errors then 
		log_debug("GraphQL error: " .. tostring(HTTP.Document.ToString():sub(1, 100)))
		return false
	end

	local plain = DecryptPages(x.data.chapter.pages, crypto.key)
	local ok_j, w = pcall(json.decode, plain)
	if not ok_j or not w or not w.p or not w.i then
		log_debug("Failed to decode decrypted json: " .. tostring(plain:sub(1, 100)))
		return false
	end
	local p = w.p
	for _, v in ipairs(w.i) do
		TASK.PageLinks.Add(CDN_URL .. p .. v)
	end
	log_debug(string.format("Successfully added %d page links!", #w.i))

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M