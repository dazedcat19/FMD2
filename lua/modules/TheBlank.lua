----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/library?page='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

math.randomseed(os.time())

local PREFIX_LENGTH = 192
local STREAM_HEADER_LENGTH = 24
local CHUNK_SIZE = 65536 + 17

local function GetRandomBytes(count)
	local res = {}
	for i = 1, count do
		res[i] = string.char(math.random(0, 255))
	end
	return table.concat(res)
end

local function GetHexNonce(count)
	local res = {}
	for i = 1, count do
		res[i] = string.format('%02x', math.random(0, 255))
	end
	return table.concat(res)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(//div[@id="app"]/@data-page).props.series.meta.last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(//div[@id="app"]/@data-page).props.series.data()').Get() do
		LINKS.Add(v.GetProperty('link').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(//div[@id="app"]/@data-page).props.serie')
	MANGAINFO.Title     = x.XPathString('name', info)
	MANGAINFO.AltTitles = x.XPathString('name_alternative', info)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('cover_image', info))
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Artists   = x.XPathString('artist', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, type?name), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('status', info), 'ongoing', 'finished', 'onhold', 'dropped')
	MANGAINFO.Summary   = x.XPathString('description', info)

	local slug = x.XPathString('slug', info)
	for v in x.XPath('chapters?*[not(isPremium)]', info).Get() do
		MANGAINFO.ChapterLinks.Add(slug .. '/' .. v.GetProperty('slug').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local crypto = require 'fmd.crypto'
	local serie_slug, chapter_slug = URL:match('^/([^/]+)/([^/]+)$')
	local u = MODULE.RootURL .. '/serie/' .. serie_slug .. '/chapter/' .. chapter_slug

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local props = x.XPath('json(//div[@id="app"]/@data-page).props')
	local server_pubkey = x.XPathString('server_pubkey', props)
	local chapter_token = x.XPathString('chapter_token', props)
	local page_count = tonumber(x.XPathString('page_count', props)) or 0

	local server_pub = crypto.DecodeBase64(server_pubkey)
	local priv = GetRandomBytes(32)
	local client_pub = crypto.X25519_PublicKey(priv)
	local shared_secret = crypto.X25519_SharedSecret(priv, server_pub)

	local shared_secretB64 = crypto.EncodeBase64(shared_secret)
	local client_pubB64 = crypto.EncodeBase64(client_pub)

	for i = 1, page_count do
		local page_url = string.format('%s/serie/%s/chapter/%s/page/%d#%s;%s;%s',
			MODULE.RootURL, serie_slug, chapter_slug, i, chapter_token, shared_secretB64, client_pubB64)
		
		TASK.PageLinks.Add(page_url)
	end

	return true
end

-- Download and decrypt image given the image URL.
function DownloadImage()
	local crypto = require 'fmd.crypto'
	local serie_slug, chapter_slug, page_idx = URL:match('/serie/([^/]+)/chapter/([^/]+)/page/(%d+)')

	local fragment = URL:match('#(.+)$')

	local parts = {}
	for v in fragment:gmatch('[^;]+') do
		parts[#parts + 1] = v
	end
	
	local chapter_token = parts[1]
	local shared_secret = crypto.DecodeBase64(parts[2])
	local client_pubB64 = parts[3]

	local ts = os.time()
	local nonce = GetHexNonce(16)
	local sig_data = page_idx .. ts .. nonce
	local sig = crypto.HMAC_SHA256Hex(sig_data, chapter_token)

	local img_url = string.format('%s?token=%s&ts=%s&nonce=%s&sig=%s', URL:match('([^#]+)'), chapter_token, ts, nonce, sig)

	HTTP.Headers.Values['X-Client-Pubkey'] = client_pubB64

	if not HTTP.GET(img_url) then return false end

	local page_name = Trim(HTTP.Headers.Values['X-Page-Name'])
	local key_hintB64 = Trim(HTTP.Headers.Values['X-Key-Hint'])

	if page_name == '' or key_hintB64 == '' then return false end

	local key_hint = crypto.DecodeBase64(key_hintB64)
	local sha = crypto.SHA256(shared_secret .. page_name)

	local stream_key = ''
	for i = 1, 32 do
		local xor_byte = string.byte(sha, i) ~ string.byte(key_hint, i)
		stream_key = stream_key .. string.char(xor_byte)
	end

	local encrypted_doc = HTTP.Document.ToString()
	if #encrypted_doc < (PREFIX_LENGTH + STREAM_HEADER_LENGTH) then return false end

	local header = encrypted_doc:sub(PREFIX_LENGTH + 1, PREFIX_LENGTH + STREAM_HEADER_LENGTH)
	
	local state = crypto.SecretStream_InitPull(header, stream_key)
	if not state then return false end

	local decrypted_doc = ''
	local offset = PREFIX_LENGTH + STREAM_HEADER_LENGTH + 1
	local doc_len = #encrypted_doc

	while offset <= doc_len do
		local chunk = encrypted_doc:sub(offset, offset + CHUNK_SIZE - 1)
		if #chunk == 0 then break end

		local msg, tag
		state, msg, tag = crypto.SecretStream_Pull(state, chunk)
		if not state then return false end

		decrypted_doc = decrypted_doc .. msg
		offset = offset + #chunk

		if tag == 3 then break end 
	end

	HTTP.Document.WriteString(decrypted_doc)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd3cbc04324b349d680855130ed7c4ff3'
	m.Name                     = 'The Blank'
	m.RootURL                  = 'https://theblank.net'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
	m.MaxTaskLimit             = 2
	m.MaxConnectionLimit       = 4
	m.SortedList               = true
end