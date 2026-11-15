----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/daftar-komik?sort=newest'
local ReaderAccessEndpoint = '/reader-access.php'
local json = require 'utils.json'
local crypto = require 'fmd.crypto'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Build a random 48-character hex session id.
local function GenerateSID()
	math.randomseed(os.time() + math.floor((os.clock() % 1) * 1000000))
	local b = {}
	for i = 1, 24 do b[i] = string.format('%02x', math.random(0, 255)) end
	return table.concat(b)
end

-- Build the reader access JSON payload.
local function EncodeReaderAccessBody(sid, page, token, ticket, nonce)
	local body = { sid = sid, page = page, token = token or '' }
	if ticket then
		body.ticket = ticket
		body.nonce = nonce
	end
	return json.encode(body)
end

-- POST to the reader access endpoint and decode the JSON response.
local function ReaderAccessPost(endpoint, body)
	HTTP.Reset()
	HTTP.MimeType = 'application/json'
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['X-Reader-Access'] = '1'

	if not HTTP.POST(endpoint, body) then return nil end

	local ok, data = pcall(json.decode, HTTP.Document.ToString())
	if not ok then return nil end

	return data
end

-- Solve the challenge.
local function SolveChallenge(challenge)
	local seed = tostring(challenge.seed or '')
	local difficulty = tonumber(challenge.difficulty) or 12
	if #seed ~= 32 or not seed:match('^%x+$') or difficulty ~= 12 then return nil end

	for n = 0, 2000000 do
		if crypto.SHA256Hex(seed .. tostring(n)):sub(1, 3) == '000' then
			return tostring(n)
		end
	end

	return nil
end

-- Extract the config.
local function GetReaderAccessConfig()
	local script = HTTP.Document.ToString()
	local cfgjson = script:match('window%.__READER_ACCESS%s*=%s*(%b{})')
	if not cfgjson then return nil end

	local ok, cfg = pcall(json.decode, cfgjson)
	if not ok then return nil end
	if cfg.page == '' then return nil end
	if cfg.endpoint == '' then cfg.endpoint = ReaderAccessEndpoint end

	return cfg
end

-- Decode the grant token to know until when it can be reused.
local function GrantExpiry(grant)
	local fallback = os.time() + 3300
	local payload = grant:match('^([^.]+)%.')
	if not payload then return fallback end

	local ok, decoded = pcall(json.decode, crypto.DecodeBase64URL(payload))
	if not ok then return fallback end
	local iat = tonumber(decoded.iat)
	local ttl = tonumber(decoded.ttl)
	if not iat or not ttl then return fallback end

	return iat + ttl - 60
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local page = 1
	while true do
		UPDATELIST.UpdateStatusText('Loading page ' .. page)
		x.XPathHREFAll('//h3/a', LINKS, NAMES)
		local next_url = x.XPathString('//nav[contains(@class, "pagination")]/a[@rel="next"]/@href')
		if next_url == '' then break end
		if not HTTP.GET(next_url) then break end
		x.ParseHTML(HTTP.Document)
		page = page + 1
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1'):gsub('Bahasa Indonesia$', '')
	MANGAINFO.AltTitles = x.XPathString('//p[@class="comic-alt-title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "comic-cover")]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//div[span="Author"]/span[2]')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="comic-genres"]/a, //div[span="Tipe"]/span[2])')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="comic-cover-badges-right"]'), 'Ongoing', 'Tamat')
	MANGAINFO.Summary   = x.XPathString('//div[@class="comic-synopsis"]/div')

	x.XPathHREFAll('//div[@class="chapter-grid"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local rev = tonumber(require 'fmd.env'.Revision)
	if rev < 7007 then
		print('Require FMD2 revision 7007 and later')
		print('Revision ' .. rev .. ' detected')
		return false
	end
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local cfg = GetReaderAccessConfig()
	if not cfg then return false end
	local endpoint = MODULE.RootURL .. cfg.endpoint

	local sid   = MODULE.Storage['reader_sid']
	local grant = MODULE.Storage['reader_grant']
	local expiry = tonumber(MODULE.Storage['reader_grant_exp']) or 0
	if sid == '' or grant == '' or os.time() > expiry then
		sid = GenerateSID()
		grant = ''
	end

	for attempt = 1, 2 do
		local data = ReaderAccessPost(endpoint, EncodeReaderAccessBody(sid, cfg.page, grant))
		if not data then return false end

		if data.challenge then
			grant = ''
			local nonce = SolveChallenge(data.challenge)
			if not nonce then return false end
			data = ReaderAccessPost(endpoint, EncodeReaderAccessBody(sid, cfg.page, '', data.challenge.ticket, nonce))
			if not data then return false end
		end

		if #data.images > 0 then
			if data.grant and data.grant.token ~= '' then
				MODULE.Storage['reader_sid'] = sid
				MODULE.Storage['reader_grant'] = data.grant.token
				MODULE.Storage['reader_grant_exp'] = GrantExpiry(data.grant.token)
			end
			for _, v in ipairs(data.images) do
				TASK.PageLinks.Add(v)
			end
			return true
		end

		if data.error ~= 'page_expired' or attempt == 2 then return false end

		if not HTTP.GET(u) then return false end
		cfg = GetReaderAccessConfig()
		if not cfg then return false end
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '463da66c3229438e9908e60dc86d71ec'
	m.Name                     = 'CGBum'
	m.RootURL                  = 'https://cgbum.com'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end