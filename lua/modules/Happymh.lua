----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local secret = 'PRO_SCAN_SECRET_20260712_watching_you_DEBUG'
local domain = 'happymh.com'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function DecodeScans(encrypted_scans)
	local crypto = require 'fmd.crypto'
	local prefix = encrypted_scans:sub(1, 8)
	local digest = crypto.SHA256(secret .. prefix .. domain)

	local d1 = digest:byte(1)
	local d2 = digest:byte(2)
	local d3 = digest:byte(3)

	local off1 = (d1 & 0xFF) % 24 + 8
	local off2 = (d2 & 0xFF) % 24 + 8
	local off3 = (d3 & 0xFF) % 24 + 8

	local key_hex = encrypted_scans:sub(off1 + 8 + 1, off1 + 72)
	local nonce_hex = encrypted_scans:sub(off1 + 72 + off2 + 1, off1 + 72 + off2 + 32)
	local cipher_b64 = encrypted_scans:sub(off1 + 72 + off2 + 32 + off3 + 1)

	local key = crypto.HexToStr(key_hex)
	local nonce = crypto.HexToStr(nonce_hex)
	local cipher = crypto.DecodeBase64(cipher_b64)

	local state_prefix = key .. nonce
	local out = {}
	local len = #cipher

	for i = 0, len - 1, 32 do
		local block_idx = math.floor(i / 32)
		local counter_bytes = string.char(
			(block_idx >> 24) & 0xFF,
			(block_idx >> 16) & 0xFF,
			(block_idx >> 8) & 0xFF,
			block_idx & 0xFF
		)

		local state = state_prefix .. counter_bytes
		local keystream = crypto.SHA256(state)
		local block_size = math.min(32, len - i)

		for j = 1, block_size do
			local c_byte = cipher:byte(i + j)
			local k_byte = keystream:byte(j)
			out[#out + 1] = string.char(c_byte ~ k_byte)
		end
	end

	local plain = table.concat(out)

	if plain:sub(1, 4) ~= 'SC01' then
		error('Decrypting scans failed')
	end

	return require 'fmd.gzip'.Inflate(plain:sub(5))
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local ts = os.time() * 1000
	local u = MODULE.RootURL .. '/apis/c/index?pn=1&_t=' .. ts
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/latest'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local page = 1
	while true do
		local series = x.XPath('json(*).data.items()')
		for v in series.Get() do
			LINKS.Add('manga/' .. v.GetProperty('manga_code').ToString())
			NAMES.Add(v.GetProperty('name').ToString())
		end
		UPDATELIST.UpdateStatusText('Loading page ' .. page)
		if series.Count == 0 then break end
		page = page + 1
		HTTP.Reset()
		HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/latest'
		if not HTTP.GET(MODULE.RootURL .. '/apis/c/index?pn=' .. page .. '&_t=' .. ts) then break end
		x.ParseHTML(HTTP.Document)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('/([^/]+)$')
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h2')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="mg-cover"]/mip-img/@src')
	MANGAINFO.Summary   = x.XPathString('//mip-showmore[@id="showmore"]')

	local page = 1
	local pages = nil
	local ts = os.time() * 1000
	while true do
		if not HTTP.GET(MODULE.RootURL .. '/v2.0/apis/manga/chapterByPage?code=' .. mid .. '&lang=cn&order=asc&page=' .. page .. '&_t=' .. ts) then return net_problem end
		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(*).data.items()').Get() do
			MANGAINFO.ChapterLinks.Add(mid .. '/' .. v.GetProperty('id').ToString())
			MANGAINFO.ChapterNames.Add(v.GetProperty('chapterName').ToString())
		end
		if not pages then
			pages = math.ceil(x.XPathString('json(*).data.total') / 50) or 1
		end
		if page >= pages then break end
		page = page + 1
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local json = require 'utils.json'
	local mid, cid = URL:match('^/([^/]+)/([^/]+)$')
	local ts = os.time() * 1000
	local u = MODULE.RootURL .. '/v2.0/apis/manga/reading?code=' .. mid .. '&cid=' .. cid .. '&v=v4.300102&_t=' .. ts

	HTTP.Reset()
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'

	if not HTTP.GET(u) then return false end

	local data = json.decode(HTTP.Document.ToString()).data
	local scans = data.scans

	if data.isEncode then
		scans = DecodeScans(scans)
	end

	local pages = json.decode(scans)
	for _, page in ipairs(pages) do
		if page.n == 0 then
			TASK.PageLinks.Add(page.url)
		end
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'b9c93a1a453943efa8834fac7cb302f5'
	m.Name                     = 'Happymh'
	m.RootURL                  = 'https://m.' .. domain
	m.Category                 = 'Raw'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end