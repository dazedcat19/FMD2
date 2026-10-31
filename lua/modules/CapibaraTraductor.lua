----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local LOGIN_URL       = '/api/auth/login'
local RV_MASK         = { 60, 110, 243, 114 }
local PAGE_MULTIPLIER = 2654435761
local URL_FIELD_KEY   = 2135587861
local DirectoryPages  = '/api/manga-custom?limit=1000&type=manga,manhwa,manhua&order=newest&page='

----------------------------------------------------------------------------------------------------
-- Local Functions
----------------------------------------------------------------------------------------------------

-- Mulberry32 PRNG with 32-bit wrapping arithmetic.
local function Mulberry32(seed)
	local state = seed & 0xFFFFFFFF
	return function()
		state = (state + 0x6D2B79F5) & 0xFFFFFFFF
		local t = state
		t = ((t ~ (t >> 15)) * (t | 1)) & 0xFFFFFFFF
		t = t ~ ((t + (((t ~ (t >> 7)) * (t | 61)) & 0xFFFFFFFF)) & 0xFFFFFFFF)
		return (t ~ (t >> 14)) >> 24
	end
end

-- URL-safe Base64 -> raw bytes.
local function DecodeBase64URL(s)
	s = s:gsub('%-', '+'):gsub('_', '/')
	local rem = #s % 4
	if rem == 2 then s = s .. '=='
	elseif rem == 3 then s = s .. '=' end

	return require 'fmd.crypto'.DecodeBase64(s)
end

-- Decrypt a page url token with the rv seed of the chapter pages response.
local function DecodePageUrl(token, rv, index)
	local rv_bytes = DecodeBase64URL(rv)
	if #rv_bytes < 8 then return nil end

	local seed = 0
	for i = 0, 3 do
		seed = ((seed << 8) | (rv_bytes:byte(i * 2 + 1) ~ RV_MASK[i + 1])) & 0xFFFFFFFF
	end

	local data = DecodeBase64URL(token)
	if #data < 3 then return nil end

	local next_byte = Mulberry32(seed ~ (((index) * PAGE_MULTIPLIER) & 0xFFFFFFFF) ~ URL_FIELD_KEY)

	local checksum = 90
	local bytes = {}
	for i = 3, #data do
		local b = (data:byte(i) ~ next_byte()) & 0xFF
		checksum = (checksum + b * (i - 2)) & 0xFF
		bytes[#bytes + 1] = string.char(b)
	end

	if ((data:byte(2) ~ next_byte()) & 0xFF) ~= checksum then return nil end

	return table.concat(bytes)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Sign in to the current website.
function Login()
	local crypto = require 'fmd.crypto'
	local u = MODULE.RootURL .. LOGIN_URL

	if not MODULE.Account.Enabled then return false end

	local s = 'email=' .. crypto.EncodeURLElement(MODULE.Account.Username) ..
		'&password=' .. crypto.EncodeURLElement(MODULE.Account.Password)
	MODULE.Account.Status = asChecking

	if HTTP.POST(u, s) then
		if HTTP.ResultCode == 200 then
			MODULE.Account.Cookies = CreateTXQuery(HTTP.Document).XPathString('json(*).data.token')
			MODULE.Account.Status = asValid
			return true
		else
			MODULE.Account.Status = asInvalid
			return false
		end
	else
		MODULE.Account.Status = asUnknown
		return false
	end
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPages .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*).data.maxPage')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPages .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).data.items()').Get() do
		LINKS.Add(v.GetProperty('organization').GetProperty('slug').ToString() .. '/manga/' .. v.GetProperty('manga').GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local site, slug = URL:match('/([^/]+)/manga/([^/]+)')
	local endpoint = URL:find('/joint/', 1, true) and 'joint' or 'manga-custom'
	::endpoint::
	local u = MODULE.RootURL .. '/api/' .. endpoint .. '/' .. slug

	HTTP.Headers.Values['X-Organization'] = site

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('parse-json(.)?data')
	local joint_slug = x.XPathString('jointSlug', info)
	if joint_slug ~= '' then
		endpoint = 'joint'
		site = 'joint'
		slug = joint_slug
		MANGAINFO.Link = '/' .. endpoint .. '/manga/' .. slug
		MANGAINFO.URL  = MODULE.RootURL .. '/' .. endpoint .. '/manga/' .. slug
		goto endpoint
	end

	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('alternativeTitle', info)
	MANGAINFO.CoverLink = x.XPathString('imageUrl', info)
	MANGAINFO.Authors   = x.XPathString('string-join(manga?authors?*?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?name, manga?demography?name, manga?bookType?name), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('replace(status, "finished", "completed")', info))
	MANGAINFO.Summary   = x.XPathString('description', info)

	for v in x.XPath('chapters?*[not(isUnreleased=true)]', info).Get() do
		local number = v.GetProperty('number').ToString()
		local title = v.GetProperty('title').ToString()

		title = (title ~= '') and not title:find('Capítulo ' .. number, 1, true) and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(site .. '/' .. slug .. '/' .. number)
		MANGAINFO.ChapterNames.Add('Capítulo ' .. number .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	if MODULE.Account.Enabled and MODULE.Account.Cookies == '' then Login() end
	local is_joint = URL:find('/joint/', 1, true)
	local site, slug, cid = URL:match('^/([^/]+)/([^/]+)/([^/]+)$')

	local endpoint = is_joint and 'joint' or 'manga-custom'
	local u = MODULE.RootURL .. '/api/' .. endpoint .. '/' .. slug .. '/chapter/' .. cid .. '/pages'

	::request::

	HTTP.Reset()
	HTTP.Headers.Values['X-Organization'] = site
	HTTP.Headers.Values['Authorization'] = 'Bearer ' .. MODULE.Account.Cookies

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPathString('json(*).errorType') == 'login_required' then
		if MODULE.Account.Enabled then
			MODULE.Account.Cookies = ''
			if Login() then goto request end
		end
		return false
	end

	local rv = x.XPathString('json(*).rv')
	if rv == '' or rv == 'null' then return false end

	local i = 1
	for v in x.XPath('json(*).data()').Get() do
		TASK.PageLinks.Add(DecodePageUrl(v.GetProperty('pg').ToString(), rv, i))
		i = i + 1
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '97dcd409f781467eb2da4a8634e5a1d9'
	m.Name                     = 'CapibaraTraductor'
	m.RootURL                  = 'https://capibaratraductor.com'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnLogin                  = 'Login'
	m.AccountSupport           = true
	m.SortedList               = true
end