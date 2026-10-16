----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local Template      = require 'templates.MangaThemesia'
local DirectoryPage = '/lista'
XPathTokenStatus    = 'Estado'

----------------------------------------------------------------------------------------------------
-- Local Functions
----------------------------------------------------------------------------------------------------

-- Extract the page image URLs from the encrypted 'ReaderBlob'.
local function GetReaderBlobPages(s)
	local crypto = require 'fmd.crypto'
	local json = require 'utils.json'
	local blob = s:match('ReaderBlob%.start%((%b{})%)')
	if not blob or blob == '' then return nil end

	local ok, b = pcall(json.decode, blob)
	if not ok then return nil end

	local pt = crypto.AESDecryptGCM(crypto.DecodeBase64(b.c), crypto.DecodeBase64(b.k), crypto.DecodeBase64(b.iv))
	if pt == '' then return nil end

	local ok2, d = pcall(json.decode, pt)
	if not ok2 then return nil end

	local skip = {}
	for _, idx in ipairs(d.s) do skip[tonumber(idx) or -1] = true end

	local pages = {}
	for i, img in ipairs(d.p) do
		if not skip[i - 1] and img ~= '' then
			pages[#pages + 1] = img
		end
	end

	return pages
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPage

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="bsx"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.AltTitles = x.XPathStringAll('//div[b="Título Alternativos"]//a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[b="Generos:"]//a')
	MANGAINFO.Summary   = x.XPathString('//div[b="Sinopsis"]/span')

	for v in x.XPath('//div[@id="chapterlist"]//div[@class="chbox"]').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('div[@class="dt"]/a/@href', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('div[@class="eph-num"]/span[@class="chapternum"][1]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local rev = tonumber(require 'fmd.env'.Revision)
	if rev < 6944 then
		print('Require FMD2 revision 6944 and later')
		print('Revision ' .. rev .. ' detected')
		return false
	end
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local s = x.XPathString('//script[contains(., "p,a,c,k,e,d")]')
	if s == '' then return false end

	local packed = s:match('^%s*eval(.+)$')
	local unpacked = require 'fmd.duktape'.ExecJS(packed)

	local pages = GetReaderBlobPages(unpacked)
	if not pages or #pages == 0 then return false end

	for _, img in ipairs(pages) do
		TASK.PageLinks.Add(img)
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd78d163c51b24bc487e777e2b0d811e3'
	m.Name                     = 'MangaTV'
	m.RootURL                  = 'https://mangatv.net'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end
