----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, url, cat)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = 'Bato.To'
		m.RootURL                  = url
		m.Category                 = cat
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.SortedList               = true
		return m
	end

	local function AddWebsiteModuleV4(id, url, cat)
		local m = AddWebsiteModule(id, url, cat)
		m.Name                     = 'Bato.To V4'
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumberV4'
		m.OnGetNameAndLink         = 'GetNameAndLinkV4'
		m.OnGetInfo                = 'GetInfoV4'
		m.OnGetPageNumber          = 'GetPageNumberV4'
	end

	AddWebsiteModule('5257a0c426b94accb6dcee3101308314', 'https://bato.to', 'English')
	AddWebsiteModule('41e43d6fa1434937afad3bc04a1e8603', 'https://batotoo.com')
	AddWebsiteModule('53347251db9d4d5eb92ef8bc6101e5f7', 'https://battwo.com')
	AddWebsiteModule('cf8702f7f5d24bd2a1b9b9904beb246b', 'https://mangatoto.com')
	AddWebsiteModule('c7908a2cdb0c4966bff604ebedc9f468', 'https://wto.to')
	AddWebsiteModule('4040307fbc04489587bb71ffcefb3ccf', 'https://mto.to')
	AddWebsiteModule('02e8d0899c8b48c8bfdde57e5e3b8f38', 'https://batotwo.com')
	AddWebsiteModule('24bdc3fed5e343e89b4ee4448d9389be', 'https://readtoto.org')
	AddWebsiteModule('c2f6082e637841b8b1031786994d4d5b', 'https://xbato.com')
	AddWebsiteModule('ad4c8809dba94062bad8ee75d8de4e1c', 'https://zbato.org')
	AddWebsiteModuleV4('ac808ac813a3499baa65bf640519ed59', 'https://bato.si', 'English')
	AddWebsiteModuleV4('52c9306a3b93482ea3145c9e619b67fa', 'https://bato.ing')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/browse?sort=create.za&page='
local API_URL = '/ap2/'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Get the language suffix by the given flag.
local function GetLanguageCodeSuffix(s)
	if not s or s == '' then
		return ' [EN]'
	end

	return ' [' .. string.upper(s) .. ']'
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//ul[contains(@class, "pagination")])[2]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@id="series-list"]/div/div').Get() do
		LINKS.Add('series/' .. x.XPathString('a/@href', v):match('/(%d+)'))
		NAMES.Add(x.XPathString('a', v) .. GetLanguageCodeSuffix(x.XPathString('em/@data-lang', v)))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MODULE.RootURL .. '/series/' .. URL:match('/(%d+)')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h3[contains(@class, "item-title")]/a') .. GetLanguageCodeSuffix(x.XPathString('//h3[contains(@class, "item-title")]/parent::*/em/@data-lang'))
	MANGAINFO.AltTitles = x.XPathString('//div[contains(@class, "alias-set")]/normalize-space(.)')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "attr-cover")]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="attr-item" and (./b="Authors:")]/span/a')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@class="attr-item" and (./b="Artists:")]/span/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="attr-item" and (./b="Genres:")]/span/span')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="attr-item" and (./b="Upload status:")]/span'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="limit-html"]')

	x.XPathHREFAll('//div[contains(@class, "episode-list")]/div[@class="main"]/div/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end
	
	local x = CreateTXQuery(HTTP.Document)
	local script = x.XPathString('//script[contains(., "const batoPass")]')
	local ext = require('fmd.duktape').ExecJS(script .. [[

	var CryptoJS = require("utils/crypto-js.min.js");
	JSON.parse(CryptoJS.AES.decrypt(batoWord, batoPass).toString(CryptoJS.enc.Utf8));

	]])
	local delimiter = ','
	ext = ext .. delimiter
	local images = script:match('const imgHttps = %[([^%]]+)'):gsub('https://k', 'https://n')
	for image in images:gmatch('"([^",]+)') do
		if ext ~= ',' then
			local mtch = ext:match('(.-)' .. delimiter)
			TASK.PageLinks.Add(image .. '?' .. mtch)
			ext = ext:gsub((mtch .. delimiter):gsub('-', '%%-'), '')
		else
			TASK.PageLinks.Add(image)
		end
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Version 4
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumberV4()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_comic_browse_pager($select: Comic_Browse_Select) { get_comic_browse_pager( select: $select ) { pages } }","variables":{"select":{"where":"browse","size":120,"sortby":"field_create"}}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).data.get_comic_browse_pager.pages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLinkV4()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_comic_browse($select: Comic_Browse_Select) { get_comic_browse( select: $select ) { items { data { id name } } } }","variables":{"select":{"where":"browse","page":' .. (URL + 1) .. ',"size":120,"sortby":"field_create"}}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.get_comic_browse.items().data').Get() do
		LINKS.Add('title/' .. v.GetProperty('id').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfoV4()
	local mid = URL:match('/(%d+)')
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_comicNode($id: ID!) { get_comicNode(id: $id) { data { name altNames urlCoverOri authors artists genres uploadStatus originalStatus summary } } }","variables":{"id":"' .. mid .. '"}}'
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local json = x.XPath('json(*).data.get_comicNode.data')
	MANGAINFO.Title     = x.XPathString('name', json)
	MANGAINFO.AltTitles = x.XPathString('string-join(altNames?*, ", ")', json)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('urlCoverOri', json))
	MANGAINFO.Authors   = x.XPathString('string-join(authors?*, ", ")', json)
	MANGAINFO.Artists   = x.XPathString('string-join(artists?*, ", ")', json)
	MANGAINFO.Genres    = x.XPathString('string-join(genres?*, ", ")', json):gsub('_', ' '):gsub('(%l)(%w*)', function(first, rest) return first:upper() .. rest end)
	MANGAINFO.Summary   = x.XPathString('summary', json)

	local status = x.XPathString('uploadStatus', json)
	if status == 'null' then status = x.XPathString('originalStatus', json) end
	MANGAINFO.Status = MangaInfoStatusIfPos(status)

	local s = '{"query":"query get_comic_chapterList($comicId: ID!, $start: Int) { get_comic_chapterList(comicId: $comicId, start: $start) { data { id dname title } } }","variables":{"comicId":"' .. mid .. '","start":-1}}'
	HTTP.Reset()
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.get_comic_chapterList().data').Get() do
		local chapter = v.GetProperty('dname').ToString()
		local title = v.GetProperty('title').ToString()
		title = (title ~= 'null' and title ~= '') and (' - ' .. title) or ''

		MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(chapter .. title)
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumberV4()
	local u = MODULE.RootURL .. API_URL
	local s = '{"query":"query get_chapterNode($id: ID!) { get_chapterNode(id: $id) { data { imageFile { urlList } } } }","variables":{"id":"' .. URL:match('(%d+)') .. '"}}'
	HTTP.Reset()
	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.get_chapterNode.data.imageFile.urlList()', TASK.PageLinks)

	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub('https://k', 'https://n')
	end

	return true
end