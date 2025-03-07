----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'English'
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.SortedList               = true
	end
	AddWebsiteModule('5257a0c426b94accb6dcee3101308314', 'Batoto', 'https://bato.to')
	AddWebsiteModule('41e43d6fa1434937afad3bc04a1e8603', 'Batotoo', 'https://batotoo.com')
	AddWebsiteModule('53347251db9d4d5eb92ef8bc6101e5f7', 'Battwo', 'https://battwo.com')
	AddWebsiteModule('cf8702f7f5d24bd2a1b9b9904beb246b', 'Mangatoto', 'https://mangatoto.com')
	AddWebsiteModule('c7908a2cdb0c4966bff604ebedc9f468', 'Wto', 'https://wto.to')
	AddWebsiteModule('4040307fbc04489587bb71ffcefb3ccf', 'Mto', 'https://mto.to')
	AddWebsiteModule('02e8d0899c8b48c8bfdde57e5e3b8f38', 'Batotwo', 'https://batotwo.com')
	AddWebsiteModule('24bdc3fed5e343e89b4ee4448d9389be', 'Readtoto', 'https://readtoto.org')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/browse?sort=create.za&page='

----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

-- Get the language suffix by given flag.
function GetLanguageCodeSuffix(s)
	local suffix = ' [EN]'

	if s and (s ~= '') then
		if s and (s ~= 'en') then suffix = ' [' .. string.upper(s) .. ']' end
	end

	return suffix
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
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@id="series-list"]/div/div').Get() do
		LINKS.Add(x.XPathString('a/@href', v):gsub('(/series/%d+).*', '%1'))
		NAMES.Add(x.XPathString('a', v) .. GetLanguageCodeSuffix(x.XPathString('em/@data-lang', v)))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL):gsub('(/series/%d+).*', '%1')

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
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
	local delimiter, ext, image, images, mtch, script, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end
	
	x = CreateTXQuery(HTTP.Document)
	script = x.XPathString('//script[contains(.,"const batoPass")]')
	ext = require("fmd.duktape").ExecJS(script .. [[

	var CryptoJS = require("utils/crypto-js.min.js");
	JSON.parse(CryptoJS.AES.decrypt(batoWord, batoPass).toString(CryptoJS.enc.Utf8));

	]])
	delimiter = ','
	ext = ext .. delimiter
	images = script:match('const imgHttps = %[([^%]]+)')
	mtch = ''
	for image in images:gmatch('"([^",]+)') do
		if ext ~= ',' then
			mtch = ext:match("(.-)" .. delimiter)
			TASK.PageLinks.Add(image .. "?" .. mtch)
			ext = ext:gsub((mtch .. delimiter):gsub("-", "%%-"), "")
		else
			TASK.PageLinks.Add(image)
		end
	end

	return no_error
end