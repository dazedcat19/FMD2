----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'eb27e424af1e4ca987aba1f332df952c'
	m.Name                     = 'Madokami'
	m.RootURL                  = 'https://manga.madokami.al'
	m.Category                 = 'English'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.MaxTaskLimit             = 1
	m.MaxConnectionLimit       = 4
	m.AccountSupport           = true
	m.OnLogin                  = 'Login'
	m.OnAccountState           = 'AccountState'

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['delay'] = 'Delay (s) between requests',
		},
		['id_ID'] = {
			['delay'] = 'Tunda (detik) antara permintaan',
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionSpinEdit('mdkm_delay', lang:get('delay'), 2)
	m.Storage['madokamiulist'] = ''
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------
local json   = require("utils.json")
local madokamilist_chr = {}
local madokamilist_custom = {'_', 'Oneshots'}
-- Add A to Z
for i = string.byte('A'), string.byte('Z') do
	madokamilist_chr[string.char(i) .. '/'] = true
end
-- Add Custom character
for _, value in ipairs(madokamilist_custom) do
	madokamilist_chr[value .. '/'] = true
end
----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------
local function CheckAuth()
    AccountState()
	HTTP.GET(MODULE.RootURL)
    if HTTP.Headers.Values['WWW-Authenticate'] ~= '' then
	    HTTP.GET(MODULE.RootURL)
	    if HTTP.ResultCode ~= 200 then
            Login()
		end
	end
end

local function Delay()
	local lastDelay = tonumber(MODULE.Storage['lastDelay']) or 1
	local mdkm_delay = tonumber(MODULE.GetOption('mdkm_delay')) or 2 -- * MODULE.ActiveConnectionCount
	if lastDelay ~= '' then
		lastDelay = os.time() - lastDelay
		if lastDelay < mdkm_delay then
			sleep((mdkm_delay - lastDelay) * 1000)
		end
	end
	MODULE.Storage['lastDelay'] = os.time()
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function Login()
    MODULE.ClearCookies()
	MODULE.Account.Status = asChecking
	local login_url=MODULE.RootURL
	local crypto = require 'fmd.crypto'
	if not HTTP.GET(login_url) then
		MODULE.Account.Status = asUnknown
		return net_problem
	end
	
	HTTP.Reset()

	HTTP.Headers.Values['Origin'] = ' ' .. MODULE.RootURL
	HTTP.Headers.Values['Referer'] = ' ' .. login_url
	HTTP.Headers.Values['Accept'] = ' */*'
	HTTP.Headers.Values['Authorization'] = "Basic " .. (crypto.EncodeBase64(MODULE.Account.Username ..":" .. MODULE.Account.Password))

	HTTP.GET(login_url)
	if HTTP.ResultCode == 200 then
		if HTTP.Headers.Values['WWW-Authenticate'] == '' then
		    MODULE.Account.Cookies = HTTP.Cookies
			MODULE.Account.Status = asValid
		else
			MODULE.Account.Cookies = ''
			MODULE.Account.Status = asInvalid
		end
	else
		MODULE.Account.Status = asUnknown
	end
	return true
end

function AccountState()
    local cookies
	if MODULE.Account.Enabled then
	    if MODULE.Account.Cookies ~= '' then
		    MODULE.AddServerCookies(MODULE.Account.Cookies)
		end
	else
	    MODULE.ClearCookies()
		MODULE.Account.Cookies = ''
	end
end

-- Get info and chapter list for the current manga.
function GetInfo()
    Delay()
	CheckAuth()
	HTTP.GET(MODULE.RootURL .. URL)
	if HTTP.ResultCode ~= 200 then
		return net_problem
	end
	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//span[@class="title"]|(//h1//span[@itemprop="title"])[last()]') 
	MANGAINFO.CoverLink = x.XPathString('//img[@itemprop="image"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//a[@itemprop="author"]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="scanstatus"]'), 'No', 'Yes')
	MANGAINFO.Summary   = x.XPathString('//meta[@property="description"]/@content')
	for ch in x.XPath('//table[@id="index-table"]/tbody/tr').Get() do
	    MANGAINFO.ChapterLinks.Add(x.XPathString('td/a[contains(text(),"Read")]/@href', ch))
	    MANGAINFO.ChapterNames.Add(x.XPathString('td[1]/a', ch):gsub("%.%w+%s*$",""))
	end
	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
    local crypto = require 'fmd.crypto'
    Delay()
	CheckAuth()
	HTTP.Headers.Values['charset'] = 'utf-8'
	HTTP.GET(MODULE.RootURL .. URL)
	if HTTP.ResultCode ~= 200 then
		return net_problem
	end
	local x = CreateTXQuery(HTTP.Document)
	local datapath = x.XPathString('//div[@id="reader"]/@data-path')
	datapath = crypto.EncodeURLElement(datapath)
	local datafiles = x.XPathString('//div[@id="reader"]/@data-files')
	datafiles = json.decode(datafiles)
	for i=1, #datafiles do
	    TASK.PageLinks.Add(MODULE.RootURL .. '/reader/image?path=' .. datapath .. '&file=' .. crypto.EncodeURLElement(datafiles[i]))
	end
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
    MODULE.Storage['madokamiulist'] = ''
	local madokamiutable = {}
    Delay()
	CheckAuth()
	HTTP.GET(MODULE.RootURL .. '/Manga')
	if HTTP.ResultCode ~= 200 then
		return net_problem
	end
	local x_1 = CreateTXQuery(HTTP.Document)
	local x_2
	local x_3
	for path_1 in x_1.XPath('//table[@id="index-table"]/tbody/tr/td/a[@href and not(@class="report-link" or @class="tag")]').Get() do
	    if madokamilist_chr[path_1.ToString()] then
			HTTP.GET(MODULE.RootURL .. '/Manga' .. '/'.. path_1.ToString())
			x_2 = CreateTXQuery(HTTP.Document)
			for path_2 in x_2.XPath('//table[@id="index-table"]/tbody/tr/td/a[@href and not(@class="report-link" or @class="tag")]').Get() do
				HTTP.GET(MODULE.RootURL .. '/Manga' .. '/'.. path_1.ToString() .. path_2.ToString())
				x_3 = CreateTXQuery(HTTP.Document)
				for path_3 in x_3.XPath('//table[@id="index-table"]/tbody/tr/td/a[@href and not(@class="report-link" or @class="tag")]').Get() do
					table.insert(madokamiutable, MODULE.RootURL .. '/Manga' .. '/'.. path_1.ToString() .. path_2.ToString() .. path_3.ToString())
                end				
			end
		end
	end
	MODULE.Storage['madokamiulist'] = json.encode(madokamiutable)
	PAGENUMBER = #madokamiutable or 1
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
    local madokamiulist
    if MODULE.Storage['madokamiulist'] ~= '' then
	    madokamiulist = json.decode(MODULE.Storage['madokamiulist'])
	end
	CheckAuth()
	local x = CreateTXQuery()
    for _, url_ in ipairs(madokamiulist) do
	    Delay()
		HTTP.GET(url_)
		CreateTXQuery(HTTP.Document).XPathHREFAll('//table[@id="index-table"]/tbody/tr/td[1]/a[@href and not(@class="report-link" or @class="tag" or ends-with(.,".txt") or ends-with(.,".zip") or ends-with(.,".rar") or ends-with(.,".cbz"))]',
		LINKS, NAMES)
	end
end
