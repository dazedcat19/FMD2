----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local madokamilist_chr = {}
local madokamilist_custom = {'_', '_Doujinshi', 'Oneshots'}

-- Add A to Z
for i = string.byte('A'), string.byte('Z') do
	madokamilist_chr[string.char(i) .. '/'] = true
end

-- Add custom character
for _, value in ipairs(madokamilist_custom) do
	madokamilist_chr[value .. '/'] = true
end

local folder = '//table[@id="index-table"]/tbody/tr/td/a[not(@class=("report-link", "tag") or @target or matches(., "\\.(txt|zip|rar|cbz|cbr|pdf|7z)$"))]'

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
	HTTP.Headers.Values['Authorization'] = 'Basic ' .. (crypto.EncodeBase64(MODULE.Account.Username .. ':' .. MODULE.Account.Password))

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

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	CheckAuth()
	local madokamiutable = {}

	if not HTTP.GET(MODULE.RootURL .. '/Manga') then return net_problem end

	for path_1 in CreateTXQuery(HTTP.Document).XPath().Get() do
	    if madokamilist_chr[path_1.ToString()] then
			UPDATELIST.UpdateStatusText('Loading ' .. path_1.ToString())
			table.insert(madokamiutable, '/Manga/' .. path_1.ToString())
			if not HTTP.GET(MODULE.RootURL .. '/Manga/' .. path_1.ToString()) then return net_problem end
			for path_2 in CreateTXQuery(HTTP.Document).XPath(folder).Get() do
				UPDATELIST.UpdateStatusText('Loading ' .. path_1.ToString() .. path_2.ToString())
				table.insert(madokamiutable, '/Manga/' .. path_1.ToString() .. path_2.ToString())
				if not HTTP.GET(MODULE.RootURL .. '/Manga/' .. path_1.ToString() .. path_2.ToString()) then return net_problem end
				for path_3 in CreateTXQuery(HTTP.Document).XPath(folder).Get() do
					UPDATELIST.UpdateStatusText('Loading ' .. path_1.ToString() .. path_2.ToString() .. path_3.ToString())
					table.insert(madokamiutable, '/Manga/' .. path_1.ToString() .. path_2.ToString() .. path_3.ToString())
					if not HTTP.GET(MODULE.RootURL .. '/Manga/' .. path_1.ToString() .. path_2.ToString() .. path_3.ToString()) then return net_problem end
					for path_4 in CreateTXQuery(HTTP.Document).XPath(folder).Get() do
						UPDATELIST.UpdateStatusText('Loading ' .. path_1.ToString() .. path_2.ToString() .. path_3.ToString() .. path_4.ToString())
						table.insert(madokamiutable, '/Manga/' .. path_1.ToString() .. path_2.ToString() .. path_3.ToString() .. path_4.ToString())
					end
                end				
			end
		end
	end
	MODULE.Storage['madokamiulist'] = require 'utils.json'.encode(madokamiutable)
	PAGENUMBER = #madokamiutable or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	CheckAuth()
	local madokamiulist = require 'utils.json'.decode(MODULE.Storage['madokamiulist'])
	local u = MODULE.RootURL .. madokamiulist[URL + 1]

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll(folder, LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	CheckAuth()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	
	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//span[@class="title"]')
	if MANGAINFO.Title == '' then MANGAINFO.Title = x.XPathString('string-join((//h1//span[@itemprop="title"])[position() >= last() - 1], " ")') end
	MANGAINFO.CoverLink = x.XPathString('//img[@itemprop="image"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//a[@itemprop="author"]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="scanstatus"]'), 'No', 'Yes')
	MANGAINFO.Summary   = x.XPathString('//meta[@property="description"]/@content')

	local desc = {}
	local folders = {}

	for folder in x.XPath(folder).Get() do
		local links = folder.GetAttribute('href')
		local names = folder.ToString():gsub('/$', '')
		table.insert(folders, '[' .. names .. ']: ' .. MODULE.RootURL .. links)
	end

	if #folders > 0 then table.insert(desc, 'Other folder(s) in this directory:\r\n• ' .. table.concat(folders, '\r\n• ')) end
	MANGAINFO.Summary = MANGAINFO.Summary .. '\r\n \r\n' .. table.concat(desc, '\r\n')

	for ch in x.XPath('//table[@id="index-table"]/tbody/tr').Get() do
	    MANGAINFO.ChapterLinks.Add(x.XPathString('td/a[text()="Read"]/@href', ch))
	    MANGAINFO.ChapterNames.Add(x.XPathString('td[1]/a', ch):gsub('%.%w+%s*$', ''))
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	CheckAuth()
	local crypto = require 'fmd.crypto'
	local json = require 'utils.json'
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local datapath = crypto.EncodeURLElement(x.XPathString('//div[@id="reader"]/@data-path'))
	local datafiles = json.decode(x.XPathString('//div[@id="reader"]/@data-files'))
	for i = 1, #datafiles do
	    TASK.PageLinks.Add(MODULE.RootURL .. '/reader/image?path=' .. datapath .. '&file=' .. crypto.EncodeURLElement(datafiles[i]))
	end

	return true
end

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
end