local madokamilist = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_'

function GetInfo()
    Delay()
    HTTP.GET(MODULE.RootURL .. URL)
	if HTTP.ResultCode ~= 200 then
	    CheckAuth()
		HTTP.GET(MODULE.RootURL .. URL)
		if HTTP.ResultCode ~= 200 then
		    return net_problem
		end
	end
	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.CoverLink = x.XPathString('//img[@itemprop="image"]/@src')
	if MANGAINFO.Title == '' then MANGAINFO.Title = x.XPathString('//*[@class="title"]') end
	if MANGAINFO.Title == '' then MANGAINFO.Title = x.XPathString('(//h1//span[@itemprop="title"])[last()]') end
	MANGAINFO.Authors   = x.XPathString('//*[@itemprop="author"]')
	MANGAINFO.Genres = x.XPathStringAll('//div[@class="genres"]/a')
	MANGAINFO.Status = x.XPathString('//span[@class="scanstatus"]')
	local chapters = x.XPath('//table[@id="index-table"]/tbody/tr')
	for ic = 1, chapters.Count do
		MANGAINFO.ChapterLinks.Add(x.XPathString('td/a[contains(text(),"Read")]/@href', chapters.Get(ic)))
		MANGAINFO.ChapterNames.Add((x.XPathString('td[1]/a', chapters.Get(ic))):gsub("%.%w+%s*$",""))
	end
	return no_error
end

function GetPageNumber()
    local crypto = require 'fmd.crypto'
    HTTP.GET(MODULE.RootURL .. URL)
	if HTTP.ResultCode ~= 200 then
	    CheckAuth()
		HTTP.GET(MODULE.RootURL .. URL)
		if HTTP.ResultCode ~= 200 then
		    return net_problem
		end
	end

	local x = CreateTXQuery(HTTP.Document)

	datapath = x.XPathString('//div[@id="reader"]/@data-path')
	datapath = crypto.EncodeURLElement(datapath)
	datafiles = x.XPathString('//div[@id="reader"]/@data-files')
	datafiles = datafiles:sub(2,-2):gsub('\\/','/')
	TASK.PageLinks.Delimiter = ','
	TASK.PageLinks.DelimitedText = datafiles
	if TASK.PageLinks.Count > 0 then
	    for i = 0, TASK.PageLinks.Count - 1 do
		    TASK.PageLinks[i] = MODULE.RootURL .. '/reader/image?path=' .. datapath .. '&file=' .. crypto.EncodeURLElement(TASK.PageLinks[i])
		end
	end

end

function CheckAuth()
    AccountState()
	HTTP.GET(MODULE.RootURL)
    if HTTP.Headers.Values['WWW-Authenticate'] ~= '' then
	    HTTP.GET(MODULE.RootURL)
	    if HTTP.ResultCode ~= 200 then
            Login()
		end
	end
end


function GetDirectoryPageNumber()
    Delay()
	local temp_table_1stlv = {}
	local temp_table_2ndlv = {}
	local temp_table_3rdlv = {}

	--create list for first level
	for dirurl in madokamilist:gmatch(".") do
	    table.insert(temp_table_1stlv, MODULE.RootURL .. '/Manga/' .. dirurl)
	end
	--create list for secend level
	for index, item in pairs(temp_table_1stlv) do
	    if index == 3 then break end
		HTTP.GET(item)
		if HTTP.ResultCode ~= 200 then
		    CheckAuth()
			HTTP.GET(item)
			if HTTP.ResultCode ~= 200 then
			    return net_problem
			end
		end
		local x = CreateTXQuery(HTTP.Document)
		local parselinks = x.XPathStringAll('//table[@id="index-table"]/tbody/tr/td[1]/a/@href')
		for i in string.gmatch(parselinks, '([^, ]+)') do
			table.insert(temp_table_2ndlv,MODULE.RootURL .. i)
		end
	end
	--create list for third level
	for index, item in pairs(temp_table_2ndlv) do
	    if index == 3 then break end
		HTTP.GET(item)
		if HTTP.ResultCode ~= 200 then
		    CheckAuth()
			HTTP.GET(item)
			if HTTP.ResultCode ~= 200 then
			    return net_problem
			end
		end
		local x = CreateTXQuery(HTTP.Document)
		local parselinks = x.XPathStringAll('//table[@id="index-table"]/tbody/tr/td[1]/a/@href')
		for i in string.gmatch(parselinks, '([^, ]+)') do
			table.insert(temp_table_3rdlv,MODULE.RootURL .. i)
		end
	end

	--input temp_table_3rdlv into madokamiulist
	for index, item in pairs(temp_table_3rdlv) do
	    if MODULE.Storage['madokamiulist'] == '' then
		    MODULE.Storage['madokamiulist'] = item
		else
		    MODULE.Storage['madokamiulist'] = MODULE.Storage['madokamiulist'] .. ', ' .. item
		end
	end

	PAGENUMBER = #temp_table_3rdlv or 1
	return no_error
end

function GetNameAndLink()
    local madokamiulist = {}
	local madokamiulist_index = 0

	Delay()

	for item in string.gmatch(MODULE.Storage['madokamiulist'], '([^, ]+)') do
		madokamiulist[madokamiulist_index] = item
		madokamiulist_index = madokamiulist_index + 1
	end
	HTTP.GET(madokamiulist[tonumber(URL)])
	if HTTP.ResultCode ~= 200 then
		CheckAuth()
		HTTP.GET(madokamiulist[tonumber(URL)])
		if HTTP.ResultCode ~= 200 then
		    return net_problem
		end
	end
	local x = CreateTXQuery(HTTP.Document)
	local v = x.XPathStringAll('//table[@id="index-table"]/tbody/tr/td[1]/a[not(ends-with(.,".txt") or ends-with(.,".zip") or ends-with(.,".rar"))]')
	for i in string.gmatch(v:gsub(', ',','), '([^,]+)') do
	    NAMES.Add(i:gsub('/$',''))
	end
	local v = x.XPathStringAll('//table[@id="index-table"]/tbody/tr/td[1]/a[not(ends-with(.,".txt") or ends-with(.,".zip") or ends-with(.,".rar"))]/@href')
	for i in string.gmatch(v, '([^, ]+)') do
		LINKS.Add(i)
	end
	return no_error
end

function Delay()
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

function Login()
	MODULE.ClearCookies()
	MODULE.Account.Status = asChecking
	local login_url=MODULE.RootURL
	local crypto = require 'fmd.crypto'
	if not HTTP.GET(login_url) then
		MODULE.Account.Status = asUnknown
		return false
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
	local cookies = ''
	if MODULE.Account.Enabled then
		cookies = MODULE.Account.Cookies
		if cookies ~= '' then
			MODULE.AddServerCookies(MODULE.Account.Cookies)
		end
	else
		--cookies = MODULE.GetServerCookies('madokami.al', 'laravel_session')
		if cookies ~= '' then
			MODULE.Account.Cookies = cookies
			--MODULE.RemoveCookies('madokami.al', 'laravel_session')
		end
	end
	return true
end

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
	--m.AddServerCookies('madokami.al', 'madokami_h_toggle=1; max-age=31556952')

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
