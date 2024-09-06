function Init()
	local m = NewWebsiteModule()
	m.ID                       = '053da85062184626a0a1ff6d57c3c955'
	m.Name                     = 'Hentai2Read'
	m.RootURL                  = 'https://hentai2read.com'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetImageURL            = 'GetImageURL'
	m.OnLogin                  = 'Login'
	m.AccountSupport           = true
	m.SortedList               = true
end

local dirurl = '/hentai-list/all/any/all/last-added';
local cdnurl = 'http://static.hentaicdn.com/hentai';

function Login()
	local login_url = MODULE.RootURL .. '/login'
	if MODULE.Account.Enabled == false then return false end
	local crypto = require 'fmd.crypto'
	local s = 'log=' .. crypto.EncodeURLElement(MODULE.Account.Username) ..
			'&pwd=' .. crypto.EncodeURLElement(MODULE.Account.Password) ..
			'&submit=&redirect_to=https%3A%2F%2Fhentai2read.com%2Fwp-admin%2F&testcookie=1'
	MODULE.Account.Status = asChecking
	if HTTP.POST(login_url, s) then
		if (HTTP.ResultCode == 200) and (CreateTXQuery(HTTP.Document).XPathString('//span[@class="font-w600 push-10-l"]') ~= '') then
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

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. dirurl) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[starts-with(@class,"pagination")]/li[last()-1]/a')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	local s = MODULE.RootURL .. dirurl
	if URL ~= '0' then
		s = s .. '/' .. (URL + 1) .. '/'
	end
	if HTTP.GET(s) then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//div[contains(@class, "book-grid-item")]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)

		MANGAINFO.CoverLink = MaybeFillHost(cdnurl, x.XPathString('//img[@class="img-responsive border-black-op"]/@src'))
		MANGAINFO.Title     = x.XPathString('//h3[@class="block-title"]/a/text()')
		MANGAINFO.Authors   = x.XPathStringAll('//ul[contains(@class,"list-simple-mini")]/li[b="Author"]/a')
		MANGAINFO.Artists   = x.XPathStringAll('//ul[contains(@class,"list-simple-mini")]/li[b="Artist"]/a')
		MANGAINFO.Genres    = x.XPathStringAll('//ul[contains(@class,"list-simple-mini")]/li[b=("Parody","Category","Content","Character")]/a[not(contains(@href, "void"))]')
		MANGAINFO.Summary   = x.XPathString('//ul[contains(@class,"list-simple-mini")]/li[b="Storyline"]/*[position()>1]')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[contains(@class,"list-simple-mini")]/li[b="Status"]'))

		local v for v in x.XPath('//ul[contains(@class,"nav-chapters")]/li/div/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('string-join(text()," ")', v))
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		for v in x.XPath('json(//script[contains(.,"images\' :")]/substring-before(substring-after(.,"images\' : "),"]")||"]")()').Get() do
			TASK.PageLinks.Add(cdnurl .. v.ToString())
		end
		if TASK.PageLinks.Count == 0 then
			TASK.PageNumber = x.XPath('(//li[contains(@class, "pageDropdown")])[1]/ul/li').Count
		end
		return true
	else
		return false
	end
end

function GetImageURL()
	local s = MaybeFillHost(MODULE.RootURL, URL)
	if WORKID > 0 then s = s:gsub('/+$', '') .. '/' .. (WORKID + 1) .. '/' end
	if HTTP.GET(s) then
		TASK.PageLinks[WORKID] = MaybeFillHost(cdnurl, CreateTXQuery(HTTP.Document).XPathString('//img[@id="arf-reader"]/@src'):gsub('^/*', ''))
		return true
	end
		return false
end