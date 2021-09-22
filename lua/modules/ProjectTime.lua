----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '14ac824309034a6495fc4f91873aeb30'
	m.Name                     = 'ProjectTime'
	m.RootURL                  = 'https://ptscans.tw'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnLogin                  = 'Login'
	m.AccountSupport           = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	CheckAuth()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[contains(@class, "uk-card-body")]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	CheckAuth()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//main[@id="site-content"]//div[@class="uk-width-2-3@m"]/div')
	MANGAINFO.CoverLink = x.XPathString('//main[@id="site-content"]//div[contains(@class, "uk-width-1-3@m")]/img/@src')
	MANGAINFO.Summary   = x.XPathString('//main[@id="site-content"]//div[@class="uk-width-2-3@m"]/p[2]')

	x.XPathHREFAll('//a[contains(@class, "uk-link-reset")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	CheckAuth()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="wreader"]//img/@data-src', TASK.PageLinks)

	return no_error
end

function CheckAuth()
	HTTP.GET(MODULE.RootURL)
	local x = CreateTXQuery(HTTP.Document)
	if x.XPathString('//div[@class="page-header"]') == 'Login' then Login() end
end

function Login()
	MODULE.ClearCookies()
	MODULE.Account.Status = asChecking
	local login_url = MODULE.RootURL .. '/login'
	local crypto = require 'fmd.crypto'
	if not HTTP.GET(login_url) then
		MODULE.Account.Status = asUnknown
		return false
	end

	HTTP.Reset()

	HTTP.Headers.Values['Content-Type'] = 'application/x-www-form-urlencoded'
	local payload = 'password=' .. crypto.EncodeURLElement(MODULE.Account.Password) .. '&username=' .. crypto.EncodeURLElement(MODULE.Account.Username)

	HTTP.POST(login_url, payload)
	if HTTP.ResultCode == 200 then
		local x = CreateTXQuery(HTTP.Document)
		if x.XPathString('//div[@class="page-header"]') == 'Login success!' then
			MODULE.Account.Cookies = HTTP.Cookies
			MODULE.Account.Status = asValid
			return true
		else
			MODULE.Account.Cookies = ''
			MODULE.Account.Status = asInvalid
			return false
		end
	else
		MODULE.Account.Status = asUnknown
		return false
	end
	return true
end
