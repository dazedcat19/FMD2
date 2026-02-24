----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/list?sortType=DATE_CREATE'
DirectoryParameters = '&offset='
DirectoryOffset     = 50
LOGIN_URL           = 'https://3.grouple.co'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Sign in to the current website.
function _M.Login()
	local crypto = require 'fmd.crypto'
	local u = LOGIN_URL .. '/login/authenticate?ttt=' .. os.time() .. '&siteId=' .. SITE_ID

	if MODULE.Account.Enabled == false then return false end

	local s = 'targetUri=%2Flogin%2FcontinueSso%3FsiteId%3D' .. SITE_ID .. '%26targetUri%3D%252F' ..
	'&username='  .. crypto.EncodeURLElement(MODULE.Account.Username) ..
	'&password=' .. crypto.EncodeURLElement(MODULE.Account.Password) ..
	'&remember_me=true&_remember_me_yes=&remember_me_yes=on'
	MODULE.Account.Status = asChecking

	if HTTP.POST(u, s) then
		if (HTTP.ResultCode == 200) and (HTTP.Cookies.Values['remember_me'] ~= '') then
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
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//span[@class="pagination"])[last()]/a[@class="step"][last()]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. DirectoryParameters .. (DirectoryOffset * URL)
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="tiles row"]//div[@class="desc"]/h3/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathStringAll('//div[@class="cr-hero-names"]/h3//span/text()')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="cr-hero-poster__img"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('(//div[div=("Сценарист","Автор")])[1]//a')
	MANGAINFO.Artists   = x.XPathStringAll('(//div[div="Художник"])[1]//a')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="cr-tags"])[1]//a/span[not(@class)]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('(//div[div="Перевод"])[1]//span'), 'Продолжается|Начат', 'Завершён', 'Приостановлен')
	MANGAINFO.Summary   = x.XPathString('(//div[@class="cr-description__content"])[1]')

	for v in x.XPath('//table[@class="table table-hover"]//a[not(contains(@href, "/internal/")) and not(contains(@class, "btn"))]').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('text()', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if MODULE.Account.Status == 2 or HTTP.Cookies.Values['remember_me'] ~= '' then
		if not HTTP.GET(u) then return false end
		local user_hash = HTTP.Document.ToString():match("window%.user_hash = '(.-)';")
		u = u .. '?mtr=true&d=' .. user_hash
	else
		u = u .. '?mtr=true'
	end

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	if not HTTP.GET(u) then return false end

	local json = CreateTXQuery(HTTP.Document).XPathString('//script[contains(., "rm_h.readerInit")]')
	local image = json:match('rm_h%.readerInit%(%s*(%b[])%s*,')

	if not image then
		image = json:match('rm_h%.readerInit%([^,]+,%s*(%b[])%s*,')
	end

	if image then
		for domain, path in image:gmatch("%['([^']+)','[^']*',\"([^\"]+)\"") do
			if domain:find('one%-way%.work', 1, true) then
				path = path:gsub('%?.*$', '')
			end

			local image_url = domain .. path
			TASK.PageLinks.Add(image_url)
		end
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M