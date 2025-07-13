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
KeepParameters      = false
LoginUrl            = 'https://3.grouple.co'
SITE_ID             = ''

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Sign in to the current website.
function _M.Login()
	local s, x = nil
	local crypto = require 'fmd.crypto'
	local login_url = LoginUrl .. '/login/authenticate?ttt=' .. os.time() .. '&siteId=' .. SITE_ID

	if MODULE.Account.Enabled == false then return false end

	s = 'targetUri=%2Flogin%2FcontinueSso%3FsiteId%3D' .. SITE_ID .. '%26targetUri%3D%252F' ..
	'&username='  .. crypto.EncodeURLElement(MODULE.Account.Username) ..
	'&password=' .. crypto.EncodeURLElement(MODULE.Account.Password) ..
	'&remember_me=true&_remember_me_yes=&remember_me_yes=on'
	MODULE.Account.Status = asChecking

	if HTTP.POST(login_url, s) then
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

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//span[@class="pagination"])[last()]/a[@class="step"][last()]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. DirectoryParameters .. (DirectoryOffset * URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="tiles row"]//div[@class="desc"]/h3/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="names"]/span[@class="name"]')
	MANGAINFO.AltTitles = x.XPathStringAll('//h1[@class="names"]//span[@class="eng-name"]|//h1[@class="names"]//span[@class="original-name"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="picture-fotorama"]/img[1]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//span[contains(@class, "elem_author")]/a|//span[contains(@class, "elem_screenwriter")]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//span[contains(@class, "elem_illustrator")]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//span[contains(., "Жанры:")]/following-sibling::a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="subject-meta"]'), 'выпуск продолжается', 'выпуск завершён')
	MANGAINFO.Summary   = x.XPathString('(//div[@class="manga-description"])[1]')

	x.XPathHREFAll('//table[@class="table table-hover"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local domain, json, user_hash, path = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if MODULE.Account.Status == 2 or HTTP.Cookies.Values['remember_me'] ~= '' then
		if not HTTP.GET(u) then return false end
		user_hash = HTTP.Document.ToString():match("window%.user_hash = '(.-)';")
		u = u .. '?mtr=true&d=' .. user_hash
	else
		u = u .. '?mtr=true'
	end

	if not HTTP.GET(u) then return false end

	json = CreateTXQuery(HTTP.Document).XPathString('//script[contains(., "rm_h.readerInit")]'):match('rm_h%.readerInit%(%[(%[.-%])%]')
	for domain, path in json:gmatch("%['([^']+)','[^']*',\"([^\"]+)\"") do
		TASK.PageLinks.Add(domain .. (KeepParameters and path or path:gsub('%?.*$', '')))
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