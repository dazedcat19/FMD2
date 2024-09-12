----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga-list/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Login account to the current website.
function _M.Login()
	local s, x = nil
	local login_url = MODULE.RootURL .. '/login/'
	if MODULE.Account.Enabled == false then return false end
	local crypto = require 'fmd.crypto'
	if HTTP.GET(login_url) then
		s = 'koi_user_login=' .. crypto.EncodeURLElement(MODULE.Account.Username) ..
		'&koi_user_pass=' .. crypto.EncodeURLElement(MODULE.Account.Password) ..
		'&koi_login_nonce=' .. CreateTXQuery(HTTP.Document).XPathString('//input[@name="koi_login_nonce"]/@value')
	end
	MODULE.Account.Status = asChecking
	HTTP.Reset()
	if HTTP.POST(login_url, s) then
		if (HTTP.ResultCode == 200) and (CreateTXQuery(HTTP.Document).XPathString('//div[@id="dropdown-user"]//li/a[contains(@href, "logout")]') ~= '') then
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

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="mangalist-blc"]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="series-title"]/h2')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="series-thumb"]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//li[contains(b, "Author")]/span')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="series-genres"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "status")]'))
	MANGAINFO.Summary   = x.XPathString('string-join(//div[@class="series-synops"], "\r\n")')

	for v in x.XPath('//div[@class="flexch-infoz"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[1]/text()', v))
	end		
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="reader-area"]//img/@src', TASK.PageLinks)

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M