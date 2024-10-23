----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga?page='
XPathTokenAuthors = 'Author'
XPathTokenArtists = 'Artist'
XPathTokenStatus  = 'Status'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Login account to the current website.
function _M.Login()
	local s, x = nil
	local login_url = MODULE.RootURL .. '/login'
	if MODULE.Account.Enabled == false then return false end
	local crypto = require 'fmd.crypto'
	if HTTP.GET(login_url) then
		s = '_token=' .. CreateTXQuery(HTTP.Document).XPathString('//input[@name="_token"]/@value') ..
		'&email=' .. crypto.EncodeURLElement(MODULE.Account.Username) ..
		'&password=' .. crypto.EncodeURLElement(MODULE.Account.Password)
	end
	MODULE.Account.Status = asChecking
	HTTP.Reset()
	if HTTP.POST(login_url, s) then
		if (HTTP.ResultCode == 200) and (CreateTXQuery(HTTP.Document).XPathString('//a[@id="logout-button"]') ~= '') then
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
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[contains(@class, "pagination")]/li[last()-1]/@onclick'):match('page=(%d+)')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@id="card-real"]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('figure/div[@class="absolute bottom-0 p-4"]/h2', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local pages, v, x = nil
	local p = 1
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[contains(@class, "flex-col")]/h2/text()')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "rounded-lg object-cover")]/@src')
	MANGAINFO.Authors   = x.XPathString('//p[contains(span, "' .. XPathTokenAuthors .. '")]/span[2]')
	MANGAINFO.Artists   = x.XPathString('//p[contains(span, "' .. XPathTokenArtists .. '")]/span[2]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[contains(@class, "flex-wrap")]/a[contains(@href, "genre")]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//p[contains(span, "' .. XPathTokenStatus .. '")]/a'), 'Hiatus|Ongoing|En cours', 'Cancelled|Completed|Dropped|Terminé')
	MANGAINFO.Summary   = x.XPathString('string-join(//p[@id="description"]/following-sibling::*[self::p or self::div], "\r\n")')

	pages = tonumber(x.XPathString('//ul[contains(@class, "pagination")]/li[last()-1]/@onclick'):match('page=(%d+)')) or 1
	while true do
		for v in x.XPath('//div[@id="chapters-list"]/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('div/div[1]/span', v))
		end
		p = p + 1
		if p > pages then
			break
		elseif HTTP.GET(MANGAINFO.URL .. '?page=' .. tostring(p)) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="chapter-container"]/img/@data-src', TASK.PageLinks)

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M