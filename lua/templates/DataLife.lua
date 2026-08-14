----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/comix/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Sign in to the current website.
function _M.Login()
	local crypto = require 'fmd.crypto'
	local login_url = MODULE.RootURL

	if not MODULE.Account.Enabled then return false end

	local s = 'login_name='  .. crypto.EncodeURLElement(MODULE.Account.Username) ..
	'&login_password=' .. crypto.EncodeURLElement(MODULE.Account.Password) ..
	'&login=submit'
	MODULE.Account.Status = asChecking

	if HTTP.POST(login_url, s) then
		if (HTTP.ResultCode == 200) and (HTTP.Cookies.Values['dle_user_id'] ~= '' and HTTP.Cookies.Values['dle_user_id'] ~= 'deleted') then
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

	return no_error
end

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//div[contains(@class, "pagination__pages")])[1]/a[last()]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//h3[@class="readed__title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	if HTTP.ResultCode ~= 200 then MANGAINFO.Title = 'Manual cookies workaround is required' return no_error end

	local s = HTTP.Document.ToString()
	local x = CreateTXQuery(s)
	MANGAINFO.Title     = x.XPathString('//header[@class="page__header"]/div/h1')
	MANGAINFO.AltTitles = x.XPathString('//header[@class="page__header"]/h2')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="page__poster img-wide"]/img/@src'))
	MANGAINFO.Authors   = x.XPathStringAll('//ul[@class="page__list"]/li[div=("Writer:", "Автор:")]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//ul[@class="page__list"]/li[div=("Artist:", "Художник:")]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="page__tags d-flex"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="page__list"]/li[div=("Release type:", "Статус:")]/text()'), 'Ongoing|Продолжается|Завершен, перевод продолжается', 'Completed|Завершён', 'Заморожен', 'Приостановлен')
	MANGAINFO.Summary   = x.XPathString('//div[@class="page__text full-text clearfix"]')

	local data = require 'utils.json'.decode('{' .. s:match('__DATA__%s=%s{(.-)};') .. '}')
	local id = data.news_id
	for _, chapter in ipairs(data.chapters) do
		MANGAINFO.ChapterLinks.Add('reader/' .. id .. '/' .. chapter.id)
		MANGAINFO.ChapterNames.Add(chapter.title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('//script[contains(., "__DATA__")]/substring-before(substring-after(., "__DATA__ = "), ";")'))
	x.XPathStringAll('json(*).images()', TASK.PageLinks)

	return false
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function _M.BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M