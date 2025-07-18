----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'bdf2eb4381a7403ca93d144b9dbc0d0a'
	m.Name                     = 'Com-X'
	m.RootURL                  = 'https://comx.life'
	m.Category                 = 'Russian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnLogin                  = 'Login'
	m.AccountSupport           = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/comix-read/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Sign in to the current website.
function Login()
	local crypto = require 'fmd.crypto'
	local login_url = MODULE.RootURL .. '/mm'

	if MODULE.Account.Enabled == false then return false end
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
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//div[contains(@class, "pagination__pages")])[1]/a[last()]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//h3[@class="readed__title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local id, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//header[@class="page__header"]/div/h1')
	MANGAINFO.AltTitles = x.XPathString('//header[@class="page__header"]/h2')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="page__poster img-wide"]/img/@src'))
	MANGAINFO.Authors   = x.XPathString('//ul[@class="page__list"]/li[./div="Автор:"]/text()')
	MANGAINFO.Artists   = x.XPathString('//ul[@class="page__list"]/li[./div="Художник:"]/text()')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="page__tags d-flex"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="page__list"]/li[./div="Статус:"]/text()'), 'Продолжается|Завершен, перевод продолжается', 'Завершён', 'Заморожен', 'Приостановлен')
	MANGAINFO.Summary   = x.XPathString('//div[@class="page__text full-text clearfix"]')

	x.ParseHTML(x.XPathString('//script[contains(., "__DATA__")]/substring-before(substring-after(., "__DATA__ = "), ";")'))
	id = x.XPathString('json(*).news_id')
	for v in x.XPath('json(*).chapters()').Get() do
		MANGAINFO.ChapterLinks.Add('reader/' .. id .. '/' .. v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(v.GetProperty('title').ToString())
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local host, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)
	HTTP.Cookies.Values['adult'] = URL:match('/reader/(%d+)/')

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('//script[contains(., "__DATA__")]/substring-before(substring-after(., "__DATA__ = "), ";")'))
	host = x.XPathString('json(*).host')
	for v in x.XPath('json(*).images()').Get() do
		TASK.PageLinks.Add(host .. '/comix/' .. v.ToString())
	end

	return true
end