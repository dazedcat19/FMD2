----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '472b36c3f1284d018b1f48c8bf2d46ad'
	m.Name                     = 'NineMangaRU'
	m.RootURL                  = 'https://ru.ninemanga.com'
	m.Category                 = 'Russian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.NineManga'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.AltTitles = x.XPathString('//li[contains(b, "альтернатива:")]/substring-after(., "альтернатива:")')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="message"]/li[contains(b, "статус")]/a[1]'), 'постоянный', 'завершенный')

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local cookie_name, last_location, x = nil
	local cid = URL:match('/(%d+).html$')
	local u = 'https://www.mastertheenglish.com'
	local p = '/go/jump/?type=runinemanga&cid=' .. cid
	local w = 'https://www.financewealthloans.com/go/runm/' .. cid

	HTTP.Headers.Values['Referer'] = w

	if not HTTP.GET(u .. p) then return net_problem end

	last_location = HTTP.Document.ToString():match('window%.location%.href="(.*)";')
	cookie_name = 'lrgarden_visit_check_' .. last_location:match('(%d+)')

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = w
	HTTP.Cookies.Values[cookie_name] = cid

	if not HTTP.GET(u .. last_location) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML('[' .. GetBetween('all_imgs_url: [', '],', x.XPathString('//script[contains(., "all_imgs_url")]')) .. ']')
	x.XPathStringAll('json(*)()', TASK.PageLinks)

	return no_error
end