----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '007f0f56c95b487781e747898c316a4c'
	m.Name                     = 'Inari Manga'
	m.RootURL                  = 'https://inarimanga.yomod.xyz'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local x = nil
	local s = 'action=madara_load_more&template=madara-core/content/content-archive&page=' .. URL .. '&vars[paged]=0&vars[post_type]=wp-manga&vars[posts_per_page]=250'
	local u = MODULE.RootURL .. '/wp-admin/admin-ajax.php'
	HTTP.Headers.Values['Referer'] = u
	HTTP.MimeType = 'application/x-www-form-urlencoded'

	if not HTTP.POST(u, s) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	if x.XPath('//div[@class="grid gap-1.5"]/a').Count == 0 then return no_error end
	x.XPathHREFTitleAll('//div[@class="grid gap-1.5"]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "photoURL")]/@style'):match('background%-image:url%((.-)%)')
	MANGAINFO.Genres    = x.XPathString('(//div[contains(@class, "sm:gap-6")]//div[@alt="type"])[2]/span')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('(//div[contains(@class, "sm:gap-6")]//div[@alt="type"])[1]/span'))
	MANGAINFO.Summary   = x.XPathString('//div[contains(@class, "sm:gap-6")]//div[@id="expand_content"]')

	for v in x.XPath('//li[@class="has-thumb"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div/div/span/span', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return true
end