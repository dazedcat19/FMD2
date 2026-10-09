----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPage = '/enchiladaweb/catalogo.json'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPage

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).items()').Get() do
		LINKS.Add('enchiladaweb' .. v.GetProperty('post_url').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title 	= x.XPathString('//h1[@class="manga-title"]')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="manga-cover"]/img/@src'))
	MANGAINFO.Authors   = x.XPathString('//li[strong="Autor:"]/text()')
	MANGAINFO.Artists   = x.XPathString('//li[strong="Arte:"]/text()')
	MANGAINFO.Genres    = x.XPathString('//li[strong="Géneros:"]/text()')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//li[strong="Estado:"]/text()'), 'En publicación|En emisión', 'Final')
	MANGAINFO.Summary   = x.XPathString('//p[@class="manga-sinopsis"]')

	for v in x.XPath('//ul[@id=("chaptersList","extrasList")]/li/a').Get() do
		local number = x.XPathString('.//span[@class="cap-number"]', v)
		local title  = x.XPathString('.//span[@class="cap-title"]', v)

		title = number:match('%d+') and title:find(number:match('%d+'), 1, true) and number or number .. ' - ' .. title

		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(title)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u
	if URL:find('/cap%d+/') then
		u = '/enchiladaweb/assets/mangas/' .. URL:match('/[^/]+/([^/]+/cap%d+)/') .. '/images.json'
	else
		if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return false end
		u = '/enchiladaweb/' .. HTTP.Document.ToString():match("RAW%s*=%s*%('([^']+)'")
	end

	if not HTTP.GET(MODULE.RootURL .. u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*)()', TASK.PageLinks)

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'dfff1035a7e647729abe8c36fe6ff271'
	m.Name                     = 'EnchiladaWEB'
	m.RootURL                  = 'https://enchiladascan.github.io'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end