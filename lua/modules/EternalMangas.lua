----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
local m = NewWebsiteModule()
	m.ID                       = '582a678d967e4d2eaf69d14270e064bd'
	m.Name                     = 'EternalMangas'
	m.RootURL                  = 'https://eternalmangas.com'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = 'https://apis.eternalmangas.com/api/comics'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v = nil
	local u = DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*)()').Get() do
		if v.GetProperty('type').ToString() == 'comic' then
			LINKS.Add('ver/' .. v.GetProperty('slug').ToString())
			NAMES.Add(v.GetProperty('name').ToString())
		end
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local name1, name2, name3, name, s, v, value1, value2, value3, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	name1, value1 = x.XPathString('//input[1]/@name'), x.XPathString('//input[1]/@value')
	name2, value2 = x.XPathString('//input[2]/@name'), x.XPathString('//input[2]/@value')
	name3, value3 = x.XPathString('//input[3]/@name'), x.XPathString('//input[3]/@value')
	name4, value4 = x.XPathString('//input[4]/@name'), x.XPathString('//input[4]/@value')
	s = name1 .. '=' .. value1 .. '&' .. name2 .. '=' .. value2 .. '&' .. name3 .. '=' .. value3 .. '&' .. name4 .. '=' .. value4
	u = x.XPathString('//form/@action')

	HTTP.Reset()
	if not HTTP.POST(u, s) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//div[./p="Nombre Alternativo:"]/p[2]')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "w-full object-cover relative")]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[./p="Autor:"]/p[2]/span')
	MANGAINFO.Artists   = x.XPathStringAll('//div[./p="Artista:"]/p[2]/span')
	MANGAINFO.Genres    = x.XPathStringAll('//div[./p="Género:"]/p[2]/span')
	MANGAINFO.Summary   = x.XPathString('//div[@id="sinopsis"]/p')

	for v in x.XPath('//div[contains(@class, "w-full grid gap-2")]/div/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div/span[1]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local name1, name2, name3, name4, name5, s, value1, value2, value3, value4, value5, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	name1, value1 = x.XPathString('//input[1]/@name'), x.XPathString('//input[1]/@value')
	name2, value2 = x.XPathString('//input[2]/@name'), x.XPathString('//input[2]/@value')
	name3, value3 = x.XPathString('//input[3]/@name'), x.XPathString('//input[3]/@value')
	name4, value4 = x.XPathString('//input[4]/@name'), x.XPathString('//input[4]/@value')
	name5, value5 = x.XPathString('//input[5]/@name'), x.XPathString('//input[5]/@value')
	s = name1 .. '=' .. value1 .. '&' .. name2 .. '=' .. value2 .. '&' .. name3 .. '=' .. value3 .. '&' .. name4 .. '=' .. value4 .. '&' .. name5 .. '=' .. value5
	u = x.XPathString('//form/@action')

	HTTP.Reset()
	if not HTTP.POST(u, s) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img/@data-src', TASK.PageLinks)

	return no_error
end