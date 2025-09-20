----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5aa6794ceeea4192a63de8296344da3c'
	m.Name                     = 'MangaOni'
	m.RootURL                  = 'https://manga-oni.com'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/directorio?adulto=false&orden=desc&p='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@id="article-div"]//a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('div/span', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="post-title"]/a')
	MANGAINFO.AltTitles = x.XPathString('//div[@id="info-i"]/strong[.="Alterno:"]/following-sibling::text()[1]')
	MANGAINFO.CoverLink = x.XPathString('//a[@class="portada"]/@href')
	MANGAINFO.Authors   = x.XPathString('//div[@id="info-i"]/strong[.="Autor:"]/following-sibling::text()[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@id="categ"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="estado"]'), 'En desarrollo', 'Finalizado')
	MANGAINFO.Summary   = x.XPathString('//div[@id="sinopsis"]/text()')

	for v in x.XPath('//div[@id="c_list"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div/h3', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local i, images, path = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	s = require 'fmd.crypto'.DecodeBase64(HTTP.Document.ToString():match("unicap = '(.-)'"))
	path, images = s:match('(.*)||%[(.*)%]')
	for i in images:gmatch('"(.-)"') do
		TASK.PageLinks.Add(path .. i)
	end

	return true
end