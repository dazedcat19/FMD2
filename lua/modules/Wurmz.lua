----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/semua-komik?sort=new&page='
local NextJs = require 'utils.nextjs'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//span[contains(@class, "tabular-nums")]/text()'):match('%d+')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('//article/a[1]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(v.GetAttribute('aria-label'))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local html = HTTP.Document.ToString()
	local ld = html:match('<script type="application/ld%+json">({"@context":"https://schema%.org","@type":"ComicSeries".-})</script>')
	local ok, info = pcall(require 'utils.json'.decode, ld)
	if not ok then return no_error end

	MANGAINFO.Title     = info.name
	MANGAINFO.AltTitles = info.alternateName:gsub(' | ', ', ')
	MANGAINFO.CoverLink = info.image
	MANGAINFO.Authors   = info.author.name
	MANGAINFO.Genres    = table.concat(info.genre or {}, ', ')
	MANGAINFO.Status    = MangaInfoStatusIfPos(html:match('status%-badge">([^<]+)</span>'), 'Ongoing', 'Tamat', 'Hiatus', 'Drop')
	MANGAINFO.Summary   = info.description

	local roots = NextJs.GetRootObjects(html)
	local data
	for _, root in ipairs(roots) do
		data = NextJs.FindObject(root, function(v)
			return type(v) == 'table' and v.chapters and v.sourceSlug
		end)
		if data then
			break
		end
	end

	local chapters = data.chapters
	local slug = data.sourceSlug
	for i = #chapters, 1, -1 do
		local ch = chapters[i]
		MANGAINFO.ChapterLinks.Add('detail/' .. slug .. '/chapter/' .. ch.chapter_label)
		MANGAINFO.ChapterNames.Add('Chapter ' .. ch.chapter_label)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local roots = NextJs.GetRootObjects(HTTP.Document.ToString())
	for _, root in ipairs(roots) do
		local images = NextJs.FindKey(root, 'images')
		if images then
			for i = 1, #images do
				TASK.PageLinks.Add(images[i])
			end
		end
	end

	return false
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '8491f21e5c97463781e705d6d0fbe3e1'
	m.Name                     = 'Wurmz'
	m.RootURL                  = 'https://wurmz.net'
	m.Category                 = 'Indonesian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end