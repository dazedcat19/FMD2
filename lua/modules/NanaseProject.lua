----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '4b211a869f1048f7ab0dc6a9dad608e3'
	m.Name                     = 'Nanase Project'
	m.RootURL                  = 'https://lmtos.net'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/series'
local NextJs = require 'utils.nextjs'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	local roots = NextJs.GetRootObjects(HTTP.Document.ToString())
	local data
	for _, root in ipairs(roots) do
		data = NextJs.FindObject(root, function(v)
			return type(v) == 'table'
				and v.mangas
		end)

		if data then
			break
		end
	end

	for _, manga in ipairs(data.mangas) do
		LINKS.Add('manga/' .. manga.slug)
		NAMES.Add(manga.title)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local roots = NextJs.GetRootObjects(HTTP.Document.ToString())
	local data
	for _, root in ipairs(roots) do
		data = NextJs.FindObject(root, function(v)
			return type(v) == 'table'
				and v.manga
				and v.chapters
		end)

		if data then
			break
		end
	end

	if not data then return no_error end

	local manga = data.manga
	MANGAINFO.Title     = manga.title
	MANGAINFO.AltTitles = table.concat(manga.alternativeTitles or {}, ', ')
	MANGAINFO.CoverLink = manga.coverImage
	MANGAINFO.Authors   = manga.author
	MANGAINFO.Genres    = table.concat(manga.genres or {}, ', ')
	MANGAINFO.Status    = MangaInfoStatusIfPos(manga.status, 'ongoing', 'completed', 'paused')
	MANGAINFO.Summary   = manga.description

	local type = manga.type
	if type then
		MANGAINFO.Genres = (MANGAINFO.Genres ~= '' and MANGAINFO.Genres .. ', ' or '') .. type:gsub('^%l', string.upper)
	end

	local chapters = data.chapters
	for i = #chapters, 1, -1 do
		local ch = chapters[i]
		MANGAINFO.ChapterLinks.Add('manga/' .. manga.slug .. '/' .. ch.slug)
		MANGAINFO.ChapterNames.Add('Capítulo ' .. ch.number)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local roots = NextJs.GetRootObjects(HTTP.Document.ToString())
	for _, root in ipairs(roots) do
		local chapter = NextJs.FindKey(root, 'chapter')
		if chapter then
			for i = 1, #chapter.pages do
				TASK.PageLinks.Add(chapter.pages[i])
			end
		end
	end

	return false
end