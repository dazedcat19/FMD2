----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '007d85f7d5cc450da9ff74bdb1a5a104'
	m.Name                     = 'Kurumizaka'
	m.RootURL                  = 'https://kurumizaka.online'
	m.Category                 = 'English-Scanlation'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL):gsub('imaizumi', '') .. 'description.json'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*)()[label="Title :"].value')
	MANGAINFO.CoverLink = MODULE.RootURL .. '/static/media/LOGO.968aaf43a0e40f105640.png'
	MANGAINFO.Authors   = x.XPathString('json(*)()[label="Author :"].value')
	MANGAINFO.Artists   = x.XPathString('json(*)()[label="Artist :"].value')
	MANGAINFO.Genres    = x.XPathString('json(*)()[label="Genre/s :"].value')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*)()[label="Status :"].value'))

	if HTTP.GET(MODULE.RootURL .. '/chapter.json') then
		for v in CreateTXQuery(HTTP.Document).XPath('json(*)/*').Get() do
			MANGAINFO.ChapterLinks.Add('Chapter/Images/' .. v.GetProperty('id').ToString())
			MANGAINFO.ChapterNames.Add(v.GetProperty('name').ToString())
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	TASK.FileNames.Clear()
	local fileutil = require 'fmd.fileutil'
	local v = nil
	local u = MaybeFillHost(MODULE.RootURL, URL) .. '/image.json'

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*)/*').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.GetProperty('path').ToString()))
		TASK.FileNames.Add(fileutil.ExtractFileNameOnly(v.GetProperty('alt_name').ToString()))
	end

	return no_error
end