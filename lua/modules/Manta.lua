----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '596b042dc96d4f3886a544f91a648438'
	m.Name                     = 'Manta'
	m.RootURL                  = 'https://manta.net'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'

	m.AddOptionEdit('auth', 'Authorization:')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/front/v1/'
local DirectoryPagination = '/home/202505'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. API_URL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data.items()').Get() do
		LINKS.Add('en/series/' .. v.GetProperty('title').ToString():lower():gsub("'", ""):gsub("%s+", "-") .. '?seriesId=' .. v.GetProperty('seriesId').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MODULE.RootURL .. API_URL .. '/series/' .. URL:match('seriesId=(%d+)$')
	HTTP.Reset()
	HTTP.Headers.Values['Authorization'] = MODULE.GetOption('auth')

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).data.data.title.en')
	MANGAINFO.CoverLink = x.XPathString('json(*).data.image.1280x1840_480.downloadUrl')
	MANGAINFO.Authors   = x.XPathStringAll('json(*).data.data.creators()[role="Original Story" or role="Writer" or role="Historia original" or role="Autor"].name')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).data.data.creators()[role="Illustration" or role="Ilustración"].name')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).data.data.tags().name.en')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('json(*).data.data.isCompleted') == 'true' and 'Completed' or 'Ongoing')
	MANGAINFO.Summary   = x.XPathString('json(*).data.data.description.short') .. '\r\n' .. x.XPathString('json(*).data.data.description.long')

	for v in x.XPath('json(*).data.episodes()[lockData/state="100" or lockData/state="110" or lockData/state="130" or lockData/state="140"]').Get() do
		local episodeTitle = x.XPathString('data/title', v)
		if episodeTitle == '' then
			episodeTitle = 'Episode ' .. x.XPathString('ord', v)
		end

		MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
		MANGAINFO.ChapterNames.Add(episodeTitle)
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. API_URL .. '/episodes' .. URL
	HTTP.Reset()
	HTTP.Headers.Values['Authorization'] = MODULE.GetOption('auth')

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.cutImages().downloadUrl', TASK.PageLinks)

	return true
end