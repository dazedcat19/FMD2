----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '6fffdd27e49c47f9958ea98f2fdca0b1'
	m.Name                     = 'UnionMangas'
	m.RootURL                  = 'https://unionmangasbr.org'
	m.Category                 = 'Portuguese'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://api.novelfull.us'
DirectoryPagination = '/api/manga-br/HomeLastUpdate/24/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = API_URL .. DirectoryPagination .. 0

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).totalPage')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1) - 1

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('manga-br/' .. v.GetProperty('idDoc').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local json, pages, v, x = nil
	local page = 0
	local u = API_URL .. '/api/manga-br/getInfoManga/' .. URL:match('br/(.-)$')

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	json = x.XPath('json(*).data.infoDoc')
	MANGAINFO.Title     = x.XPathString('name', json)
	MANGAINFO.CoverLink = API_URL .. x.XPathString('image', json)
	MANGAINFO.Authors   = x.XPathStringAll('authName', json)
	MANGAINFO.Artists   = x.XPathStringAll('artName', json)
	MANGAINFO.Genres    = x.XPathString('genresName', json)
	MANGAINFO.Summary   = x.XPathString('desc', json) ~= 'null' or ''

	while true do
		if HTTP.GET(API_URL .. '/api/manga-br/GetChapterListFilter/' .. x.XPathString('idDoc', json) .. '/16/' .. tostring(page) .. '/all/DES') then
			x = CreateTXQuery(HTTP.Document)
			pages = tonumber(x.XPathString('json(*).totalPage')) or 1
			for v in x.XPath('json(*).data()').Get() do
				MANGAINFO.ChapterLinks.Add(v.GetProperty('idDoc').ToString() .. '/' .. v.GetProperty('idDetail').ToString())
				MANGAINFO.ChapterNames.Add(v.GetProperty('nameChapter').ToString())
			end
		else
			break
		end
		page = page + 1
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local json, i, x = nil
	local u = API_URL .. '/api/manga-br/GetImageChapter' .. URL

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	json = x.XPathString('json(*).data.detail_documents.source')
	for i in json:gmatch('(.-)#') do
		TASK.PageLinks.Add(i)
	end

	return no_error
end