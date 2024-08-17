----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '2f5e1b629ce148988e56fca46798afd1'
	m.Name                     = 'Tsumino'
	m.RootURL                  = 'https://www.tsumino.com'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local s = 'PageNumber=1&Text=&Sort=Newest&List=0&Length=0&MinimumRating=0&ExcludeList=0&CompletelyExcludeHated=false'
	local u = MODULE.RootURL .. '/Search/Operate/?type=Book'

	if not HTTP.POST(u, s) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).x.XPathString('json(*)("pageCount")')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v = nil
	local s = 'Text=&Sort=Newest&List=0&Length=0&MinimumRating=0&ExcludeList=0&CompletelyExcludeHated=false&PageNumber=' .. (URL + 1)
	local u = MODULE.RootURL .. '/Search/Operate/?type=Book'

	if not HTTP.POST(u, s) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*)("data")().entry').Get() do
		NAMES.Add(v.GetProperty('title').ToString())
		LINKS.Add('entry/' .. v.GetProperty('id').ToString())
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathStringAll('//div[@id="Title"]')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="book-page-image img-responsive"]/@src')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@id="Artist"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@id="Tag" or @id="Parody" or @id="Collection"]/a')

	MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local data_cdn, data_length, i, x = nil
	local u = MODULE.RootURL .. '/Read/Index/' .. URL:match('(%d+)') .. '?page=1'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	data_cdn = x.XPathString('//div[@data-cdn]/@data-cdn')
	data_length = tonumber(x.XPathString('//h1[starts-with(., "Page 1 of ")]'):match('(%d+)$'))
	if data_cdn and data_length then
		for i = 1, data_length do
			TASK.PageLinks.Add(data_cdn:gsub('%[PAGE%]', i))
		end
	end
	
	return no_error
end