----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/list?sortType=DATE_CREATE'
DirectoryParameters = '&offset='
DirectoryOffset     = 50

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//span[@class="pagination"])[last()]/a[@class="step"][last()]')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if URL ~= '0' then u = u .. DirectoryParameters .. (DirectoryOffset * tonumber(URL)) end

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="tiles row"]//div[@class="desc"]/h3/a', LINKS, NAMES)

  return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="names"]/span[@class="name"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="picture-fotorama"]/img[1]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//span[contains(@class, "elem_author")]/a|//span[contains(@class, "elem_screenwriter")]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//span[contains(@class, "elem_illustrator")]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//span[contains(., "Жанры:")]/following-sibling::a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="subject-meta"]'), 'выпуск продолжается', 'выпуск завершён')
	MANGAINFO.Summary   = x.XPathString('(//div[@class="manga-description"])[1]')

	x.XPathHREFAll('//table[@class="table table-hover"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local json, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if string.find(URL, 'mtr=1', 1, true) == nil then u = u .. '?mtr=1' end

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	json = GetBetween('[[', ', 0, ', Trim(GetBetween('rm_h.readerDoInit(', 'false);', x.XPathString('//script[@type="text/javascript" and contains(., "rm_h.readerDoInit")]'))))
	json = json:gsub('%],%[', ';'):gsub('\'', ''):gsub('"', ''):gsub(']]', ';')
	for i in json:gmatch('(.-);') do
		i1, i2 = i:match('(.-),.-,(.-),.-,.-')
		TASK.PageLinks.Add(i1 .. i2)
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
