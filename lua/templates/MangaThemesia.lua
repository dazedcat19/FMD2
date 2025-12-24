----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga/list-mode/'
XPathTokenAuthors   = 'Author'
XPathTokenArtists   = 'Artist'
XPathTokenStatus    = 'Status'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Extract alternative titles
local function GetAltTitles(x)
	local alttitle = x.XPathString('//div[@class="seriestualt"]')
	if alttitle == '' then alttitle = x.XPathString('//span[@class="alternative"]') end
	return alttitle
end

-- Extract the manga cover image URL
local function GetCoverLink(x)
	local img = x.XPathString('//div[@itemprop="image"]//img/@data-lazy-src')
	if img == '' then img = x.XPathString('//div[@itemprop="image"]//img/@data-src') end
	if img == '' then img = x.XPathString('//div[@itemprop="image"]//img/@src') end
	return img
end

-- Extract author information
local function GetAuthors(x)
	local authors = x.XPathString('//td[contains(., "' .. XPathTokenAuthors .. '")]/following-sibling::td')
	if authors == '' then authors = x.XPathString('//div[@class="fmed" and contains(b, "' .. XPathTokenAuthors .. '")]/span') end
	if authors == '' then authors = x.XPathString('(//div[@class="imptdt" and contains(., "' .. XPathTokenAuthors .. '")]/i)[1]') end
	return authors
end

-- Extract artist information
local function GetArtists(x)
	local artists = x.XPathString('//td[contains(., "' .. XPathTokenArtists .. '")]/following-sibling::td')
	if artists == '' then artists = x.XPathString('//div[@class="fmed" and contains(b, "' .. XPathTokenArtists .. '")]/span') end
	if artists == '' then artists = x.XPathString('(//div[@class="imptdt" and contains(., "' .. XPathTokenArtists .. '")]/i)[1]') end
	return artists
end

-- Extract genre information
local function GetGenres(x)
	local genre = x.XPathStringAll('//div[@class="seriestugenre"]/a')
	if genre == '' then genre = x.XPathStringAll('//span[@class="mgen"]/a') end
	return genre
end

-- Extract status information
local function GetStatus(x)
	local status = x.XPathString('//td[contains(., "' .. XPathTokenStatus .. '")]/following-sibling::td')
	if status == '' then status = x.XPathString('//div[@class="imptdt" and contains(., "' .. XPathTokenStatus .. '")]/i') end
	return status
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="blix"]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="entry-title"]')
	MANGAINFO.AltTitles = GetAltTitles(x)
	MANGAINFO.CoverLink = GetCoverLink(x)
	MANGAINFO.Authors   = GetAuthors(x)
	MANGAINFO.Artists   = GetArtists(x)
	MANGAINFO.Genres    = GetGenres(x)
	MANGAINFO.Status    = MangaInfoStatusIfPos(GetStatus(x), 'Berjalan|Ongoing|Publishing|En Cours', 'Tamat|Completed|Finished|Terminé', 'Hiatus|En Pause', 'Dropped|Abandonné')
	MANGAINFO.Summary   = x.XPathString('//div[@itemprop="description"]/*[not(script)]/string-join(text(), "\r\n")')

	for v in x.XPath('//div[@id="chapterlist"]//div[@class="eph-num"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[@class="chapternum"]/normalize-space(.)', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('run(', ');', x.XPathString('//script[contains(., "ts_reader")]')))
	x.XPathStringAll('json(*).sources()[1].images()', TASK.PageLinks)
	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub('i%d.wp.com/', '')
		if string.find(TASK.PageLinks[i], 'blogger') or string.find(TASK.PageLinks[i], 'blogspot') then
			TASK.PageLinks[i] = TASK.PageLinks[i]:gsub('/s1600/', '/s0/')
		end
		i = i + 1
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M