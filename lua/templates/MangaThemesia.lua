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
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = GetTitle(x)
	MANGAINFO.CoverLink = GetCoverLink(x)
	MANGAINFO.Authors   = GetAuthors(x)
	MANGAINFO.Artists   = GetArtists(x)
	MANGAINFO.Genres    = GetGenres(x)
	MANGAINFO.Status    = MangaInfoStatusIfPos(GetStatus(x), 'Berjalan|Ongoing|Publishing', 'Tamat|Completed|Finished')
	MANGAINFO.Summary   = x.XPathString('//div[@itemprop="description"]/*[not(script)]')

	for v in x.XPath('//div[@id="chapterlist"]//div[@class="eph-num"]/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[@class="chapternum"]/normalize-space(.)', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

function GetTitle(x)
	local title = x.XPathString('//h1[@class="entry-title"]')
	title = title:gsub('Bahasa Indonesia$', ''):gsub('Indonesia Terbaru$', ''):gsub('^Komik', '')
	return title
end

function GetCoverLink(x)
	local img = x.XPathString('//div[@itemprop="image"]//img/@data-lazy-src')
	if img == '' then img = x.XPathString('//div[@itemprop="image"]//img/@data-src') end
	if img == '' then img = x.XPathString('//div[@itemprop="image"]//img/@src') end
	return img
end

function GetAuthors(x)
	local authors = x.XPathString('//td[contains(., "' .. XPathTokenAuthors .. '")]/following-sibling::td')
	if authors == '' then authors = x.XPathString('//div[@class="fmed" and contains(b, "' .. XPathTokenAuthors .. '")]/span') end
	if authors == '' then authors = x.XPathString('(//div[@class="imptdt" and contains(., "' .. XPathTokenAuthors .. '")]/i)[1]') end
	return authors
end

function GetArtists(x)
	local artists = x.XPathString('//td[contains(., "' .. XPathTokenArtists .. '")]/following-sibling::td')
	if artists == '' then artists = x.XPathString('//div[@class="fmed" and contains(b, "' .. XPathTokenArtists .. '")]/span') end
	if artists == '' then artists = x.XPathString('(//div[@class="imptdt" and contains(., "' .. XPathTokenArtists .. '")]/i)[1]') end
	return artists
end

function GetGenres(x)
	local genre = x.XPathStringAll('//div[@class="seriestugenre"]/a')
	if genre == '' then genre = x.XPathStringAll('//span[@class="mgen"]/a') end
	return genre
end

function GetStatus(x)
	local status = x.XPathString('//td[contains(., "Status")]/following-sibling::td')
	if status == '' then status = x.XPathString('//div[@class="imptdt" and contains(., "' .. XPathTokenStatus .. '")]/i') end
	return status
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local i, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(GetBetween('run(', ');', x.XPathString('//script[contains(., "ts_reader")]')))
	x.XPathStringAll('json(*).sources()[1].images()', TASK.PageLinks)
	for i = 0, TASK.PageLinks.Count - 1 do -- Bypass 'i0.wp.com' image CDN to ensure original images are loaded directly from host
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub("i%d.wp.com/", "")
		if string.find(TASK.PageLinks[i], "blogger") or string.find(TASK.PageLinks[i], "blogspot") then
			TASK.PageLinks[i] = TASK.PageLinks[i]:gsub("/s1600/", "/s0/")
		end
		i = i + 1
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
