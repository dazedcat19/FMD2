----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'b8a5b7a71025420fa3718fd498977e3e'
	m.Name                     = '禁漫天堂 (18comic)'
	m.RootURL                  = 'https://18comic.vip'
	m.Category                 = 'Raw'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/albums?o=mr&page='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Get a row count based on the MD5 hash of aid and index.
local function GetRows(aid, index)
	local modulus = (aid >= 421926 and 8) or (aid >= 268850 and 10) or 10
	local md5 = require 'fmd.crypto'.MD5(aid .. index)
	local last_char = string.format('%x', md5:byte(-1)):byte(-1)
	return 2 * (last_char % modulus) + 2
end

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
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@class="thumb-overlay-albums"]/a').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('img/@alt', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@id="book-name"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="thumb-overlay"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('(//span[@data-type="author"])[1]/a')
	MANGAINFO.Genres    = x.XPathStringAll('(//span[@data-type="tags"])[1]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('(//span[@data-type="tags"])[1]/a[contains(., "連載中") or contains(., "完結")]'), '連載中', '完結')
	MANGAINFO.Summary   = x.XPathString('//h2[contains(., "叙述")]/substring-after(., "叙述：")')

	for v in x.XPath('//div[@class="episode"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('.//h3', v))
	end
	if MANGAINFO.ChapterLinks.Count == 0 then
		MANGAINFO.ChapterLinks.Add(x.XPathString('(//div[@class="p-t-5 p-b-5 read-block"])[1]/a/@href'))
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="center scramble-page spnotice_chk"]/img/@data-original', TASK.PageLinks)

	return true
end

-- Download and decrypt and/or descramble image given the image URL.
function DownloadImage()
	if not HTTP.GET(URL) then return false end

	local aid, index = URL:match('/media/photos/(%d+)/([^/%.]+)')
	aid = tonumber(aid) or 0
	local rows = GetRows(aid, index)
	local puzzle = require('fmd.imagepuzzle').Create(1, rows)

	for i = 0, rows - 1 do
		puzzle.Matrix[i] = rows - 1 - i
	end

	puzzle.DeScramble(HTTP.Document, HTTP.Document)

	return true
end