----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '7103ae6839ea46ec80cdfc2c4b37c803'
	m.Name                     = 'Asura Scans'
	m.RootURL                  = 'https://asurascans.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.asurascans.com/api/series'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/browse?sort=newest'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	while true do
		for v in x.XPath('//div[@class="p-3"]/a').Get() do
			LINKS.Add(v.GetAttribute('href'):gsub('-(%w+)$', ''))
			NAMES.Add(x.XPathString('h3', v))
		end
		local next_url = x.XPathString('//a[@aria-label="Next page"]/@href')
		if next_url == '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('(%d+)') or ''))
		if not HTTP.GET(MaybeFillHost(MODULE.RootURL, next_url)) then break end
		x.ParseHTML(HTTP.Document)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//p[@id="alt-titles"]')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "w-full h-full absolute")]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//a[contains(@href, "author")]')
	MANGAINFO.Artists   = x.XPathStringAll('//a[contains(@href, "artist")]')
	MANGAINFO.Genres    = x.XPathStringAll('(//a[contains(@href, "genres")], concat(upper-case(substring(//span[contains(@class, "bold text-[#913FE2]")], 1, 1)), lower-case(substring(//span[contains(@class, "bold text-[#913FE2]")], 2))))')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "text-[#A78BFA]")]'), 'Ongoing', 'Completed', 'Hiatus', 'Dropped')
	MANGAINFO.Summary   = x.XPathString('string-join(//div[@id="description-text"]//p, "\r\n")')

	local data = require 'utils.json'.decode(x.XPathString('//div[@class="mt-4"]//@props'))
	local slug = data.seriesSlug[2]
	for _, v in ipairs(data.chapters[2]) do
		local c = v[2]
		local number = c.number[2]
		local title = c.title and c.title[2]
		local is_locked = c.is_locked[2]

		title = title and (' - ' .. title) or ''

		if not is_locked then
			MANGAINFO.ChapterLinks.Add(slug .. '/chapters/' .. number)
			MANGAINFO.ChapterNames.Add('Chapter ' .. number .. title)
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. URL

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data.chapter.pages().url', TASK.PageLinks)

	return true
end