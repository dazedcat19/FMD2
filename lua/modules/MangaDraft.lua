----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd7f782e7fc464d32bb6d0fec507cec7f'
	m.Name                     = 'MangaDraft'
	m.RootURL                  = 'https://www.mangadraft.com'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.MaxTaskLimit             = 1
	m.MaxConnectionLimit       = 1
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/catalog/projects?number=10000&order=name'
local delay = 2000

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local function Delay()
	local now     = os.time()
	local last    = tonumber(MODULE.Storage['lastDelay']) or 0
	local elapsed = (now - last) * 1000

	if elapsed < delay then
		sleep(delay - elapsed)
	end

	MODULE.Storage['lastDelay'] = now
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString())).XPath('json(*).data()').Get() do
		LINKS.Add(v.GetProperty('url').ToString())
		NAMES.Add(v.GetProperty('name').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Delay()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(//script ! substring-before(substring-after(., "window.project = "), "};") || "}")')
	MANGAINFO.Title     = x.XPathString('name', info)
	MANGAINFO.CoverLink = x.XPathString('avatar', info)
	MANGAINFO.Authors   = x.XPathString('//a[@title="créateur"]')
	MANGAINFO.Artists   = x.XPathString('//a[@title="Dessinatrice"]')
	MANGAINFO.Genres    = x.XPathString('replace(string-join((genres?*?name, concat(upper-case(substring(project_type, 1, 1)), lower-case(substring(project_type, 2)))), ", "), " [|]", ",")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('project_status_id', info), '0', '1', '2')
	MANGAINFO.Summary   = x.XPathString('description', info)

	for v in x.XPath('//div[@class="mt-7 w-full"]//a[not(h5/small)]').Get() do
		local url = v.GetAttribute('href')

		MANGAINFO.ChapterLinks.Add(url)

		if not url:find('c.', 1, true) then
			MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
			break
		end

		MANGAINFO.ChapterNames.Add(x.XPathString('h5/text()', v))
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	Delay()
	local cid
	local gid = URL:match('%d+')
	local suffix = ''
	local pages

	if URL:find('c.', 1, true) then
		local u = MaybeFillHost(MODULE.RootURL, URL)

		if not HTTP.GET(u) then return false end

		cid = HTTP.LastURL:match('/(%d+)$')
		suffix = '&grouped_by_category=true'
	else
		cid = gid
	end

	if not HTTP.GET(MODULE.RootURL .. '/api/reader/listPages?first_page=' .. cid .. suffix) then return false end

	local data = require 'utils.json'.decode(HTTP.Document.ToString())

	if suffix ~= '' then
		pages = data[gid]
	else
		pages = data.data
	end

	for _, v in ipairs(pages) do
		TASK.PageLinks.Add(v.url .. '?size=full')
	end

	return true
end