----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

-- Extract slug from URL
function GetSlug(x)
	return string.match(string.gsub(string.gsub(URL, "read", ""), "manga", ""), "/([%a-]+)/")
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(MODULE.RootURL) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	x.XPathHREFAll('css("div.dropdown-menu > a")[contains(@href, "read/manga")]', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local slug = GetSlug(x)

	u = string.format('%s/api/series/%s/', MODULE.RootURL, slug)
	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('json(*)')

	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('cover', json))
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Summary   = x.XPathString('description', json)

	local chapters = [[
	for $k in jn:keys(chapters)
	return jn:object(object(("chapter_id", $k)), (chapters)($k))
	]]

	local v; for v in x.XPath(chapters, json).Get() do
		local id = x.XPathString('chapter_id', v)
		local w; for w in x.XPath('jn:keys(groups)', v).Get() do
			local group_id = w.ToString()
			local group = x.XPathString('(groups)(' .. group_id .. ')', json)
			local link = string.gsub(string.format('/read/manga/%s/%s/#%s', slug, id, group_id), "%.", "-")
			local title = string.format('%s - %s [%s]', id, x.XPathString('title', v), group)
			MANGAINFO.ChapterLinks.Add(link)
			MANGAINFO.ChapterNames.Add(title)
		end
	end
	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local group_id = URL:match('/#(%d+)$')
	local u = MaybeFillHost(MODULE.RootURL, URL:gsub('/#%d+$', ''))

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local slug = GetSlug(x)
	local ch = string.gsub(u:match('/([%d%-]+)/?$'), "-", ".")

	u = string.format('%s/api/series/%s/', MODULE.RootURL, slug)
	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	local json = x.XPath('(json(*).chapters)("' .. ch .. '")')
	local folder = x.XPathString('folder', json)

	local v; for v in x.XPath('jn:members((groups)(' .. group_id .. '))', json).Get() do
		local link = string.format('%s/media/manga/%s/chapters/%s/%s/%s',
			MODULE.RootURL, slug, folder, group_id, v.ToString())
		TASK.PageLinks.Add(link)
	end
	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------
function Init()
	local m = NewWebsiteModule()
	m.ID                = '7d12ff8d54d44137adbdf8fbe6f49bee'
	m.Name              = 'GuyaMoe'
	m.RootURL           = 'https://guya.moe'
	m.Category          = 'English'
	m.OnGetNameAndLink  = 'GetNameAndLink'
	m.OnGetInfo         = 'GetInfo'
	m.OnGetPageNumber   = 'GetPageNumber'
end
