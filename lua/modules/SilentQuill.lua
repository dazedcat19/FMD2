----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Directory = '/series/'
local NextJs = require 'utils.nextjs'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. Directory

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//a[contains(@class, "flex min-h-16")]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('.//span[contains(@class, "line-clamp-2 text-sm")]', v))
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local s = HTTP.Document.ToString()
	local x = CreateTXQuery(s)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('(//img[contains(@class, "object-cover")])[1]/@src'))
	MANGAINFO.Authors   = x.XPathString('//p[@class="mt-2 text-sm text-ink-dim"]/substring-before(., " · art by")')
	MANGAINFO.Artists   = x.XPathString('//p[@class="mt-2 text-sm text-ink-dim"]/substring-after(., "· art by ")')
	MANGAINFO.Genres    = x.XPathStringAll('//a[contains(@href, "/?genre=")]')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[dt="Status"]/dd'))
	MANGAINFO.Summary   = Trim(x.XPathString('//div[@class="reader-content"]'))

	local roots = NextJs.GetRootObjects(s)
	local data
	for _, root in ipairs(roots) do
		data = NextJs.FindObject(root, function(v)
			if type(v) ~= 'table' or type(v.chapters) ~= 'table' then
				return false
			end
			local first = v.chapters[1]
			return type(first) == 'table' and first.slug ~= nil
		end)

		if data then
			break
		end
	end

	if not data then return no_error end

	local chapters = data.chapters
	for i = 1, #chapters do
		local ch = chapters[i]
		MANGAINFO.ChapterLinks.Add(ch.slug)
		MANGAINFO.ChapterNames.Add('Ch. ' .. ch.chapter_no)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL) .. '/'

	if not HTTP.GET(u) then return false end

	local roots = NextJs.GetRootObjects(HTTP.Document.ToString())
	for _, root in ipairs(roots) do
		local pages = NextJs.FindKey(root, 'pages')
		if type(pages) == 'table' then
			for _, page in ipairs(pages) do
				TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, page.url))
			end
		end
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '94caa4da3f334418b27bc1ba7e127b57'
	m.Name                     = 'SilentQuill'
	m.RootURL                  = 'https://silentquill.net'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end