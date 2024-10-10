----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://api.mghcdn.com/graphql'
CDN_URL = 'https://imgx.mghcdn.com/'
json = require "utils.json"
DirectoryPerPage = 30
Variables = ''

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local data, mhub_access = nil
	local s = '{"query":"{search(x:' .. Variables .. ',q:\\"\\",genre:\\"all\\",mod:ALPHABET,count:true,offset:0){rows{id,rank,title,slug,status,author,genres,image,latestChapter,unauthFile,createdDate},count}}"}'

	if not HTTP.GET(MODULE.RootURL) then return net_problem end

	mhub_access = HTTP.Cookies.Values['mhub_access']
	HTTP.Reset()
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['X-Mhub-Access'] = mhub_access
	HTTP.MimeType = 'application/json'
	if HTTP.POST(API_URL, s) then
		data = json.decode(HTTP.Document.ToString())
		PAGENUMBER = math.ceil(data.data.search.count / DirectoryPerPage)
	end

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local data, mhub_access, v = nil
	local offset = DirectoryPerPage * tonumber(URL)
	local s = '{"query":"{search(x:' .. Variables .. ',q:\\"\\",genre:\\"all\\",mod:ALPHABET,count:true,offset:' .. tostring(offset) .. '){rows{id,rank,title,slug,status,author,genres,image,latestChapter,unauthFile,createdDate},count}}"}'

	if not HTTP.GET(MODULE.RootURL) then return net_problem end

	mhub_access = HTTP.Cookies.Values['mhub_access']
	HTTP.Reset()
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['X-Mhub-Access'] = mhub_access
	HTTP.MimeType = 'application/json'
	if HTTP.POST(API_URL, s) then
		data = json.decode(HTTP.Document.ToString()).data.search.rows
		for _, v in ipairs(data) do
			LINKS.Add('/manga/' .. v.slug)
			NAMES.Add(v.title)
		end
	end

	return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@id="mangadetail"]//h1/text()')
	MANGAINFO.CoverLink = x.XPathString('//div[@id="mangadetail"]//img/@src')
	MANGAINFO.Authors   = x.XPathString('//div[@id="mangadetail"]//div/span[contains(., "Author")]/following-sibling::span')
	MANGAINFO.Artists   = x.XPathString('//div[@id="mangadetail"]//div/span[contains(., "Artist")]/following-sibling::span')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@id="mangadetail"]//div/p/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@id="mangadetail"]//div/span[contains(., "Status")]/following-sibling::span'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="tab-content"]/div[last()]//p')
	
	for v in x.XPath('//li[contains(@class, "list-group-item")]//a[not(@rel)]').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span/span[1]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local mhub_access, p, s, x, v = nil
	local chapter = URL:match('/chapter%-(.+)$'):gsub('/$', '')
	local slug = URL:match('/chapter/(.+)/')
	local s = '{"query":"{chapter(x:' .. Variables .. ',slug:\\"' .. slug ..'\\",number:' .. chapter .. '){id,title,mangaID,number,slug,date,pages,noAd,manga{id,title,slug,mainSlug,author,isWebtoon,isYaoi,isPorn,isSoftPorn,unauthFile,isLicensed}}}"}'

	if not HTTP.GET(MODULE.RootURL) then return net_problem end

	mhub_access = HTTP.Cookies.Values['mhub_access']
	HTTP.Reset()
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['X-Mhub-Access'] = mhub_access
	HTTP.MimeType = 'application/json'
	if HTTP.POST(API_URL, s) then
		x = CreateTXQuery(HTTP.Document)
		x.ParseHTML(x.XPathString('json(*).data.chapter.pages'))
		p = x.XPathString('json(*).p')
		for v in x.XPath('json(*).i()').Get() do
			TASK.PageLinks.Add(CDN_URL .. p .. v.ToString())
		end
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M