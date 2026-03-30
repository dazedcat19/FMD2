----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'cd31ebc0ca51467688568da8e9a85ad4'
	m.Name                     = 'Baozi (org)'
	m.RootURL                  = 'https://baozimh.org'
	m.Category                 = 'Raw'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end

local function LoadHeaders()
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	HTTP.Headers.Values['Cache-Control'] = 'no-cache'
	HTTP.Headers.Values['Pragma'] = 'no-cache'
end

----------------------------------------------------------------------------------------------------
-- Get manga list 
----------------------------------------------------------------------------------------------------

function GetNameAndLink()
	-- api not found
	return no_error
end

----------------------------------------------------------------------------------------------------
-- Get Info + Chapters
----------------------------------------------------------------------------------------------------

function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)

	-- 🔑 get mid
	local mid = x.XPathString('//div[@id="mangachapters"]/@data-mid')

	if mid == '' then return no_error end

	-- 🔥 API request
	local api = 'https://api-get-v3.mgsearcher.com/api/manga/get?mid=' .. mid .. '&mode=all&_=' .. os.time()
	LoadHeaders()

	if not HTTP.GET(api) then return net_problem end

	local json = CreateTXQuery(HTTP.Document)

	MANGAINFO.Title   = json.XPathString('json(*).data.title')
	MANGAINFO.Summary = json.XPathString('json(*).data.desc')
	MANGAINFO.CoverLink = json.XPathString('json(*).data.cover')

	for v in json.XPath('json(*).data.chapters()').Get() do
		local cid = v.GetProperty('id').ToString()
		local title = v.GetProperty('attributes').GetProperty('title').ToString()

		MANGAINFO.ChapterLinks.Add(mid .. '|' .. cid)
		MANGAINFO.ChapterNames.Add(title)
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Get Pages
----------------------------------------------------------------------------------------------------

function GetPageNumber()
	local mid, cid = URL:match('(.+)|(.+)')

	mid = mid:gsub('/', '')

	local api = 'https://api-get-v3.mgsearcher.com/api/chapter/getinfo?m=' .. mid .. '&c=' .. cid
	LoadHeaders()

	if not HTTP.GET(api) then return false end

	local json = CreateTXQuery(HTTP.Document)

	-- 🔑 Dinamic host detect
	local line = json.XPathString('json(*).data.info.images.line')

	local host = ''

	if line == '3' then
		host = 'https://t40-2-4.g-mh.online'
	elseif line == '2' then
		host = 'https://t40-1-4.g-mh.online'
	else
		host = 'https://t40-1-4.g-mh.online'
	end

	for v in json.XPath('json(*).data.info.images.images()').Get() do
		local url = v.GetProperty('url').ToString()
		TASK.PageLinks.Add(host .. url)
	end

	return true
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	return true
end
