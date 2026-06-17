----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'a791d82a6e584cd0909029560bbb4926'
	m.Name                     = 'OniSaga'
	m.RootURL                  = 'https://onisaga.com'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetImageURL            = 'GetImageURL'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.MaxTaskLimit             = 1
	m.MaxThreadPerTaskLimit    = 1
	m.SortedList               = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['lang'] = 'Language:'
		},
		['id_ID'] = {
			['lang'] = 'Bahasa:'
		}
	}
	local lang = translations[slang] or translations.en
	local items = table.concat(GetLangList(), '\r\n')
	m.AddOptionComboBox('lang', lang.lang, items, 1)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Langs = {
	{   nil, 'All' },
	{  'EN', 'English' },
	{  'FR', 'French' },
	{  'JA', 'Japanese' },
	{ 'PT-BR', 'Portuguese (Br)' },
	{  'PT', 'Portuguese (Pt)' },
	{ 'ES-LA', 'Spanish (LATAM)' },
	{  'ES', 'Spanish (Es)' }
}

local json = require 'utils.json'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Return language names in defined order
function GetLangList()
	local t = {}
	for _, v in ipairs(Langs) do
		table.insert(t, v[2])
	end
	return t
end

-- Return language key by index
local function FindLanguage(lang)
	return Langs[lang + 1][1]
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. '/browse?sort=release_date'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local ss = x.XPath('parse-json(//div[contains(@*[name()="snapshot"], "post-filter")]/@*[name()="snapshot"])')
	MODULE.Storage['token'] = x.XPathString('(//input[@name="_token"])[1]/@value')
	MODULE.Storage['id'] = x.XPathString('memo?id', ss)
	MODULE.Storage['checksum'] = x.XPathString('checksum', ss)

	PAGENUMBER = tonumber(x.XPathString('//div[contains(@class, "gap-2 mt-8 mb-8")]/button[last()-1]/span')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/livewire/update'
	local s = '{"_token":"' .. MODULE.Storage['token'] .. '","components":[{"snapshot":"{\\"data\\":{\\"param\\":[{\\"type\\":null,\\"genre\\":[[],{\\"s\\":\\"arr\\"}],\\"heading\\":\\"Browse\\"},' ..
	'{\\"s\\":\\"arr\\"}],\\"genre\\":[[],{\\"s\\":\\"arr\\"}],\\"scene\\":null,\\"release\\":null,\\"vote_average\\":null,\\"platform\\":null,\\"type\\":null,\\"search\\":null,\\"sort\\":\\"release_date\\",' ..
	'\\"status\\":null,\\"min_chapters\\":null,\\"group\\":null,\\"excludeGenre\\":[[],{\\"s\\":\\"arr\\"}],\\"release_start\\":null,\\"release_end\\":null,\\"loading\\":false,\\"filterOpen\\":false,\\"openSort\\":false,' ..
	'\\"paginators\\":[{\\"page\\":1},{\\"s\\":\\"arr\\"}]},\\"memo\\":{\\"id\\":\\"' .. MODULE.Storage['id'] .. '\\",\\"name\\":\\"post-filter\\",\\"path\\":\\"browse\\",\\"method\\":\\"GET\\",' ..
	'\\"release\\":\\"a-a-a\\",\\"children\\":[],\\"scripts\\":[],\\"assets\\":[],\\"errors\\":[],\\"locale\\":\\"en\\"},\\"checksum\\":\\"' .. MODULE.Storage['checksum'] .. '\\"}","updates":{},' ..
	'"calls":[{"path":"","method":"gotoPage","params":[' .. (URL + 1) .. ']}]}]}'

	HTTP.MimeType = 'application/json'

	if not HTTP.POST(u, s) then return net_problem end

	if HTTP.ResultCode ~= 200 then print('Cloudflare workaround is required') return no_error end
	CreateTXQuery(json.decode(HTTP.Document.ToString()).components[1].effects.html).XPathHREFTitleAll('//a[@title]', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local slug = URL:match('/([^/]+)$')
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//p[contains(@class, "text-[13px]")] ! replace(., " [|]| ·", ",")')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="w-full h-full object-cover"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//a[contains(@href, "/author/")]')
	MANGAINFO.Genres    = x.XPathStringAll('(//a[contains(@href, "/genre/")], //div[contains(@class, "bg-violet-400/20")], upper-case(substring(//div[contains(@class, "uppercase tracking-widest")], 1, 1)) || lower-case(substring(//div[contains(@class, "uppercase tracking-widest")], 2)))')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "gap-1.5 text-[10px]")]'), 'Ongoing|Releasing')
	MANGAINFO.Summary   = x.XPathString('//p[contains(@class, "leading-relaxed line-clamp-2")]')

	local ss = json.decode(x.XPathString('//div[contains(@*[name()="snapshot"], "manga.chapter-list")]/@*[name()="snapshot"]'))
	local token = x.XPathString('//input[@name="_token"]/@value')
	local key = ss.data.manga[2].key
	local id = ss.memo.id
	local checksum = ss.checksum

	local page = 1
	local pages = math.ceil(x.XPathString('//div[contains(@class, "whitespace-nowrap -mt-1 -mb-1")]') / 100 - 1) or 1
	while true do
		local s = '{"_token":"' .. token .. '","components":[{"snapshot":"{\\"data\\":{\\"manga\\":[null,{\\"class\\":\\"App\\\\\\\\Models\\\\\\\\Post\\",\\"key\\":' .. key .. ',\\"s\\":\\"mdl\\"}],' ..
		'\\"view\\":\\"chapters\\",\\"sortOrder\\":\\"asc\\",\\"search\\":\\"\\",\\"groupFilter\\":null,\\"chaptersLoaded\\":' .. page .. ',\\"volumesLoaded\\":1,\\"rateLimited\\":false,\\"importingChapters\\":false},\\"memo\\":{\\"id\\":\\"' .. id ..
		'\\",\\"name\\":\\"manga.chapter-list\\",\\"path\\":\\"manga\\\\/' .. slug .. '\\",\\"method\\":\\"GET\\",\\"release\\":\\"a-a-a\\",\\"children\\":[],\\"scripts\\":[],\\"assets\\":[],\\"errors\\":[],' ..
		'\\"locale\\":\\"en\\"},\\"checksum\\":\\"' .. checksum .. '\\"}","updates":{},"calls":[{"path":"","method":"loadMoreChapters","params":[]}]}]}'

		HTTP.Reset()
		HTTP.MimeType = 'application/json'

		if not HTTP.POST(MODULE.RootURL .. '/livewire/update', s) then return net_problem end

		if HTTP.ResultCode ~= 200 then MANGAINFO.Title = 'Cloudflare workaround is required' return no_error end
		local data = json.decode(HTTP.Document.ToString()).components[1]
		checksum = json.decode(data.snapshot).checksum
		x.ParseHTML(data.effects.html)

		local optlang   = MODULE.GetOption('lang')
		local optlangid = FindLanguage(optlang)

		for v in x.XPath('//div[@class="relative"]//a').Get() do
			local title = x.XPathString('.//div[@data-flux-heading]', v)
			local language

			if title == '' then
				title = x.XPathString('ancestor::ui-dropdown[1]//div[@data-flux-heading]', v)
				language = x.XPathString('.//div[@data-flux-badge]', v)
			else
				language = x.XPathString('tokenize(normalize-space(.//p[@data-flux-text]), " · ")[last()]', v)
			end

			if not optlangid or language == optlangid then
				local volume = x.XPathString('.//p[@data-flux-text]/span', v)
				if volume ~= '' then volume = volume .. ' ' end

				local lang = (optlang == 0) and (' [' .. language .. ']') or ''

				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(volume .. title .. lang)
			end
		end
		page = page + 1
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local body = HTTP.Document.ToString()
	MODULE.Storage[URL] = body:match('readerToken:%s*"([^"]+)"')
	for order in body:gmatch('"order":(%d+)') do
		TASK.PageContainerLinks.Add(order)
	end
	TASK.PageNumber = TASK.PageContainerLinks.Count

	return true
end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
	sleep(1500)
	local cid = URL:match('/(%d+)$')
	local u = MODULE.RootURL .. '/api/chapter/' .. cid .. '/page/' .. TASK.PageContainerLinks[WORKID]
	HTTP.Headers.Values['X-Reader-Token'] = MODULE.Storage[URL]
	HTTP.Headers.Values['Sec-Fetch-Mode'] = 'cors'
	HTTP.Headers.Values['Sec-Fetch-Site'] = 'same-origin'

	if not HTTP.GET(u) then return false end

	local s = HTTP.Document.ToString()
	if s:find('expired', 1, true) then
		if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return false end

		MODULE.Storage[URL] = HTTP.Document.ToString():match('readerToken:%s*"([^"]+)"')
		HTTP.Reset()
		HTTP.Headers.Values['X-Reader-Token'] = MODULE.Storage[URL]
		HTTP.Headers.Values['Sec-Fetch-Mode'] = 'cors'
		HTTP.Headers.Values['Sec-Fetch-Site'] = 'same-origin'

		if not HTTP.GET(u) then return false end

		s = HTTP.Document.ToString()
	end

	TASK.PageLinks[WORKID] = CreateTXQuery(s).XPathString('json(*).url')

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])

	return true
end