----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api/v2'
local DOMAINS = {
	'https://kagane.si',
	'https://kagane.net',
	'https://kagane.cx',
	'https://kagane.moe',
}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Ordered list of domains to try for the next request.
local function GetDomainCandidates()
	local list, seen = {}, {}
	local function add(d)
		if d and d ~= '' and not seen[d] then
			list[#list + 1] = d
			seen[d] = true
		end
	end
	local pinned = MODULE.GetOption('domain') or 0
	if pinned >= 1 and pinned <= #DOMAINS then
		add(DOMAINS[pinned])
		return list
	end
	for _, d in ipairs(DOMAINS) do add(d) end

	return list
end

-- Remember the working domain for the next requests.
local function SetDomain(d)
	MODULE.Storage['domain'] = d
end

-- Request a URL path using the configured mirror fallback.
local function Request(method, urlpath, postdata)
	for _, d in ipairs(GetDomainCandidates()) do
		HTTP.Reset()
		if method == 'GET' then
			if HTTP.GET(d .. urlpath) then
				SetDomain(d)
				return d
			end
		else
			if postdata then
				HTTP.MimeType = 'application/json'
				if HTTP.POST(d .. urlpath, postdata) then
					SetDomain(d)
					return d
				end
			elseif HTTP.POST(d .. urlpath) then
				SetDomain(d)
				return d
			end
		end
	end

	return nil
end

local function Get(urlpath)
	return Request('GET', urlpath)
end

local function Post(urlpath, postdata)
	return Request('POST', urlpath, postdata)
end

-- Set the required http headers for making a request.
local function SetRequestHeaders(d)
	local now = os.time()
	local token  = MODULE.Storage['token@' .. d]
	local expiry = tonumber(MODULE.Storage['exp@' .. d] or '') or 0

	if token == '' or now > expiry then
		HTTP.Reset()
		if not HTTP.POST(d .. '/api/integrity') then return false end
		local body = HTTP.Document.ToString()
		MODULE.Storage['token@' .. d] = body:match('"token":"(.-)"') or ''
		MODULE.Storage['exp@' .. d]   = body:match('"exp":(%d+)') or ''
		token = MODULE.Storage['token@' .. d]
	end

	return token ~= ''
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local demographics = {
		['019c1fea-85c7-76a7-996a-b41ce0cb05f8'] = 'Shounen',
		['019c1b6f-fe1c-7eed-b93b-e1bf6dff63fd'] = 'Shoujo',
		['019c1fea-983f-7279-944e-8fb6e2cccdbc'] = 'Seinen',
		['019c1b70-101d-7c9c-a28c-cc2439104443'] = 'Josei'
	}
	local format = { 'Manga', 'Manhwa', 'Manhua', 'Comic', 'Other' }
	local mangastatus = { 'Ongoing', 'Completed', 'Hiatus', 'Abandoned' }
	local contentrating = { 'Safe', 'Suggestive', 'Erotica', 'Pornographic' }

	for _, fr in ipairs(format) do
		for dg, dgname in pairs(demographics) do
			for _, ms in ipairs(mangastatus) do
				for _, cr in ipairs(contentrating) do
					local total = 1
					local page = 1
					local limit = 100
					local totalpages = 1
					local order = 'asc'

					while page <= totalpages do

						if total == 1000 and order == 'asc' then
							page = 1
							order = 'desc'
						end

						local u = API_URL .. '/search/series?page=' .. (page - 1) .. '&size=' .. limit .. '&sort=created_at%2C' .. order
						local s = '{"content_rating":["' .. cr .. '"],"upload_status":["' .. ms .. '"],"format":["' .. fr .. '"],"genres":{"values":["' .. dg .. '"],"match_all":true}}'
						local d

						if page <= 10 then d = Post(u, s) end
						if not d then return net_problem end

						local x = require 'utils.json'.decode(HTTP.Document.ToString())
						total = tonumber(x.total_elements)
						totalpages = math.ceil(total / limit)
						UPDATELIST.UpdateStatusText(string.format(
							'Loading page %d of %d | Format: %s | Demographic: %s | Status: %s | Rating: %s | Order: %s',
							page, totalpages, fr, dgname, ms, cr, order
						))

						for _, data in ipairs(x.content or {}) do
							LINKS.Add('series/' .. data.series_id)
							NAMES.Add(data.title)
						end

						page = page + 1
					end
				end
			end
		end
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local id = URL:match('(%x%x%x%x%x%x%x%x%-%x%x%x%x%-%x%x%x%x%-%x%x%x%x%-%x%x%x%x%x%x%x%x%x%x%x%x)$')

	local d = Get(API_URL .. '/series/' .. id)
	if not d then return net_problem end

	MANGAINFO.Link = 'series/' .. id
	MANGAINFO.URL  = d .. '/series/' .. id
	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('parse-json(.)')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(series_alternate_titles?*?title, ", ")', info)
	MANGAINFO.CoverLink = d .. API_URL .. '/image/' .. x.XPathString('series_covers?1?image_id', info) .. '/compressed'
	MANGAINFO.Authors   = x.XPathString('string-join(series_staff?*[role=("Author","Story")]?name, ", ")', info)
	MANGAINFO.Artists   = x.XPathString('string-join(series_staff?*[role=("Artist","Art")]?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?genre_name, format), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('upload_status', info), 'Ongoing', 'Completed', 'Hiatus', 'Abandoned')
	MANGAINFO.Summary   = x.XPathString('description', info)

	local edition = x.XPathString('edition_info', info)
	if edition ~= '' then
		MANGAINFO.Title = MANGAINFO.Title .. ' (' .. edition .. ')'
	end

	if not Get(API_URL .. '/series/' .. id .. '/books/index?fields=List') then return net_problem end

	local optgroup = MODULE.GetOption('showscangroup')
	local titlemode = MODULE.GetOption('chaptertitle')
	local data = require 'utils.json'.decode(HTTP.Document.ToString())
	table.sort(data.books or {}, function(a, b)
		return (a[2] or 0) < (b[2] or 0)
	end)

	for _, v in ipairs(data.books or {}) do
		local vol = v[4] or ''
		local chap = v[3] or ''
		local title = v[7] or ''

		local group = ''
		for _, ix in ipairs(v[5] or {}) do
			local g = data.groups and data.groups[ix + 1]
			if g then
				group = group .. ((group ~= '') and ', ' or '') .. (g.title or '')
			end
		end

		local volume  = (vol ~= '') and ('Vol. ' .. vol .. ' ') or ''
		local chapter = (chap ~= '') and ('Ch. ' .. chap) or ''

		local name
		if titlemode == 0 then
			name = (title ~= '') and title or chapter
		elseif titlemode == 1 then
			name = chapter .. ((title ~= '') and (' - ' .. title) or '')
		else
			name = volume .. chapter .. ((title ~= '') and (' - ' .. title) or '')
		end

		local scanlator = ''
		if optgroup then
			scanlator = (group ~= '') and (' [' .. group .. ']') or ' [No Group]'
		end

		MANGAINFO.ChapterLinks.Add(v[1] or '')
		MANGAINFO.ChapterNames.Add(name .. scanlator)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local d
	for _, cand in ipairs(GetDomainCandidates()) do
		if SetRequestHeaders(cand) then
			HTTP.Reset()
			HTTP.Headers.Values['X-Integrity-Token'] = MODULE.Storage['token@' .. cand]
			if HTTP.POST(cand .. API_URL .. '/books' .. URL) then
				d = cand
				SetDomain(d)
				break
			end
		end
	end

	if not d then return false end

	local x = CreateTXQuery(HTTP.Document)
	local data = x.XPath('json(*)')
	local CDN_URL = x.XPathString('cache_url', data)
	local token = x.XPathString('access_token', data)
	local datasaver = MODULE.GetOption('datasaver')
	for v in x.XPath('manifest?pages?*', data).Get() do
		TASK.PageLinks.Add(string.format('%s/api/v2/books/page%s/%s.%s?token=%s&is_datasaver=%s',
			CDN_URL, URL, v.GetProperty('page_id').ToString(), v.GetProperty('ext').ToString(), token, datasaver))
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Origin'] = MODULE.Storage['domain']

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '01528d6f798b4a07ac3f074a5441ec7f'
	m.Name                     = 'Kagane'
	m.RootURL                  = table.concat(DOMAINS, ' ')
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['domain'] = 'Domain',
			['autodomain'] = 'Auto',
			['showscangroup'] = 'Show group name',
			['chaptertitle'] = 'Chapter title format',
			['titlemode'] = "Title only (e.g. 'Title' / 'Ch. 26')\nChapter and title (e.g. 'Ch. 26 - Title')\nVolume, chapter, and title (e.g. 'Vol. 3 Ch. 26 - Title')",
			['datasaver'] = 'Data saver'
		},
		['id_ID'] = {
			['domain'] = 'Domain',
			['autodomain'] = 'Otomatis',
			['showscangroup'] = 'Tampilkan nama grup',
			['chaptertitle'] = 'Format judul bab',
			['titlemode'] = "Hanya judul (Contoh 'Judul' / 'Ch. 26')\nBab dan judul (Contoh 'Ch. 26 - Judul')\nJilid, bab, dan judul (Contoh 'Vol. 3 Ch. 26 - Judul')",
			['datasaver'] = 'Penghemat data'
		}
	}
	local lang = translations[slang] or translations.en

	local domainitems = { lang.autodomain }
	for i = 1, #DOMAINS do
		domainitems[#domainitems + 1] = DOMAINS[i]:gsub('^https?://', '')
	end
	m.AddOptionCheckBox('showscangroup', lang.showscangroup, false)
	m.AddOptionCheckBox('datasaver', lang.datasaver, false)
	m.AddOptionComboBox('domain', lang.domain, table.concat(domainitems, '\n'), 0)
	m.AddOptionComboBox('chaptertitle', lang.chaptertitle, lang.titlemode, 0)
end