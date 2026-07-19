----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '01528d6f798b4a07ac3f074a5441ec7f'
	m.Name                     = 'Kagane'
	m.RootURL                  = 'https://kagane.to'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showscangroup'] = 'Show group name',
			['chaptertitle'] = 'Chapter title format',
			['titlemode'] = "Title only (e.g. 'Title' / 'Ch. 26')\nChapter and title (e.g. 'Ch. 26 - Title')\nVolume, chapter, and title (e.g. 'Vol. 3 Ch. 26 - Title')",
			['datasaver'] = 'Data saver'
		},
		['id_ID'] = {
			['showscangroup'] = 'Tampilkan nama grup',
			['chaptertitle'] = 'Format judul bab',
			['titlemode'] = "Hanya judul (Contoh 'Judul' / 'Ch. 26')\nBab dan judul (Contoh 'Ch. 26 - Judul')\nJilid, bab, dan judul (Contoh 'Vol. 3 Ch. 26 - Judul')",
			['datasaver'] = 'Penghemat data'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('showscangroup', lang.showscangroup, false)
	m.AddOptionCheckBox('datasaver', lang.datasaver, false)
	m.AddOptionComboBox('chaptertitle', lang.chaptertitle, lang.titlemode, 0)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = '/api/v2'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http headers for making a request.
local function SetRequestHeaders()
	local now = os.time()

	local token  = MODULE.Storage['token']
	local expiry = tonumber(MODULE.Storage['exp']) or 0

	if token == '' or now > expiry then
		HTTP.Reset()

		if not HTTP.POST(MODULE.RootURL .. '/api/integrity') then return net_problem end

		local body = HTTP.Document.ToString()
		MODULE.Storage['token'] = body:match('"token":"(.-)"')
		MODULE.Storage['exp'] = body:match('"exp":(%d+)')
	end
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

						local u = MODULE.RootURL .. API_URL .. '/search/series?page=' .. (page - 1) .. '&size=' .. limit .. '&sort=created_at%2C' .. order
						local s = '{"content_rating":["' .. cr .. '"],"upload_status":["' .. ms .. '"],"format":["' .. fr .. '"],"genres":{"values":["' .. dg .. '"],"match_all":true}}'
						HTTP.Reset()
						HTTP.MimeType = 'application/json'

						if page <= 10 and HTTP.POST(u, s) then
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
						else
							return net_problem
						end
					end
				end
			end
		end
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MODULE.RootURL .. API_URL .. URL

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(require 'fmd.crypto'.HTMLEncode(HTTP.Document.ToString()))
	local info = x.XPath('parse-json(.)')
	MANGAINFO.Title     = x.XPathString('title', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(series_alternate_titles?*?title, ", ")', info)
	MANGAINFO.CoverLink = MODULE.RootURL .. API_URL .. '/image/' .. x.XPathString('series_covers?1?image_id', info) .. '/compressed'
	MANGAINFO.Authors   = x.XPathString('string-join(series_staff?*[role=("Author","Story")]?name, ", ")', info)
	MANGAINFO.Artists   = x.XPathString('string-join(series_staff?*[role=("Artist","Art")]?name, ", ")', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*?genre_name, format), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('upload_status', info), 'Ongoing', 'Completed', 'Hiatus', 'Abandoned')
	MANGAINFO.Summary   = x.XPathString('description', info)

	local edition = x.XPathString('edition_info', info)
	if edition ~= '' then
		MANGAINFO.Title = MANGAINFO.Title .. ' (' .. edition .. ')'
	end

	local optgroup = MODULE.GetOption('showscangroup')
	local titlemode = MODULE.GetOption('chaptertitle')

	for v in x.XPath('series_books?*', info).Get() do
		local vol   = v.GetProperty('volume_no').ToString()
		local chap  = v.GetProperty('chapter_no').ToString()
		local title = v.GetProperty('title').ToString()
		local group = x.XPathString('string-join(groups?*?title, ", ")', v)

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
			if group and group ~= '' then
				scanlator = ' [' .. group .. ']'
			else
				scanlator = ' [No Group]'
			end
		end

		MANGAINFO.ChapterLinks.Add(v.GetProperty('book_id').ToString())
		MANGAINFO.ChapterNames.Add(name .. scanlator)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	SetRequestHeaders()

	HTTP.Reset()
	HTTP.Headers.Values['X-Integrity-Token'] = MODULE.Storage['token']

	local u = MODULE.RootURL .. API_URL .. '/books' .. URL

	if not HTTP.POST(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('json(*)')
	local CDN_URL = x.XPathString('cache_url', info)
	local token = x.XPathString('access_token', info)
	local datasaver = MODULE.GetOption('datasaver')
    for v in x.XPath('manifest?pages?*', info).Get() do
		TASK.PageLinks.Add(string.format('%s/api/v2/books/page%s/%s.%s?token=%s&is_datasaver=%s', 
		CDN_URL, URL, v.GetProperty('page_id').ToString(), v.GetProperty('ext').ToString(), token, datasaver))
    end

    return true
end