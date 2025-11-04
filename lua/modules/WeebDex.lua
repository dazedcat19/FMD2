----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5324f9766846418d9ebde2ce30d27678'
	m.Name                     = 'WeebDex'
	m.RootURL                  = 'https://weebdex.org'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.MaxTaskLimit             = 2
	m.MaxConnectionLimit       = 4

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local translations = {
		['en'] = {
			['showscangroup'] = 'Show scanlation group',
			['lang'] = 'Language:',
			['datasaver'] = 'Data saver'
		},
		['id_ID'] = {
			['showscangroup'] = 'Tampilkan grup scanlation',
			['lang'] = 'Bahasa:',
			['datasaver'] = 'Penghemat data'
		}
	}
	local lang = translations[slang] or translations['en']
	local items = table.concat(GetLangList(), '\r\n')
	m.AddOptionComboBox('lang', lang.lang, items, 1)
	m.AddOptionCheckBox('showscangroup', lang.showscangroup, false)
	m.AddOptionCheckBox('datasaver', lang.datasaver, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.weebdex.org'

local Langs = {
    { nil,   'All' },
    { 'en', 'English' },
    { 'ja', 'Japanese' }
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

-- Capitalize the first letter of a string
local function Capitalize(s)
    return s:gsub('^%l', string.upper)
end

-- Set the required http header for making a request.
local function SetRequestHeaders()
    HTTP.Headers.Values['Origin'] = MODULE.RootURL
    HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local demographics = { 'shounen', 'shoujo', 'josei', 'seinen', 'none' }
	local mangastatus = { 'ongoing', 'completed', 'hiatus', 'cancelled' }
	local contentrating = { 'safe', 'suggestive', 'erotica', 'pornographic' }

	for _, dg in ipairs(demographics) do
		for _, ms in ipairs(mangastatus) do
			for _, cr in ipairs(contentrating) do
				local total = 1
				local page = 1
				local limit = 100
				local totalpages = 1
				local order = 'asc'

				while page <= totalpages do

					if total > 10000 and page > 100 and order == 'asc' then
						page = 1
						order = 'desc'
					end

					local u = API_URL ..
					'/manga?limit=' .. limit ..
					'&sort=createdAt&page=' .. page ..
					'&order=' .. order ..
					'&demographic=' .. dg ..
					'&status=' .. ms ..
					'&contentRating=' .. cr
					SetRequestHeaders()

					if page <= 100 and HTTP.GET(u) then
						local x = json.decode(HTTP.Document.ToString())

						total = tonumber(x.total)
						totalpages = math.ceil(total / limit)

						if order == 'desc' then
							totalpages = math.ceil((total - 10000) / limit)
						end

						local displaytotal = (order == 'asc' and totalpages > 100) and 100 or totalpages

						UPDATELIST.UpdateStatusText(string.format(
							'Loading page %d of %d | Demographic: %s | Status: %s | Rating: %s | Order: %s',
							page, displaytotal,
							Capitalize(dg),
							Capitalize(ms),
							Capitalize(cr),
							Capitalize(order)
						))

						for _, data in ipairs(x.data or {}) do
							LINKS.Add('manga/' .. data.id)
							NAMES.Add(data.title)
						end

						page = page + 1
					elseif page > 100 then
						print('Page over max limit! Total page: ' .. totalpages .. ' ' .. string.format(
							'| Demographic: %s | Status: %s | Rating: %s | Order: %s',
							Capitalize(dg),
							Capitalize(ms),
							Capitalize(cr),
							Capitalize(order)
						))
					else
						return net_problem
					end
				end
			end
		end
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local mid = URL:match('/title/([^/]+)')
	local u = API_URL .. '/manga/' .. mid
	SetRequestHeaders()

	if not HTTP.GET(u) then return net_problem end

	local x = json.decode(HTTP.Document.ToString())
	MANGAINFO.Title     = x.title
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.status)
	MANGAINFO.Summary   = x.description

	local alttitles = {}
	local keys = {}
	for lang in pairs(x.alt_titles or {}) do
		table.insert(keys, lang)
	end
	table.sort(keys)
	for _, lang in ipairs(keys) do
		for _, title in ipairs(x.alt_titles[lang]) do
			table.insert(alttitles, title)
		end
	end
	MANGAINFO.AltTitles = table.concat(alttitles, ', ')

	local cover = x.relationships.cover
	if cover then
		MANGAINFO.CoverLink = 'https://srv.notdelta.xyz/covers/' .. mid .. '/' .. cover.id .. '.256.webp'
	end

	local authors = {}
	for _, author in ipairs(x.relationships.authors or {}) do
		table.insert(authors, author.name)
	end
	MANGAINFO.Authors = table.concat(authors, ', ')

	local artists = {}
	for _, artist in ipairs(x.relationships.artists or {}) do
		table.insert(artists, artist.name)
	end
	MANGAINFO.Artists = table.concat(artists, ', ')

	local genres = {}
	for _, genre in ipairs(x.relationships.tags or {}) do
		table.insert(genres, genre.name)
	end
	MANGAINFO.Genres = table.concat(genres, ', ')

	local demographic = x.demographic
	if demographic then
		MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. Capitalize(demographic)
	end

	local optgroup  = MODULE.GetOption('showscangroup')
	local optlang   = MODULE.GetOption('lang')
	local optlangid = FindLanguage(optlang)
	local page = 1
	local langparam = optlangid and '&tlang=' .. optlangid or ''

	while true do
		SetRequestHeaders()
		if not HTTP.GET(u .. '/chapters?order=asc&limit=100&page=' .. tostring(page) .. langparam) then return net_problem end
		local x = json.decode(HTTP.Document.ToString())
		for _, chapters in ipairs(x.data or {}) do

			local groups = {}
			for _, group in ipairs(chapters.relationships.groups or {}) do
				table.insert(groups, group.name)
			end

			local volume = (chapters.volume ~= nil and chapters.volume ~= '') and ('Vol. ' .. chapters.volume .. ' ') or ''
			local chapter = (chapters.chapter ~= nil and chapters.chapter ~= '') and ('Ch. ' .. chapters.chapter) or ''
			local title = (chapters.title ~= nil and chapters.title ~= '') and (' - ' .. chapters.title) or ''

			if volume == '' and chapter == '' and title == '' then title = 'Oneshot' end

			local language = (optlang == 0) and (' [' .. chapters.language .. ']') or ''

			local scanlators = ''
			if optgroup then
				if #groups == 0 then
					scanlators = ' [No Group]'
				else
					scanlators = ' [' .. table.concat(groups, ', ') .. ']'
				end
			end

			MANGAINFO.ChapterLinks.Add(chapters.id)
			MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. string.upper(language) .. scanlators)
		end

		page = page + 1
		local pages = tonumber(math.ceil(x.total / x.limit)) or 1
		if page > pages then
			break
		end
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local cid = URL:gsub('^/', '')
	local u = API_URL .. '/chapter/' .. cid
	SetRequestHeaders()

	if not HTTP.GET(u) then return false end

	local x = json.decode(HTTP.Document.ToString())
	local node = x.node
	local data = MODULE.GetOption('datasaver') and x.data_optimized or x.data

	for _, v in ipairs(data) do
		TASK.PageLinks.Add(node .. '/data/' .. cid .. '/' .. v.name)
	end

	return true
end