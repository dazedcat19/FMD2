local API_URL = 'https://api.comick.fun'

function Init()
	local m = NewWebsiteModule()
	m.ID                         = '214e30f0afec420cafddefe22b4d973c'
	m.Name                       = 'ComicK'
	m.RootURL                    = 'https://comick.io'
	m.Category                   = 'English'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['showscangroup'] = 'Show scanlation group',
			['lang'] = 'Language:'
		},
		['id_ID'] = {
			['showscangroup'] = 'Tampilkan grup scanlation',
			['lang'] = 'Bahasa:'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	local items = 'All'
	local t = GetLangList()
	for k, v in ipairs(t) do items = items .. '\r\n' .. v; end
	m.AddOptionComboBox('lang', lang:get('lang'), items, 11)
	m.AddOptionCheckBox('showscangroup', lang:get('showscangroup'), false)
end

function parse_iso8601(date_str)
    local pattern = "(%d+)%-(%d+)%-(%d+)T(%d+):(%d+):(%d+)"
    local year, month, day, hour, min, sec = date_str:match(pattern)
    return os.time({
        year = year,
        month = month,
        day = day,
        hour = hour,
        min = min,
        sec = sec
    })
end

function GetNameAndLink()
	if HTTP.GET(API_URL .. '/v1.0/search?limit=300&page=' .. (URL + 1) .. '&sort=uploaded') then
		local x = CreateTXQuery(HTTP.Document)
		if x.XPathString('json(*)().title') == '' then return no_error end
		local v for v in x.XPath('json(*)()').Get() do
			LINKS.Add('comic/' .. x.XPathString('slug', v))
			NAMES.Add(x.XPathString('title', v))
		end
		UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	local s = API_URL .. '/comic/' .. URL:match('/comic/(.-)$')
	if HTTP.GET(s) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathString('json(*).comic.title')
		MANGAINFO.CoverLink  = 'https://meo3.comick.pictures/' .. x.XPathString('json(*).comic.md_covers().b2key')
		MANGAINFO.Authors    = x.XPathStringAll('json(*).authors().name')
		MANGAINFO.Artists    = x.XPathStringAll('json(*).artists().name')
		MANGAINFO.Status     = MangaInfoStatusIfPos(x.XPathString('json(*).comic.status'), '1', '2')
		MANGAINFO.Summary    = x.XPathString('json(*).comic.desc')

		MANGAINFO.Genres     = x.XPathStringAll('json(*).comic.md_comic_md_genres().md_genres.name')
		local demographic    = x.XPathString('json(*).demographic')
		if demographic == 'null' then demographic = '' end
		if demographic ~= '' then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. demographic end

		local mangaId   = x.XPathString('json(*).comic.hid')
		local optgroup  = MODULE.GetOption('showscangroup')
		local optlang   = MODULE.GetOption('lang')
		local optlangid = FindLanguage(optlang)
		local page = 1
		local langparam

		if optlangid == nil then langparam = '' else langparam = '&lang=' .. optlangid end

		while true do
			if HTTP.GET(API_URL .. '/comic/' .. mangaId .. '/chapters?page=' .. tostring(page) .. langparam) then
				local x = CreateTXQuery(HTTP.Document)
				local chapters = x.XPath('json(*).chapters()')
				for ic = 1, chapters.Count do
					local ignore = false

					-- Ignore chapter if it has delayed time
					local pa = x.XPathString('json(*).chapters()[' .. ic .. '].publish_at')
					if pa ~= 'null' then
						if parse_iso8601(pa) >= os.time(os.date('!*t')) then
							ignore = true
						end
					end

					local groupids = x.XPath('json(*).chapters()[' .. ic .. '].md_chapters_groups().md_groups.title')
					if groupids.Count == 0 then groupids = x.XPath('json(*).chapters()[' .. ic .. '].group_name()') end
					local groups = {}
					for gid = 1, groupids.Count do
						groups[gid] = groupids.Get(gid).ToString()
					end

					if ignore == false then
						local title    = x.XPathString('title', chapters.Get(ic))
						local volume   = x.XPathString('vol', chapters.Get(ic))
						local chapter  = x.XPathString('chap', chapters.Get(ic))
						local scanlators = ' [' .. table.concat(groups, ", ") .. ']'

						title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''
						volume = volume ~= 'null' and string.format('Vol. %s ', volume) or ''
						chapter = chapter ~= 'null' and string.format('Ch. %s ', chapter) or ''

						if optlang == 0 then language = string.format(' [%s]', language) else language = '' end

						if optgroup then
							if groupids.Count == 0 then scanlators = ' [no group]' end
						else
							scanlators = ''
						end

						MANGAINFO.ChapterLinks.Add(x.XPathString('hid', chapters.Get(ic)))
						MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. language .. scanlators)
					end
				end
				page = page + 1
				pages = tonumber(math.ceil(x.XPathString('json(*).total') / x.XPathString('json(*).limit'))) or 1
				if page > pages then
					break
				end
			else
				break
			end
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(API_URL .. '/chapter' .. URL .. '?tachiyomi=true') then
		local x = CreateTXQuery(HTTP.Document)
		local v for v in x.XPath('json(*).chapter.images().url').Get() do
			TASK.PageLinks.Add(v.ToString())
		end
		return true
	else
		return false
	end
end

local Langs = {
	["ar"] = "Arabic",
	["bn"] = "Bengali",
	["bg"] = "Bulgarian",
	["my"] = "Burmese",
	["ca"] = "Catalan",
	["zh"] = "Chinese (Simp)",
	["zh-hk"] = "Chinese (Trad)",
	["cs"] = "Czech",
	["da"] = "Danish",
	["nl"] = "Dutch",
	["en"] = "English",
	["fr"] = "French",
	["de"] = "German",
	["el"] = "Greek",
	["he"] = "Hebrew",
	["hi"] = "Hindi",
	["hu"] = "Hungarian",
	["id"] = "Indonesian",
	["it"] = "Italian",
	["ja"] = "Japanese",
	["ko"] = "Korean",
	["lt"] = "Lithuanian",
	["ms"] = "Malay",
	["mn"] = "Mongolian",
	["no"] = "Norwegian",
	["fa"] = "Persian",
	["pl"] = "Polish",
	["pt-br"] = "Portuguese (Br)",
	["pt"] = "Portuguese (Pt)",
	["ro"] = "Romanian",
	["ru"] = "Russian",
	["es"] = "Spanish (Es)",
	["es-419"] = "Spanish (LATAM)",
	["sv"] = "Swedish",
	["tl"] = "Tagalog",
	["th"] = "Thai",
	["tr"] = "Turkish",
	["uk"] = "Ukrainian",
	["vi"] = "Vietnamese"
}
		
function GetLangList()
	local t = {}
	for k, v in pairs(Langs) do table.insert(t, v); end
	table.sort(t)
	return t
end

function FindLanguage(lang)
	local t = GetLangList()
	for i, v in ipairs(t) do
		if i == lang then
			lang = v
			break
		end
	end
	for k, v in pairs(Langs) do
		if v == lang then return k; end
	end
	return nil
end