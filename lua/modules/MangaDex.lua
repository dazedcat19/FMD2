htmlEntities = require('utils.htmlEntities')
local API_PATH = '/api/v2' -- This is the path to the JSON API. Call the full url to look at the API documentation.
local API_PARAMS = '?include=chapters' -- This parameter minimizes the calls to the API by combining the info and chapter parts into one call instead of two. Getting manga info should only be one call overall.
local API_CHAPTER_PARAMS = '?saver=false' -- This parameter forces the API to always deliver the source images instead of the data-saver low quality images. Default of the API is actually false, but this prevents that you download data-saver images if the default will ever be changed.

function GetInfo()
	-- Extract Manga ID which is needed for getting info and chapter list:
	local mid = URL:match('title/(%d+)')
	if mid == nil then mid = URL:match('manga/(%d+)') end

	-- Delay this task if configured:
	Delay()

	-- Fetch JSON from API:
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, API_PATH .. '/manga/' .. mid .. API_PARAMS)) then
		local crypto = require 'fmd.crypto'
		local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))

		local minfo    = x.XPath('json(*)')
		local mcode    = x.XPathString('code', minfo)
		local mstatus  = x.XPathString('status', minfo)
		local mmessage = x.XPathString('message', minfo)

		-- Handle the returned JSON if code is 200. Any other code will result in an error that is being shown in the description of the Mangainfo.
		-- If no code is returned, the API certainly didn't respond. It's probably not reachable if that happens.
		if mcode == '200' then
			MANGAINFO.Title     = x.XPathString('data/manga/title', minfo)
			MANGAINFO.CoverLink = x.XPathString('data/manga/mainCover', minfo)
			MANGAINFO.Authors   = x.XPathStringAll('json(*).data.manga.author()')
			MANGAINFO.Artists   = x.XPathStringAll('json(*).data.manga.artist()')
			MANGAINFO.Summary   = htmlEntities.decode(x.XPathString('data/manga/description', minfo))
			MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('data/manga/publication/status', minfo), '1', '2')

			-- Fetch genre/demographic IDs and match them against an array with corresponding names:
			local gids = x.XPath('json(*).data.manga.tags()')
			local dgid = x.XPathString('data/manga/publication/demographic', minfo)
			MANGAINFO.Genres = GetGenre(gids.Get(1).ToString())
			if gids.Count > 1 then
				for i = 2, gids.Count do
					MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. GetGenre(gids.Get(i).ToString())
				end
			end
			if (dgid ~= '0') and (MANGAINFO.Genres ~= '') then
				MANGAINFO.Genres = GetDemographic(dgid) .. ', ' .. MANGAINFO.Genres
			elseif (dgid ~= '0') and (MANGAINFO.Genres == '') then
				MANGAINFO.Genres = GetDemographic(dgid)
			end

			-- If Manga is Hentai, add it to genre list:
			if x.XPathString('data/manga/isHentai', minfo) == 'true' then
				if MANGAINFO.Genres ~= '' then MANGAINFO.Genres =  'Hentai, ' .. MANGAINFO.Genres else MANGAINFO.Genres = 'Hentai' end
			end

			-- Fetch and map all groups involved with the current manga chapters. The following array can be further used to get the group name by id:
			local grouplist = {}
			local g for g in x.XPath('json(*).data.groups()').Get() do
				grouplist[x.XPathString('id', g)] = x.XPathString('name', g)
			end

			-- Get user defined options for fetching all chapters respecting these options:
			local optlang  = MODULE.GetOption('lualang')
			local optgroup = MODULE.GetOption('luashowscangroup')
			local opttitle = MODULE.GetOption('luashowchaptertitle')

			-- Map options with values/IDs from arrays:
			local optlangid = FindLanguage(optlang)

			-- Get all chapters:
			local chapters = x.XPath('json(*).data.chapters()')
			for ic = 1, chapters.Count do
				local ignore = false

				-- Check if the language of the chapter is matching with the user selection, otherwise skip the chapter:
				local language = x.XPathString('language', chapters.Get(ic))
				if (optlangid ~= language) and (optlang > 0) then ignore = true end

				-- Check if the timestamp is in the future, skip the chapter if true:
				local timestamp = tonumber(x.XPathString('timestamp', chapters.Get(ic)))
				if timestamp > os.time() then ignore = true end

				-- Check if the group that released the chapter is on the built-in ignore list, otherwise skip the chapter:
				local groupids = x.XPath('json(*).data.chapters()[' .. ic .. '].groups()') -- This is a fuckin' workaround because the direct approach by using "x.XPath('groups').Get(i).ToString" does not work as the array will be combined to a single string.
				if ignore == false then
					for i = 1, groupids.Count do
						if ignore == false then -- Chapter should be ignored if at least one group is on the ignore list. This check prevents that the ignore value is being set back to false if a second group is not in the list.
							ignore = IgnoreChaptersByGroupId(groupids.Get(i).ToString())
						end
					end
				end

				-- If at least one group is in the ignore list or if the timestamp is in the future skip the current chapter:
				if ignore == false then

					-- Initialize some variables for building the chapter name:
					local title   = x.XPathString('title', chapters.Get(ic))
					local volume  = x.XPathString('volume', chapters.Get(ic))
					local chapter = x.XPathString('chapter', chapters.Get(ic))
					local group   = ''

					-- Remove title if user option is disabled:
					if opttitle == false then title = '' end

					-- Add prefix to title, if it's not empty:
					if title ~= '' then title = ' - ' .. title end

					-- Complete volume and chapter strings if not empty:
					if volume ~= '' then volume = string.format('Vol. %s ', volume) end
					if chapter ~= '' then chapter = string.format('Ch. %s', chapter) end

					-- Append language id if user option is set to "All":
					if optlang == 0 then language = string.format(' [%s]', language) else language = '' end

					-- Get the group names if the user option is enabled:
					if optgroup then
						group = ' [' .. grouplist[groupids.Get(1).ToString()]
						if groupids.Count > 1 then
							for i = 2, groupids.Count do
								group = group .. ', ' .. grouplist[groupids.Get(i).ToString()]
							end
						end
						group = group .. ']'
					end

					-- Add chapter name and link to the manga info list:
					MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. language .. group)
					MANGAINFO.ChapterLinks.Add('/chapter/' .. x.XPathString('id', chapters.Get(ic)))

				end
			end

			-- Sort the chapter list from oldest to latest:
			MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

		elseif mcode == '' then
			MANGAINFO.Summary = 'Manga information could not be fetched. Please check if you can access the Manga page in your browser.'
			return net_problem

		else
			MANGAINFO.Summary = mstatus .. ' (' .. mcode .. '):   ' .. mmessage
			return net_problem

		end
		return no_error
	else
		return net_problem
	end
end

function IgnoreChaptersByGroupId(id)
	local groups = {
		["9097"] = "MangaPlus"
	}

	if groups[id] ~= nil then
		return true
	else
		return false
	end
end

function GetGenre(genre)
	local genres = {
		["1"] = "4-koma",
		["2"] = "Action",
		["3"] = "Adventure",
		["4"] = "Award Winning",
		["5"] = "Comedy",
		["6"] = "Cooking",
		["7"] = "Doujinshi",
		["8"] = "Drama",
		["9"] = "Ecchi",
		["10"] = "Fantasy",
		["11"] = "Gyaru",
		["12"] = "Harem",
		["13"] = "Historical",
		["14"] = "Horror",
		["15"] = "Josei",
		["16"] = "Martial Arts",
		["17"] = "Mecha",
		["18"] = "Medical",
		["19"] = "Music",
		["20"] = "Mystery",
		["21"] = "Oneshot",
		["22"] = "Psychological",
		["23"] = "Romance",
		["24"] = "School Life",
		["25"] = "Sci-Fi",
		["26"] = "Seinen",
		["27"] = "Shoujo",
		["28"] = "Shoujo Ai",
		["29"] = "Shounen",
		["30"] = "Shounen Ai",
		["31"] = "Slice of Life",
		["32"] = "Smut",
		["33"] = "Sports",
		["34"] = "Supernatural",
		["35"] = "Tragedy",
		["36"] = "Long Strip",
		["37"] = "Yaoi",
		["38"] = "Yuri",
		["39"] = "[no chapters]",
		["40"] = "Video Games",
		["41"] = "Isekai",
		["42"] = "Adaptation",
		["43"] = "Anthology",
		["44"] = "Web Comic",
		["45"] = "Full Color",
		["46"] = "User Created",
		["47"] = "Official Colored",
		["48"] = "Fan Colored",
		["49"] = "Gore",
		["50"] = "Sexual Violence",
		["51"] = "Crime",
		["52"] = "Magical Girls",
		["53"] = "Philosophical",
		["54"] = "Superhero",
		["55"] = "Thriller",
		["56"] = "Wuxia",
		["57"] = "Aliens",
		["58"] = "Animals",
		["59"] = "Crossdressing",
		["60"] = "Demons",
		["61"] = "Delinquents",
		["62"] = "Genderswap",
		["63"] = "Ghosts",
		["64"] = "Monster Girls",
		["65"] = "Loli",
		["66"] = "Magic",
		["67"] = "Military",
		["68"] = "Monsters",
		["69"] = "Ninja",
		["70"] = "Office Workers",
		["71"] = "Police",
		["72"] = "Post-Apocalyptic",
		["73"] = "Reincarnation",
		["74"] = "Reverse Harem",
		["75"] = "Samurai",
		["76"] = "Shota",
		["77"] = "Survival",
		["78"] = "Time Travel",
		["79"] = "Vampires",
		["80"] = "Traditional Games",
		["81"] = "Virtual Reality",
		["82"] = "Zombies",
		["83"] = "Incest",
		["84"] = "Mafia",
		["85"] = "Villainess"
	}
	if genres[genre] ~= nil then
		return genres[genre]
	else
		return genre
	end
end

function GetDemographic(demograhic)
	local demographics = {
		["1"] = "Shounen",
		["2"] = "Shoujo",
		["3"] = "Seinen",
		["4"] = "Josei"
	}
	if demographics[demograhic] ~= nil then
		return demographics[demograhic]
	else
		return demograhic
	end
end

local Langs = {
	["sa"] = "Arabic",
	["bd"] = "Bengali",
	["bg"] = "Bulgarian",
	["mm"] = "Burmese",
	["ct"] = "Catalan",
	["cn"] = "Chinese (Simp)",
	["hk"] = "Chinese (Trad)",
	["cz"] = "Czech",
	["dk"] = "Danish",
	["nl"] = "Dutch",
	["gb"] = "English",
	["ph"] = "Filipino",
	["fi"] = "Finnish",
	["fr"] = "French",
	["de"] = "German",
	["gr"] = "Greek",
	["hu"] = "Hungarian",
	["id"] = "Indonesian",
	["it"] = "Italian",
	["jp"] = "Japanese",
	["kr"] = "Korean",
	["my"] = "Malay",
	["mn"] = "Mongolian",
	["ir"] = "Persian",
	["pl"] = "Polish",
	["br"] = "Portuguese (Br)",
	["pt"] = "Portuguese (Pt)",
	["ro"] = "Romanian",
	["ru"] = "Russian",
	["rs"] = "Serbo-Croatian",
	["es"] = "Spanish (Es)",
	["mx"] = "Spanish (LATAM)",
	["se"] = "Swedish",
	["th"] = "Thai",
	["tr"] = "Turkish",
	["ua"] = "Ukrainian",
	["vn"] = "Vietnamese"
}

function GetLang(lang)
	if Langs[lang] ~= nil then
		return Langs[lang]
	else
		return 'Unknown'
	end
end

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

function GetPageNumber()
	-- Extract chapter ID which is needed for getting info about the current chapter:
	local cid = URL:match('chapter/(%d+)')

	-- Delay this task if configured:
	Delay()

	-- Fetch JSON from API:
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, API_PATH .. '/chapter/' .. cid .. API_CHAPTER_PARAMS)) then
		local crypto = require 'fmd.crypto'
		local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString():gsub('<', ''):gsub('>', ''):gsub('&quot;', ''))) -- Some characters may not be correctly escaped and create issues for the parser. That's why these will be removed.

		local cinfo    = x.XPath('json(*)')
		local ccode    = x.XPathString('code', cinfo)
		local cstatus  = x.XPathString('status', cinfo)
		local cmessage = x.XPathString('message', cinfo)

		-- Handle the returned JSON if code is 200. Any other code will result in an error that is being shown in the log if enabled.
		-- If no code is returned, the API certainly didn't respond. It's probably not reachable if that happens.
		if ccode == '200' then
			local hash   = x.XPathString('data/hash', cinfo)
			local server = x.XPathString('data/server', cinfo)

			-- If the base url of the image server misses or has a '/' too much, the download won't work. This check prevents that:
			if server:sub(-1) ~= '/' then server = server .. '/' end

			local pages for pages in x.XPath('json(*).data.pages()').Get() do
				TASK.PageLinks.Add(server .. hash .. '/' .. pages.ToString())
			end

		elseif ccode == '' then
			print('Chapter could not be fetched. Please check if you can access the Manga page and read the chapter in your browser.')
			return net_problem

		else
			print(cstatus .. ' (' .. ccode .. '):   ' .. cmessage)
			return net_problem

		end

		return no_error
	else
		return net_problem
	end
end

local dirurl='/titles/2'

function GetDirectoryPageNumber()
	HTTP.Cookies.Values['mangadex_title_mode'] = '2'
	Delay()
	if HTTP.GET(MODULE.RootURL .. dirurl) then
		local x = CreateTXQuery(HTTP.Document)
		PAGENUMBER = tonumber(x.XPathString('(//ul[contains(@class,"pagination")]/li/a)[last()]/@href'):match('/2/(%d+)')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	HTTP.Cookies.Values['mangadex_title_mode'] = '2'
	Delay()
	if HTTP.GET(MODULE.RootURL .. dirurl .. '/' .. (URL + 1) .. '/') then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//a[contains(@class, "manga_title")]', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function Delay()
	local lastDelay = tonumber(MODULE.Storage['lastDelay']) or 1
	local mdx_delay = tonumber(MODULE.GetOption('mdx_delay')) or 2 -- * MODULE.ActiveConnectionCount
	if lastDelay ~= '' then
		lastDelay = os.time() - lastDelay
		if lastDelay < mdx_delay then
			sleep((mdx_delay - lastDelay) * 1000)
		end
	end
	MODULE.Storage['lastDelay'] = os.time()
end

function Login()
	MODULE.ClearCookies()
	MODULE.Account.Status = asChecking
	local login_url=MODULE.RootURL .. '/login'
	if not HTTP.GET(login_url) then
		MODULE.Account.Status = asUnknown
		return false
	end
	local login_post_url = CreateTXQuery(HTTP.Document).XPathString('//form[@id="login_form"]/@action') or ''
	if login_post_url == '' then
		MODULE.Account.Status = asUnknown
		return false
	end
	login_post_url = MODULE.RootURL .. login_post_url:gsub('&nojs=1','')
	HTTP.Reset()

	HTTP.Headers.Values['Origin'] = ' ' .. MODULE.RootURL
	HTTP.Headers.Values['Referer'] = ' ' .. login_url
	HTTP.Headers.Values['Accept'] = ' */*'
	HTTP.Headers.Values['X-Requested-With'] = ' XMLHttpRequest'

	function getFormData(formData)
		local t = tostring(os.time())
		local b = string.rep('-',39-t:len()) .. t
		local crlf = string.char(13) .. string.char(10)
		local r = ''
		for k, v in pairs(formData) do
			r = r .. '--' .. b .. crlf ..
				'Content-Disposition: form-data; name="' .. k .. '"' .. crlf ..
				crlf ..
				v .. crlf
		end
		r=r .. '--' .. b .. '--' .. crlf
		return 'multipart/form-data; boundary=' .. b, r
	end
	local post_data
	HTTP.MimeType,post_data = getFormData({
		login_username = MODULE.Account.Username,
		login_password = MODULE.Account.Password,
		two_factor = '',
		remember_me = '1'})

	HTTP.POST(login_post_url,post_data)
	if HTTP.ResultCode == 200 then
		if HTTP.Cookies.Values['mangadex_rememberme_token'] ~= '' then
			MODULE.Account.Status = asValid
		else
			MODULE.Account.Status = asInvalid
		end
	else
		MODULE.Account.Status = asUnknown
	end
	return true
end

function AccountState()
	local cookies = ''
	if MODULE.Account.Enabled then
		cookies = MODULE.Account.Cookies
		if cookies ~= '' then
			MODULE.AddServerCookies(MODULE.Account.Cookies)
		end
	else
		cookies = MODULE.GetServerCookies('mangadex.org', 'mangadex_rememberme_token')
		if cookies ~= '' then
			MODULE.Account.Cookies = cookies
			MODULE.RemoveCookies('mangadex.org', 'mangadex_rememberme_token')
		end
	end
	return true
end

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd07c9c2425764da8ba056505f57cf40c'
	m.Name                     = 'MangaDex'
	m.RootURL                  = 'https://mangadex.org'
	m.Category                 = 'English'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.MaxTaskLimit             = 1
	m.MaxConnectionLimit       = 2
	m.AccountSupport           = true
	m.OnLogin                  = 'Login'
	m.OnAccountState           = 'AccountState'
	m.AddServerCookies('mangadex.org', 'mangadex_h_toggle=1; max-age=31556952')

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['delay'] = 'Delay (s) between requests',
			['showscangroup'] = 'Show scanlation group',
			['showchaptertitle'] = 'Show chapter title',
			['lang'] = 'Language:'
		},
		['id_ID'] = {
			['delay'] = 'Tunda (detik) antara permintaan',
			['showscangroup'] = 'Tampilkan grup scanlation',
			['showchaptertitle'] = 'Tampilkan judul bab',
			['lang'] = 'Bahasa:'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionSpinEdit('mdx_delay', lang:get('delay'), 2)
	m.AddOptionCheckBox('luashowscangroup', lang:get('showscangroup'), false)
	m.AddOptionCheckBox('luashowchaptertitle', lang:get('showchaptertitle'), true)

	local items = 'All'
	local t = GetLangList()
	for k, v in ipairs(t) do items = items .. '\r\n' .. v; end
	m.AddOptionComboBox('lualang', lang:get('lang'), items, 11)
end
