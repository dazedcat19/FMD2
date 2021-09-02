local API_URL = 'https://api.mangadex.org' -- This is the url to the JSON API. Call this url to look at the API documentation.
local API_PARAMS = '?includes[]=author&includes[]=artist&includes[]=cover_art'
local COVER_URL = 'https://uploads.mangadex.org/covers'
local GUID_PATTERN = '(%x%x%x%x%x%x%x%x%-%x%x%x%x%-%x%x%x%x%-%x%x%x%x%-%x%x%x%x%x%x%x%x%x%x%x%x)'
local MAPPING_FILE = 'userdata/mangadex_v5_mapping.txt'

function Init()
	local m = NewWebsiteModule()
	m.ID                 = 'd07c9c2425764da8ba056505f57cf40c'
	m.Name               = 'MangaDex'
	m.RootURL            = 'https://mangadex.org'
	m.Category           = 'English'
	m.OnGetNameAndLink   = 'GetNameAndLink'
	m.OnGetInfo          = 'GetInfo'
	m.OnGetPageNumber    = 'GetPageNumber'
	m.MaxTaskLimit       = 1
	m.MaxonnectionLimit  = 2

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['delay'] = 'Delay (s) between requests',
			['showscangroup'] = 'Show scanlation group',
			['showchaptertitle'] = 'Show chapter title',
			['lang'] = 'Language:',
			['datasaver'] = 'Data saver'
		},
		['id_ID'] = {
			['delay'] = 'Tunda (d) antara permintaan',
			['showscangroup'] = 'Tampilkan grup scanlation',
			['showchaptertitle'] = 'Tampilkan judul bab',
			['lang'] = 'Bahasa:',
			['datasaver'] = 'Penghemat data'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionSpinEdit('mdx_delay', lang:get('delay'), 1)
	m.AddOptionCheckBox('luashowscangroup', lang:get('showscangroup'), false)
	m.AddOptionCheckBox('luashowchaptertitle', lang:get('showchaptertitle'), true)
	m.AddOptionCheckBox('luadatasaver', lang:get('datasaver'), false)

	local items = 'All'
	local t = GetLangList()
	for k, v in ipairs(t) do items = items .. '\r\n' .. v; end
	m.AddOptionComboBox('lualang', lang:get('lang'), items, 11)
end

function GetNameAndLink()
	local crypto = require 'fmd.crypto'

	local demographics = {
		[1] = 'shounen',
		[2] = 'shoujo',
		[3] = 'josei',
		[4] = 'seinen',
		[5] = 'none'
	}
	local mangastatus = {
		[1] = 'ongoing',
		[2] = 'completed',
		[3] = 'hiatus',
		[4] = 'cancelled'
	}
	local contentrating = {
		[1] = 'safe',
		[2] = 'suggestive',
		[3] = 'erotica',
		[4] = 'pornographic'
	}
	
	-- Delay this task if configured:
	Delay()

	for _, dg in ipairs(demographics) do
		for _, ms in ipairs(mangastatus) do
			for _, cr in ipairs(contentrating) do
				local total = 1
				local offset = 0
				local offmaxlimit = 0
				local order = 'asc'

				while total > offset do
					if total > 10000 and offset >= 10000 and order == 'asc' then
						offset = 0
						order = 'desc'
					end

					if offset < 10000 and HTTP.GET(API_URL .. '/manga?limit=100&offset=' .. offset ..'&order[createdAt]=' .. order ..'&publicationDemographic[]=' .. dg .. '&status[]=' .. ms .. '&contentRating[]=' .. cr) then
						UPDATELIST.UpdateStatusText('Loading page ' .. dg .. ', ' .. ms .. ', ' .. cr .. ' (' .. order .. ')' or '')
						local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))

						local ninfo    = x.XPath('json(*)')
						local nstatus  = x.XPathString('result', ninfo)
						local nmessage = x.XPathString('errors/detail', ninfo)

						if nstatus == 'error' then
							print(nstatus .. ': ' .. nmessage)
						else
							if order == 'asc' then
								total = tonumber(x.XPathString('json(*).total'))
							else
								total = tonumber(x.XPathString('json(*).total')) - 10000
							end
							offset = offset + tonumber(x.XPathString('json(*).limit'))
							if total > 10000 then
								offmaxlimit = total - 10000
								print('Total Over Max Limit: ' .. offmaxlimit .. ' are over the max limit!')
							end

							local results for results in x.XPath('json(*).results()').Get() do
								LINKS.Add('title/' .. x.XPathString('data/id', results))
								NAMES.Add(x.XPathString('data/attributes/title/en', results))
							end
						end
					elseif offset >= 10000 then
						print('Offset for fetching manga list is over the max limit: ' .. offset .. ' (Total: ' .. total .. ')')
					else
						print('List could not be fetched. Please check if you can access the Manga page and read the chapter in your browser.')
						return net_problem
					end
				end
			end
		end
	end
	return no_error
end

function GetInfo()
	local crypto = require 'fmd.crypto'
	local MAPPING = {}

	-- Extract manga GUID which is needed for getting info and chapter list:
	local mid = URL:match('title/' .. GUID_PATTERN) -- When pasting a link from browser.

	-- Delay this task if configured:
	Delay()

	-- If no GUID (v5) was found, check if old ID (v3) is present:
	if mid == nil then
		local newid
		mid = URL:match('%a%a%a%a%a/(%d+)')
		if mid ~= nil then -- When input is old ID (v3) check in the local mapping table if the new GUID (v5) already has been mapped.
			-- Read local mapping file for old (v3) to new (v5) manga IDs and add them to the global table.
			-- Sadly this can't be loaded globally and therefore needs to be read again everytime:
			local mfile = io.open(MAPPING_FILE, 'r')
			if mfile then
				for line in io.lines(MAPPING_FILE) do
					for old, new in line:gmatch('(%d+);' .. GUID_PATTERN) do
						MAPPING[old] = new
					end
				end
				mfile:close()
				newid = MAPPING[mid]
			end
			if newid ~= nil then -- When mapping has been found, use the GUID (v5) for the rest of this function.
				mid = newid
			-- print('MangaDex: Legacy ID Mapping found in local mapping file: ' .. mid)
			else
				-- If the GUID (v5) has not been mapped yet, get it from the legacy endpoint of the API:
				HTTP.MimeType = 'application/json'
				if HTTP.POST(API_URL .. '/legacy/mapping', '{"type":"manga", "ids":[' .. mid .. ']}') then
					newid = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPathString('json(*)()[1].data.attributes.newId')
					print('MangaDex: Legacy ID Mapping has been executed. The new ID is: ' .. newid)
					if newid ~= nil then -- If a valid GUID (v5) has been returned, map it to the global mapping table and save it to the mapping file.
						MAPPING[mid] = newid
						local mfile = io.open(MAPPING_FILE, 'a')
						io.output(mfile)
						io.write(mid .. ';' .. newid .. '\n')
						mfile:close()
						mid = newid
					else
						print('ERROR: ' .. MANGAINFO.Title)
						print('ID: ' .. mid)
						return net_problem
					end
				else
					print('ERROR: ' .. MANGAINFO.Title)
					print('ID: ' .. mid)
					return net_problem
				end
			end
		else
			print('ERROR: ' .. MANGAINFO.Title)
			print('ID: ' .. mid)
			return net_problem
		end
	end

	-- Fetch JSON from API:
	if HTTP.GET(API_URL .. '/manga/' .. mid .. API_PARAMS) then

		local x	= CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
		local minfo    = x.XPath('json(*)')
		local mstatus  = x.XPathString('result', minfo)
		local mmessage = x.XPathString('errors/detail', minfo)

		if mstatus == 'ok' then
			MANGAINFO.Title     = x.XPathString('data/attributes/title/en', minfo)
			MANGAINFO.Summary   = x.XPathString('data/attributes/description/en', minfo)
			MANGAINFO.Authors   = x.XPathStringAll('json(*).relationships()[type="author"].attributes.name')
			MANGAINFO.Artists   = x.XPathStringAll('json(*).relationships()[type="artist"].attributes.name')
			MANGAINFO.CoverLink = COVER_URL .. '/' .. mid .. '/' .. x.XPathString('json(*).relationships()[type="cover_art"].attributes.fileName')

			-- Fetch genre, demographic, and rating:
			MANGAINFO.Genres    = x.XPathStringAll('json(*).data.attributes.tags().attributes.name.en')
			local demographic = GetDemographic(x.XPathString('json(*).data.attributes.publicationDemographic'))
			if demographic ~= 'null' then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. demographic end
			local rating = GetRating(x.XPathString('json(*).data.attributes.contentRating'))
			if rating ~= 'null' then MANGAINFO.Genres = MANGAINFO.Genres .. ', ' .. rating end

			-- Set status to 'completed' if it's not 'ongoing' or 'hiatus'. The status 'cancelled' will also be set to 'completed':
			local status = x.XPathString('data/attributes/status', minfo)
			if (status == 'ongoing') or (status == 'hiatus') then
				status = 'ongoing'
			else
				status = 'completed'
			end
			MANGAINFO.Status = MangaInfoStatusIfPos(status, 'ongoing', 'completed')

			-- Get user defined options for fetching all chapters respecting these options:
			local optgroup   = MODULE.GetOption('luashowscangroup')
			local opttitle   = MODULE.GetOption('luashowchaptertitle')
			local optlangid  = FindLanguage(MODULE.GetOption('lualang'))
			local limitparam = 50
			local langparam

			-- If all languages are selected, the remove the filter parameter:
			if optlangid == nil then langparam = '' else langparam = '&translatedLanguage[]=' .. optlangid end
			
			-- Workaround: Because there is a bug in the JSON navigation of internet tools, it's not possible to get a sub sequence (in this case "relationships").
			-- Because of that the complete JSON needs to be queried for each chapter to get the scanlation group names. This is extremely heavy on local performance.
			-- To work around that performance issue this check sets the limit per request to 50 instead of the maximum of 500 because it works much faster even with more requests.
			-- If optgroup then limitparam = 50 else limitparam = 500 end

			local total  = 1
			local offset = 0

			-- The API has a limit of 500 chapters per request.
			-- The first request provides the overall total of the query, which will be used to check if there are still some chapters left to fetch.
			while total > offset do
			if HTTP.GET(API_URL .. '/manga/' .. mid .. '/feed' .. '?limit=' .. limitparam .. '&offset=' .. offset .. langparam .. '&contentRating[]=safe&contentRating[]=suggestive&contentRating[]=erotica&contentRating[]=pornographic&includes[]=scanlation_group&order[chapter]=asc') then
				local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
				local chapters = x.XPath('json(*).results()')
				total = tonumber(x.XPathString('json(*).total'))
				offset = offset + tonumber(x.XPathString('json(*).limit'))
				for ic = 1, chapters.Count do
					local ignore = false

					-- Check if the group that released the chapter is on the built-in ignore list, otherwise skip the chapter:
					local groupids = x.XPath('json(*).results()[' .. ic .. '].relationships()[type="scanlation_group"]')
					local groups = {}
					if ignore == false then
						for gid = 1, groupids.Count do
							local groupid = x.XPathString('id', groupids.Get(gid))
							if ignore == false then -- Chapter should be ignored if at least one group is on the ignore list. This check prevents that the ignore value is being set back to false if a second group is not in the list.
								ignore = IgnoreChaptersByGroupId(groupid)
							end
							groups[gid] = x.XPathString('attributes/name', groupids.Get(gid))
						end
					end

					if ignore == false then
						local volume     = x.XPathString('data/attributes/volume', chapters.Get(ic))
						local chapter    = x.XPathString('data/attributes/chapter', chapters.Get(ic))
						local title      = x.XPathString('data/attributes/title', chapters.Get(ic))
						local language   = x.XPathString('data/attributes/translatedLanguage', chapters.Get(ic))
						local scanlators = ' [' .. table.concat(groups, ", ") .. ']'

						-- Remove title if user option is disabled:
						if opttitle == false then title = '' end

						-- Add prefix to title if it's not empty:
						if title ~= '' and title ~= nil and title ~= 'null' then title = ' - ' .. title else title = '' end

						-- Format volume and chapter strings if not empty:
						volume = volume ~= 'null' and string.format('Vol. %s ', volume) or ''
						chapter = chapter ~= 'null' and string.format('Ch. %s', chapter) or ''

						-- Make unnumbered chapter as oneshot
						if volume == '' and chapter == '' then chapter = 'Oneshot' end

						-- Append language id if user option is set to "All":
						if optlangid == nil then language = string.format(' [%s]', language) else language = '' end

						-- Set a fixed value if chapter doesn't have any group assigned or remove group names if option is disabled:
						if optgroup then
							if groupids.Count == 0 then scanlators = ' [no group]' end
						else
							scanlators = ''
						end

						-- Add chapter name and link to the manga info list:
						MANGAINFO.ChapterLinks.Add(x.XPathString('data/id', chapters.Get(ic)))
						MANGAINFO.ChapterNames.Add(volume .. chapter .. title .. language .. scanlators)
					end
				end
			end
		end

		elseif mstatus == 'error' then
			MANGAINFO.Summary = mstatus .. ': ' .. mmessage
			print(mstatus .. ': ' .. mmessage)
			return no_error
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	local crypto = require 'fmd.crypto'

	-- Delay this task if configured:
	Delay()

	-- Fetch JSON from API:
	if HTTP.GET(API_URL .. '/chapter' .. URL) then
		local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))

		local cinfo    = x.XPath('json(*)')
		local cstatus  = x.XPathString('result', cinfo)
		local cmessage = x.XPathString('errors/detail', cinfo)

		if cstatus == 'ok' then
			-- Get the base server url of MangaDex@Home (MDH):
			if HTTP.GET(API_URL .. '/at-home/server' .. URL) then
				local server = CreateTXQuery(HTTP.Document).XPathString('json(*).baseUrl')
				local hash   = x.XPathString('data/attributes/hash', cinfo)

				if MODULE.GetOption('luadatasaver') then -- Use data saver
					local pages for pages in x.XPath('json(*).data.attributes.dataSaver()').Get() do
						TASK.PageLinks.Add(server .. '/data-saver/' .. hash .. '/' .. pages.ToString())
					end
				else
					local pages for pages in x.XPath('json(*).data.attributes.data()').Get() do
						TASK.PageLinks.Add(server .. '/data/' .. hash .. '/' .. pages.ToString())
					end
				end
			end

		elseif cstatus == 'error' then
			print(cstatus .. ': ' .. cmessage)
			return net_problem
		else
			print('Chapter could not be fetched. Please check if you can access the Manga page and read the chapter in your browser.')
			return net_problem
		end
		return no_error
	else
		return net_problem
	end
end

function IgnoreChaptersByGroupId(id)
	local groups = {
		["4f1de6a2-f0c5-4ac5-bce5-02c7dbb67deb"] = "MangaPlus",
		["8d8ecf83-8d42-4f8c-add8-60963f9f28d9"] = "Comikey"
	}

	if groups[id] ~= nil then
		return true
	else
		return false
	end
end

function GetDemographic(demographic)
	local demographics = {
		["shounen"] = "Shounen",
		["shoujo"] = "Shoujo",
		["seinen"] = "Seinen",
		["josei"] = "Josei"
	}
	if demographics[demographic] ~= nil then
		return demographics[demographic]
	else
		return demographic
	end
end

function GetRating(rating)
	local ratings = {
		["safe"] = "Safe",
		["suggestive"] = "Suggestive",
		["erotica"] = "Erotica",
		["pornographic"] = "Pornographic"
	}
	if ratings[rating] ~= nil then
		return ratings[rating]
	else
		return rating
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
	["tl"] = "Filipino",
	["fi"] = "Finnish",
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
	["NULL"] = "Other",
	["fa"] = "Persian",
	["pl"] = "Polish",
	["pt-br"] = "Portuguese (Br)",
	["pt"] = "Portuguese (Pt)",
	["ro"] = "Romanian",
	["ru"] = "Russian",
	["sh"] = "Serbo-Croatian",
	["es"] = "Spanish (Es)",
	["es-la"] = "Spanish (LATAM)",
	["sv"] = "Swedish",
	["th"] = "Thai",
	["tr"] = "Turkish",
	["uk"] = "Ukrainian",
	["vi"] = "Vietnamese"
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

function Delay()
	local lastDelay = tonumber(MODULE.Storage['lastDelay']) or 1
	local mdx_delay = tonumber(MODULE.GetOption('mdx_delay')) or 1 -- * MODULE.ActiveConnectionCount
	if lastDelay ~= '' then
		lastDelay = os.time() - lastDelay
		if lastDelay < mdx_delay then
			sleep((mdx_delay - lastDelay) * 1000)
		end
	end
	MODULE.Storage['lastDelay'] = os.time()
end
