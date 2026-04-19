----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '8a1c8f08664b4f0d91bc847fe81a4221'
	m.Name                     = 'ValirScans'
	m.RootURL                  = 'https://valirscans.org'
	m.Category                 = 'English-Scanlation'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showpaidchapters'] = 'Show paid chapters'
		},
		['id_ID'] = {
			['showpaidchapters'] = 'Tampilkan bab berbayar'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('showpaidchapters', lang.showpaidchapters, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/api/series?sort=newest&limit=100&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).meta.totalPages')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	for v in CreateTXQuery(HTTP.Document).XPath('json(*).data()').Get() do
		LINKS.Add('series/comic/' .. v.GetProperty('slug').ToString())
		NAMES.Add(v.GetProperty('title').ToString())
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local s = HTTP.Document.ToString():gsub('\\"', '"'):gsub('\\\\', '\\'):gsub('"%]%)</script><script>self%.__next_f%.push%(%[1,"', '')
	local x = CreateTXQuery(s)
	x.ParseHTML('{"series"' .. x.XPathString('//script[contains(., "originalTitle")]/substring-before(substring-after(., "{""series"""), "]]")'))
	local info = x.XPath('json(*)')
	MANGAINFO.Title     = x.XPathString('series?title', info)
	MANGAINFO.AltTitles = x.XPathString('string-join(series?aliases?*, ", ")', info)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('series?coverImage', info))
	MANGAINFO.Genres    = x.XPathString('string-join((series?genres?*?name, series?tags?*?name, concat(upper-case(substring(series?type, 1, 1)), lower-case(substring(series?type, 2)))), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('series?status', info), 'ONGOING', 'COMPLETED', 'HIATUS', 'DISCONTINUED|DROPPED')
	MANGAINFO.Summary   = x.XPathString('series?description', info)

	local page = 1
	local pages = tonumber(x.XPathString('totalPages', info)) or 1
	local show_paid_chapters = MODULE.GetOption('showpaidchapters')
	while true do
		for v in x.XPath('chapters?*', info).Get() do
			local is_accessible = v.GetProperty('isLocked').ToString() ~= 'true'

			if show_paid_chapters or is_accessible then
				local title = v.GetProperty('title').ToString()
				local number = v.GetProperty('number').ToString()

				if title:find('-', 1, true) then
					title = title
				else
					title = 'Chapter ' .. number
				end

				MANGAINFO.ChapterLinks.Add(MANGAINFO.URL .. '/chapter/' .. number)
				MANGAINFO.ChapterNames.Add(title)
			end
		end
		page = page + 1
		if page > pages then
			break
		end
		if not HTTP.GET(MANGAINFO.URL .. '?page=' .. page) then break end
		x.ParseHTML(HTTP.Document.ToString():gsub('\\"', '"'):gsub('\\\\', '\\'):gsub('"%]%)</script><script>self%.__next_f%.push%(%[1,"', ''))
		x.ParseHTML('{"series"' .. x.XPathString('//script[contains(., "originalTitle")]/substring-before(substring-after(., "{""series"""), "]]")'))
		info = x.XPath('json(*)')
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local s = HTTP.Document.ToString():gsub('\\"', '"'):gsub('\\\\', '\\'):gsub('"%]%)</script><script>self%.__next_f%.push%(%[1,"', '')
	local x = CreateTXQuery(s)
	x.ParseHTML('{"chapter"' .. x.XPathString('//script[contains(., "originalTitle")]/substring-before(substring-after(., "{""chapter"""), "},""")') .. '}}')
	for v in x.XPath('json(*).chapter.pages().imageUrl').Get() do
		TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
	end

	return true
end