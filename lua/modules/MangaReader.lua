----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'e9f4e7fe53bc4ff6921db6d67c9b9fb2'
	m.Name                     = 'MangaReader'
	m.RootURL                  = 'https://mangareader.to'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.TotalDirectory           = AlphaList:len()

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['lang'] = 'Language:',
			['imagequality'] = 'Image quality:',
			['quality'] = 'High\nMedium\nLow',
			['listtype'] = 'List type:',
			['type'] = 'Chapter\nVolume'
		},
		['id_ID'] = {
			['lang'] = 'Bahasa:',
			['imagequality'] = 'Kualitas gambar:',
			['quality'] = 'Tinggi\nSedang\nRendah',
			['listtype'] = 'Tipe daftar:',
			['type'] = 'Bab\nJilid'
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
	for k, v in ipairs(t) do items = items .. '\r\n' .. v end
	m.AddOptionComboBox('lang', lang:get('lang'), items, 2)
	m.AddOptionComboBox('imagequality', lang:get('imagequality'), lang:get('quality'), 0)
	m.AddOptionComboBox('listtype', lang:get('listtype'), lang:get('type'), 0)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

AlphaList = '##ABCDEFGHIJKLMNOPQRSTUVWXYZ'
local DirectoryPagination = '/az-list/'
local Langs = {
	["zh"] = "Chinese (Simp)",
	["en"] = "English",
	["fr"] = "French",
	["ja"] = "Japanese",
	["ko"] = "Korean",
	["pt-br"] = "Portuguese (Br)",
	["es-mx"] = "Spanish (LATAM)"
}

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Return a sorted list of all available language names.
function GetLangList()
	local t = {}
	for k, v in pairs(Langs) do table.insert(t, v) end
	table.sort(t)
	return t
end

-- Find and return the language key for a given language name or index.
local function FindLanguage(lang)
	local t = GetLangList()
	for i, v in ipairs(t) do
		if i == lang then
			lang = v
			break
		end
	end
	for k, v in pairs(Langs) do
		if v == lang then return k end
	end
	return nil
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
    local s
    if MODULE.CurrentDirectoryIndex == 0 then
        s = 'other'
    elseif MODULE.CurrentDirectoryIndex == 1 then
        s = '0-9'
    else
        local i = MODULE.CurrentDirectoryIndex + 1
        s = AlphaList:sub(i, i)
    end
    local u = MODULE.RootURL .. DirectoryPagination .. s .. '?page=' .. (URL + 1)

    if not HTTP.GET(u) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)
    x.XPathHREFTitleAll('//div[@class="mls-wrap"]//h3/a', LINKS, NAMES)
    UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//ul[contains(@class, "pagination")]/li[last()]/a/@href'):match('(%d+)$')) or 1

    return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h2[@class="manga-name"]')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="manga-name-or"]')
	MANGAINFO.CoverLink = x.XPathString('(//div[@class="manga-poster"])[1]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="anisc-info"]/div[./span="Authors:"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres"]/a') .. ', ' .. x.XPathString('//div[@class="anisc-info"]/div[./span="Type:"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathStringAll('//div[@class="anisc-info"]/div[./span="Status:"]/span[@class="name"]'), 'Publishing', 'Finished', 'On Hiatus', 'Discontinued')
	MANGAINFO.Summary   = x.XPathString('//div[@class="description"]')

	local sel_listtype = (MODULE.GetOption('listtype') or 0) + 1
	local listtype     = ({'chap', 'vol'})[sel_listtype]
	local optlang      = MODULE.GetOption('lang')
	local optlangid    = FindLanguage(optlang)

	if not HTTP.GET(MODULE.RootURL .. '/ajax/manga/reading-list/' .. URL:match('-(%d+)$') .. '?readingBy=' .. listtype) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(require 'utils.json'.decode(HTTP.Document.ToString()).html)
	local listtype = ({'chapters', 'volumes'})[sel_listtype]
	local xpath = optlangid and ('//ul[@id="' .. optlangid .. '-' .. listtype .. '"]/li') or ('//ul[contains(@id, "' .. listtype .. '")]/li')
	for v in x.XPath(xpath).Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('data-id'))
		MANGAINFO.ChapterNames.Add(x.XPathString('.//span[@class="name"]', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local listtype = ({'chap', 'vol'})[(MODULE.GetOption('listtype') or 0) + 1]
	local quality  = ({'high', 'medium', 'low'})[(MODULE.GetOption('imagequality') or 0) + 1]
	local u = MODULE.RootURL .. '/ajax/image/list/' .. listtype .. URL .. '?quality=' .. quality

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(require 'utils.json'.decode(HTTP.Document.ToString()).html)
	x.XPathStringAll('//div[@data-url]/@data-url', TASK.PageLinks)

	return true
end