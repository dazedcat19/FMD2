----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '7103ae6839ea46ec80cdfc2c4b37c803'
	m.Name                     = 'Asura Scans'
	m.RootURL                  = 'https://asuracomic.net'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['force_high'] = 'Force high quality chapter images'
		},
		['id_ID'] = {
			['force_high'] = 'Paksa gambar bab kualitas tinggi'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('force_high', lang.force_high, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/series?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPath('//div[contains(@class, "md:grid-cols-5")]/a').Count == 0 then return no_error end
	for v in x.XPath('//div[contains(@class, "md:grid-cols-5")]/a').Get() do
		LINKS.Add(v.GetAttribute('href'):gsub('-(%w+)$', '-'))
		NAMES.Add(x.XPathString('div/div/div[2]/span[1]', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//span[@class="text-xl font-bold"]')
	MANGAINFO.CoverLink = x.XPathString('(//img[@alt="poster"])[1]/@src')
	MANGAINFO.Authors   = x.XPathString('//h3[contains(., "Author")]/following-sibling::h3')
	MANGAINFO.Artists   = x.XPathString('//h3[contains(., "Artist")]/following-sibling::h3')
	MANGAINFO.Genres    = x.XPathStringAll('//h3[contains(., "Genres")]/following-sibling::div/button')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//h3[contains(., "Status")]/following-sibling::h3'))
	MANGAINFO.Summary   = x.XPathString('//span[@class="font-medium text-sm text-[#A2A2A2]"]')

	for v in x.XPath('//div[contains(@class, "group")]/a').Get() do
		-- Check if the chapter has a circle icon (an <svg> element inside a <span> within the <h3>)
		local hasCircleIcon = x.XPath(string.format(
			'//a[@href="%s"]//h3[contains(@class, "text-sm text-white font-medium")]/span/svg',
			v.GetAttribute('href')
		)).Count > 0
		-- Only add chapters without a circle icon
		if not hasCircleIcon then
			-- Add the chapter link to MANGAINFO.ChapterLinks
			MANGAINFO.ChapterLinks.Add('series/' .. v.GetAttribute('href'):gsub('-(%w+)/chapter', '-/chapter'))
			-- Extract and add the chapter name to MANGAINFO.ChapterNames
			MANGAINFO.ChapterNames.Add(x.XPathString('string-join(h3[contains(@class, "text-sm text-white font-medium")]//text(), " ")', v))
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local s = HTTP.Document.ToString():gsub('\\"', '"')
	local x = CreateTXQuery(s)
	local json = x.XPathString('(//script[contains(., "published_at")])[last()]/substring-before(substring-after(., """chapter"":"), "}],[")')

	if MODULE.GetOption('force_high') then
		local key = URL
		local format_to_try = MODULE.Storage[key]

		local replacement_format
		if format_to_try == 'webp' then
			replacement_format = '/%1.webp'
			MODULE.Storage[key] = nil
		else
			replacement_format = '/%1.jpg'
			MODULE.Storage[key] = 'webp'
		end

		json = json:gsub('/conversions/([%w%-]+)-optimized%.webp', replacement_format)
	end

	x.ParseHTML(json)
	x.XPathStringAll('json(*).pages().url', TASK.PageLinks)

	return true
end
