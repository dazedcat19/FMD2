----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '94caa4da3f334418b27bc1ba7e127b57'
	m.Name                     = 'Armageddon'
	m.RootURL                  = 'https://www.silentquill.net'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.OnCheckSite			   = 'CheckSite'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function CheckSite()
    MANGACHECK.MangaURL     = "https://www.silentquill.net/the-rise-of-the-errand-boy-who-was-kicked-out-of-the-guild-worlds-strongest-with-ultra-versatile-living-skills/"
    MANGACHECK.MangaTitle   = 'The Rise of the Errand Boy Who was Kicked Out of the Guild: World’s Strongest with Ultra-Versatile Living Skills'
    MANGACHECK.ChapterURL   = "https://www.silentquill.net/the-rise-of-the-errand-boy-who-was-kicked-out-of-the-guild-worlds-strongest-with-ultra-versatile-living-skills-ch-1/"
    MANGACHECK.ChapterTitle = 'Chapter 1'
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	Template.GetInfo()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	if TASK.PageLinks.Count == 0 then --Secure Reader
		local crypto = require 'fmd.crypto'
		local x = CreateTXQuery(HTTP.Document)
		local secure_js = x.XPathString('//script[contains(., "const _0x1b8fbd")]')
		local base64 = GetBetween('const _0x1b8fbd="', '",', secure_js)
		if secure_js == base64 then
			base64 = GetBetween("const _0x1b8fbd='", "',", secure_js)
		end
		local s = crypto.DecodeBase64(base64)
		x.ParseHTML(s)
		x.XPathStringAll('json(*)()', TASK.PageLinks)
		for i = 0, TASK.PageLinks.Count - 1 do
			TASK.PageLinks[i] = crypto.DecodeURL(TASK.PageLinks[i])
		end 
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end