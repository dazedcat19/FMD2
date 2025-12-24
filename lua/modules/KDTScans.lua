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
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

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
		x = CreateTXQuery(HTTP.Document)
		local secure_js = x.XPathString('//script[contains(., "const _0x1b8fbd")]')
		local base64_str = GetBetween('const _0x1b8fbd="', '",', secure_js)
		if secure_js == base64_str then
			base64_str = GetBetween("const _0x1b8fbd='", "',", secure_js)
		end
		local urls_links = require "utils.json".decode(require 'fmd.crypto'.DecodeBase64(base64_str))
		for i = 0, #urls_links -1 do
			TASK.PageLinks.Add(urls_links[i])
		end
	end
	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end