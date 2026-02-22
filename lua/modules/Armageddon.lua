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
	local crypto = require 'fmd.crypto'
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local _, base64 = x.XPathString('//script[contains(., "const _0x1b8fbd")]'):match('const%s+_0x1b8fbd=([\'"])(.-)%1')
	if base64 then
		x.ParseHTML(crypto.DecodeBase64(base64))
		x.XPathStringAll('json(*)()', TASK.PageLinks)
	else
		local s = x.XPathString('//script[contains(., "ts_reader")]')
		if s ~= '' then
			x.ParseHTML(s:match('run%((.-)%);'):gsub('!0', 'true'):gsub('!1', 'false'))
		end
		x.XPathStringAll('json(*).sources()[1].images()', TASK.PageLinks)
	end
	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = crypto.DecodeURL(TASK.PageLinks[i])
	end

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end