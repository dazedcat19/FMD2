----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '8cd912c557ff4e5ba6b5473ba30fc7b7'
	m.Name                     = 'Ritharscans'
	m.RootURL                  = 'https://ritharscans.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.KeyoApp'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.CoverLink = x.XPathString('//div[@class="flex lg:block"]//@style'):match('background%-image:url%((.-)%)')
	MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//a[@title="Status"]/span'))

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local body, i, images, path, v = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	body   = HTTP.Document.ToString():gsub('&quot;', '"')
	images = body:match('pages: (%[.-%]),')
	path   = body:match("baseLink: '(.-)',")
	if images then
		images = require 'utils.json'.decode(images)
		for i, v in ipairs(images) do
			TASK.PageLinks.Add(path .. v.path)
		end
	end

	return no_error
end