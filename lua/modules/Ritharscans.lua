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
function stand_status(status)
    status = string.lower(status)
	status_com = {'yes','completed','complete'}
	status_can = {'cancelled'}
	status_hia = {'hiatus'}
	for i=1, #status_com do if status == status_com[i] then return 'Completed' end end
	for i=1, #status_can do if status == status_can[i] then return 'Cancelled' end end
	for i=1, #status_hia do if status == status_hia[i] then return 'Hiatus' end end
	return 'Ongoing'
end

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
	status = x.XPathString('//a[@title="Status"]/span')
	MANGAINFO.Status = MangaInfoStatusIfPos(stand_status(status))

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