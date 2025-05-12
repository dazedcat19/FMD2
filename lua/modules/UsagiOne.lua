----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fe152f2481a84befbfa01d7f4312edb9'
	m.Name                     = 'UsagiOne'
	m.RootURL                  = 'https://web.usagi.one'
	m.Category                 = 'Russian'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.GroupLe'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	Template.GetDirectoryPageNumber()

	return no_error
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
	local i, i1, i2, json, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not URL:find('mtr=1') then u = u .. '?mtr=1' end

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	json = GetBetween('[[', ', false, ', Trim(GetBetween('rm_h.readerInit(', 'false);', x.XPathString('//script[@type="text/javascript" and contains(., "rm_h.readerInit")]'))))
	json = json:gsub('%],%[', ';'):gsub('\'', ''):gsub('"', ''):gsub(']]', ';')
	for i in json:gmatch('(.-);') do
		i1, i2 = i:match('(.-),.-,(.-),.-,.-')
		TASK.PageLinks.Add(i1 .. i2:match("^(.-)?"))
	end

	return true
end