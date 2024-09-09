----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'edb6b5cdbd8e42eab1f0c6b2f4b28758'
	m.Name                     = 'Kanzenin'
	m.RootURL                  = 'https://kanzenin.info'
	m.Category                 = 'H-Sites'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.TotalDirectory           = AlphaList:len()
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
AlphaList = '##ABCDEFGHIJKLMNOPQRSTUVWXYZ'
DirectoryPagination = '/a-z-list/page/'
-- XPathTokenAuthors   = 'Author'
-- XPathTokenArtists   = 'Artist'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local i, s, x = nil
	if MODULE.CurrentDirectoryIndex == 0 then
		s = '.'
	elseif MODULE.CurrentDirectoryIndex == 1 then
		s = '0-9'
	else
		i = MODULE.CurrentDirectoryIndex + 1
		s = AlphaList:sub(i, i)
	end
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. '/?show=' .. s

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFTitleAll('//div[@class="bsx"]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//div[@class="pagination"]/a[last()-1]')) or 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end