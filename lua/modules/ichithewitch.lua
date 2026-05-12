----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()

	local m = NewWebsiteModule()
	m.ID                    = 'd18c9c3525765da8ba056505a58cf41a'
	m.Name                  = 'Ichi the Witch'
	m.RootURL               = 'https://ichithewitch.org'
	m.Category              = 'English-Scanlation'
	m.OnGetInfo             = 'GetInfo'
	m.OnGetPageNumber       = 'GetPageNumber'
	m.MaxTaskLimit          = 2
	m.MaxConnectionLimit    = 4
    return m
end

----------------------------------------------------------------------------------------------------
-- Local Constants (optional)
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Madara'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------


function GetNameAndLink()
  
  LINKS = {'https://ichithewitch.org/rnd'}
  NAMES = {'Ichi the Witch'}
	return no_error
end

function GetInfo()

  if not HTTP.GET('https://ichithewitch.org') then
    return net_problem
  end
  local x = CreateTXQuery(HTTP.Document)
  MANGAINFO.Title     = "Ichi the Witch"
  MANGAINFO.AltTitles = "Madan no Ichi"
  MANGAINFO.CoverLink = x.XPathString('//figure//img/@src')
  MANGAINFO.Authors   = "Osamu Nishi, Shiro Usazaki"
  MANGAINFO.Genres    = "Action, Adventure, Comedy, Fantasy"
  MANGAINFO.Status    = "Ongoing"
  MANGAINFO.Summary   = x.XPathString('//div[contains(@class, "wp-block-column")]/p')
  
  x.XPathHREFAll('//li[contains(@class, "su-post")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
  MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse() 

  return no_error
end

function GetPageNumber()

  local u = MaybeFillHost(MODULE.RootURL, URL)
  if not HTTP.GET(u) then
    return false
  end    
  local x = CreateTXQuery(HTTP.Document)
  x.XPathStringAll('//div[@class="wp-block-image"]//figure//img/@src', TASK.PageLinks)
  return true
end