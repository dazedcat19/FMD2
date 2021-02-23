json = require "utils.json"
local dirurls = {
	'/series',
	'/series/finished',
	'/series/oneshot'
}
local chapterListQuery = '/api/viewer/readable_products?aggregate_id=%s&number_since=%d&number_until=-1&read_more_num=50&type=episode'
local lastchapter = 0

--
function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.CoverLink = x.XPathString('//div[@class="series-header-image-wrapper"]/img/@data-src')
		MANGAINFO.Title     = x.XPathString('//h1[@class="series-header-title"]/text()')
		MANGAINFO.Authors   = x.XPathString('//h2[@class="series-header-author"]')
	MANGAINFO.Summary   = x.XPathString('//*[@class="series-header-description"]')
	lastChapter = x.XPathCount('//ul[contains(@class, "series-episode-list")]/li') + 1

	local s = x.XPathString('//div[@class="js-readable-product-list"]/@data-latest-list-endpoint')
	local aggregateId = ''
	if s ~= nil then
		lastChapter = lastChapter + tonumber(s:match('number_since=(%d+)&'))
		aggregateId = s:match('aggregate_id=(%d+)&')
	end
	
	if lastChapter < 50 then lastChapter = 50 end

	while lastChapter > 1 do 
		local url = MaybeFillHost(MODULE.RootURL, string.format(chapterListQuery, aggregateId, lastChapter))
		if not HTTP.GET(url) then break end
		x.ParseHTML(json.decode(HTTP.Document.ToString()).html)
		for v in x.XPath('//li').Get() do
		  local isChapterPrivate = v.GetAttribute('class'):find("private")

		  local addChapter = true
		  if MODULE.GetOption('luashowprivate') == false and isChapterPrivate ~= nil then addChapter = false end

		  if addChapter == true then
			local chapterLink = x.XPathString('a/@href', v)
			if chapterLink ~= nil then
			  MANGAINFO.ChapterLinks.Add(chapterLink)
			  MANGAINFO.ChapterNames.Add(x.XPathString('a/div/h4/text()', v))
			else
			  MANGAINFO.ChapterLinks.Add(url)
			  MANGAINFO.ChapterNames.Add(x.XPathString('div/h4/text()', v))
			end
		  end
		end
		lastChapter = lastChapter - 50
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	local currPage = diruls[MODULE.CurrentDirectoryIndex + 1]
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, currPage)) then
		local x, v = CreateTXQuery(HTTP.Document)
		for v in x.XPath('//div[@class="series-items"]/ul/li/a').Get() do
			LINKS.Add(v.GetAttribute('href'))
			NAMES.Add(x.XPathString('h4', v))
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
	local x = CreateTXQuery(HTTP.Document)
	local episodesJson = x.XPathString('//script[@id="episode-json"]/@data-value')
	local episodes = json.decode(episodesJson).readableProduct.pageStructure.pages

	for _, page in ipairs(episodes) do
	  if page.src ~= nil then
		TASK.PageLinks.Add(page.src)
	  end
	end
	return no_error
  end
  return net_problem
end

-- Get the page count for the current chapter.
function DownloadImage()
  if HTTP.GET(URL) then
	local fragmentSize = 4
	local puzzle = require('fmd.imagepuzzle').Create(fragmentSize, fragmentSize)
	puzzle.Multiply = 8
	for i=0, (fragmentSize*fragmentSize)-1 do
	  puzzle.Matrix[i] = (i%fragmentSize) * fragmentSize + (i // fragmentSize)
	end
	puzzle.DeScramble(HTTP.Document, HTTP.Document)
	return true
  end
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '88fb782a9c234fde82b0de32f710ce3f'
	m.Name                     = 'TonarinoYoungJump'
	m.RootURL                  = 'https://tonarinoyj.jp'
	m.Category                 = 'Raw'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
	m.TotalDirectory           = #dirurls

  local fmd = require 'fmd.env'
		local slang = fmd.SelectedLanguage
		local lang = {
			['en'] = {
				['showprivate'] = 'Show private chapters'
			},
			get =
				function(self, key)
					local sel = self[slang]
					if sel == nil then sel = self['en'] end
					return sel[key]
				end
		}
		m.AddOptionCheckBox('luashowprivate', lang:get('showprivate'), false)
end
