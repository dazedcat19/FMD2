----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//meta[@property="og:title"]/@content'):gsub(' | %w+$', '')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="mlabel" and contains(., "Status")]/following-sibling::a[1]'))
	MANGAINFO.Authors   = x.XPathString('//span[@class="mlabel" and contains(., "Author")]/following-sibling::a[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//span[@class="mlabel" and contains(., "Genre")]/following-sibling::a')
	MANGAINFO.Summary   = x.XPathString('//span[@class="mlabel" and contains(., "Description")]/following-sibling::div[1]')

	local chapter_uri = x.XPathString('//a[@ng-repeat]/@href'):gsub('{{.+$', '')
	local body = HTTP.Document.ToString()

	-- direct translation from "function MainFunction($http){..."
	local json = require "utils.json"
	local vm = {}
	vm.Chapters = json.decode(body:match('vm%.Chapters%s*=%s*(%[.-%]);'))
	vm.PageOne = body:match('vm%.PageOne%s*=%s*"(.-)"')
	vm.ChapterDisplay = function(e)
		local t = tonumber(e:sub(2, -2))
		local n = e:sub(-1)
		if '0' ~= n then t = t .. "." .. n end
		return t
	end
	vm.ChapterURLEncode = function(e)
		local Index = ""
		local t = e:sub(1, 1)
		if '1' ~= t then Index = "-index-" .. t end
		local n = tonumber(e:sub(2, -2))
		local m = ""
		local a = e:sub(-1)
		if '0' ~= a then m = "." .. a end
		return "-chapter-" .. n .. m .. Index .. vm.PageOne .. ".html"
	end
	local chapter, name; for _, chapter in ipairs(vm.Chapters) do
		MANGAINFO.ChapterLinks.Add(chapter_uri .. vm.ChapterURLEncode(chapter.Chapter))
		if (chapter.Type == nil) or (chapter.Type == "") then chapter.Type = "Chapter" end
		name = chapter.Type .. " " .. vm.ChapterDisplay(chapter.Chapter)
		if chapter.ChapterName and (chapter.ChapterName ~= "") then name = name .. " " .. chapter.ChapterName end
		MANGAINFO.ChapterNames.Add(name)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/directory/'

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	json = GetBetween('vm.FullDirectory = ', '}]};', x.XPathString('//script[contains(., "vm.FullDirectory = ")]')) .. '}]}'
	json = json:gsub('\\"', ''):gsub('\\u2019', '\''):gsub('&#', '')
	x.ParseHTML(json)
	local v for v in x.XPath('json(*).Directory()').Get() do
		NAMES.Add(x.XPathString('s', v))
		LINKS.Add(MODULE.RootURL .. '/manga/' .. x.XPathString('i', v))
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end
	
	local json = require "utils.json"
	local body = HTTP.Document.ToString()
	local vm = {}
	vm.CurChapter = json.decode(body:match('vm.CurChapter = ({.-});'))
	vm.IndexName = '/manga/' .. body:match('vm.IndexName = "(.-)"')
	vm.CurPathName = body:gsub("[\r\n\t]", ""):match('%;%S+ = "(%S+)"%;vm.CHAPTERS = %[')
	vm.ChapterImage = function(ChapterString)
		local Chapter = ChapterString:sub(2, -2)
		local Odd = tonumber(ChapterString:sub(-1))
		if Odd == 0 then
			return Chapter
		else
			return Chapter .. "." .. Odd
		end
	end
	vm.PageImage = function(PageString)
		local s = "000" .. PageString
		return s:sub(-3)
	end
	vm.CurURI = vm.CurPathName .. vm.IndexName .. '/'
	if vm.CurChapter.Directory ~= '' then vm.CurURI = vm.CurURI .. vm.CurChapter.Directory .. '/' end
	vm.CurURI = vm.CurURI .. vm.ChapterImage(vm.CurChapter.Chapter) .. '-'

	local Page; for Page = 1, vm.CurChapter.Page do
		TASK.PageLinks.Add(vm.CurURI .. vm.PageImage(Page) .. '.png')
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                    = id
		m.Name                  = name
		m.RootURL               = url
		m.Category              = 'English'
		m.OnGetInfo             = 'GetInfo'
		m.OnGetNameAndLink      = 'GetNameAndLink'
		m.OnGetPageNumber       = 'GetPageNumber'
	end

	AddWebsiteModule('4c1e8bcc433d4ebca8e6b4d86ce100cf', 'MangaLife', 'https://manga4life.com')
	AddWebsiteModule('3db42782cfc441e3a3498afa91f70a80', 'MangaSee', 'https://mangasee123.com')
end
