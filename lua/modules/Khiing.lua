----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'aef7472c9d6f4842963d6f4f04b7e744'
	m.Name                     = 'Khiing'
	m.RootURL                  = 'https://khiing.com'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()


	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()

	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title 	= x.XPathStringAll('//div/h1/text()[normalize-space()]')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//main/div/div/div/img/@src'))
	MANGAINFO.Summary   = x.XPathString('//main//p/text()')

    local json = require "utils.json"   
	local script_blob = x.XPathString('//script[contains(text(), "chapters") and contains(text(), "__next_f.push") and contains(text(), "coinPrice")]')
	
	local json_text = script_blob:match("self.__next_f.push%(%s*(%[.*%])%s*%)")
    print ('script_blob' .. script_blob)
    print ('json_text' .. json_text)
	
	local pages = json.decode(json_text)
	local chapters_json = GetBetween('"chapters":', ',"seriesId"', pages[2])
    print(chapters_json)
	x.ParseHTML(chapters_json)
		
	-- Cargar capítulos en tabla temporal
	local chapter_list = {}
	local v
	for v in x.XPath('json(*)()').Get() do
		local isfree = x.XPathString('isFreeNow', v)
		if isfree == "true" then
			table.insert(chapter_list, {
				num = tonumber(x.XPathString('number', v)),   -- número como integer
				slug = x.XPathString('number', v)             -- string
			})
		end
	end

	-- Ordenar del capítulo 1 → n
	table.sort(chapter_list, function(a, b)
		return a.num < b.num
	end)

	-- Agregar al MANGAINFO ya ordenado
	for _, ch in ipairs(chapter_list) do
		MANGAINFO.ChapterLinks.Add(u .. '/chapter/' .. ch.slug)
		MANGAINFO.ChapterNames.Add('Capítulo ' .. ch.slug)
	end
	
	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
    local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
    local json = require "utils.json"
	local script_blob = x.XPathString('//script[contains(text(), "pages") and contains(text(), "__next_f.push")]')
	
	local json_text = script_blob:match("self.__next_f.push%(%s*(%[.*%])%s*%)")
	local pages = json.decode(json_text)
	local chapters_json = GetBetween('"pages":', ',"series"', pages[2])
	x.ParseHTML(chapters_json)	
	
	local v for v in x.XPath('json(*)()').Get() do 
		TASK.PageLinks.Add(x.XPathString('image_url', v))
	end	
	
	return no_error
end
