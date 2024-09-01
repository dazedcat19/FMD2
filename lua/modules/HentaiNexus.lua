----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5eda5ccf87f1488f9dfa7a9a18f2bcf1'
	m.Name                     = 'HentaiNexus'
	m.RootURL                  = 'https://hentainexus.com'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

Hostname = 'hentainexus.com'     -- Replace with actual hostname for retrieval
DirectoryPagination = '/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//ul[@class="pagination-list"])[1]/li[last()]/a/text()'))

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//div[@class="container"]/div/div/a[contains(@href, "/view/")]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('div/header/@title', v))
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local magazine, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.CoverLink = x.XPathString('(//figure[@class="image"]/img/@src)[1]')
	MANGAINFO.Authors   = x.XPathString('//table[@class="view-page-details"]//a[contains(@href, "=publisher:")]/text()')
	MANGAINFO.Artists   = x.XPathString('//table[@class="view-page-details"]//a[contains(@href, "=artist:")]/text()')
	MANGAINFO.Title     = x.XPathString('//h1[@class="title"]/text()')
	MANGAINFO.Genres    = x.XPathStringAll('//table[@class="view-page-details"]//a[contains(@href, "=tag:")]/substring-before(., "(")')
	MANGAINFO.Summary   = x.XPathString('//table[@class="view-page-details"]//td[contains(., "Description")]/following-sibling::td/text()')

	magazine = x.XPathString('//table[@class="view-page-details"]//a[contains(@href, "=magazine:")]/text()')
	if magazine ~= '' then
		MANGAINFO.Title = MANGAINFO.Title .. ' (' .. magazine .. ')'
	else
		MANGAINFO.Title = MANGAINFO.Title .. ' [' .. MANGAINFO.Authors .. ']'
	end
	if MANGAINFO.Artists ~= '' then MANGAINFO.Title = '[' .. MANGAINFO.Artists .. '] ' .. MANGAINFO.Title end

	MANGAINFO.ChapterLinks.Add(URL)
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local x = nil
	local u = string.gsub(URL, "view", "read") -- fetch 1st image to get encrypted message

	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, u)) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(DecryptMessage(GetBetween('initReader("', '", ', x.XPathString('//script[contains(., "initReader")]'))))
	x.XPathStringAll('json(*)().image', TASK.PageLinks)

	return no_error
end

-- Helper functions
local function charCodeAt(str, index)
	return string.byte(str, index)
end

local function fromCharCode(...)
	return string.char(...)
end

-- Custom XOR function
local function bxor(a, b)
	local r = 0
	for i = 0, 31 do
		local x = a / 2 + b / 2
		if x ~= math.floor(x) then
			r = r + 2^i
		end
		a = math.floor(a / 2)
		b = math.floor(b / 2)
	end
	return r
end

-- Unpack function that works across different Lua versions
local unpack = table.unpack or unpack

-- Decrypt text
function DecryptMessage(str)
	local crypto = require 'fmd.crypto'
	message = crypto.DecodeBase64(str)
	local decodedChars = {message:byte(1, #message)}
	
	local host = Hostname  
	local hostnameChars = {host:byte(1, #host)}
	local limit = math.min(#hostnameChars, 64)
	
	for i = 1, limit do
		decodedChars[i] = charCodeAt(fromCharCode(bxor(decodedChars[i], hostnameChars[i])), 1)
	end
	
	message = string.char(unpack(decodedChars))

	prime_array = {2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53}
	local prime_picker = 0
	for i = 1, 64 do
		prime_picker = prime_picker ~ string.byte(message, i)
		for j = 1, 8 do
			-- no need for zero shift bit correction since message byte will always be positive
			if(prime_picker & 1 == 1) then
				prime_picker = prime_picker >> 1 ~ 12
			else
				prime_picker = prime_picker >> 1
			end
		end
	end
	prime_picker = prime_picker & 7
	array = {}
	o2 = 0
	o3 = 0
	o5 = 0
	
	for i = 0, 255 do array[i] = i end
	for i = 0, 255 do 
			o2 = (o2 + array[i] + string.byte(message, (i % 64)+1)) % 256
			o4 = array[i]
			array[i] = array[o2]
			array[o2] = o4
	end
	
	prime = prime_array[prime_picker+1]
	o1 = 0
	o2 = 0
	parsed = ''
	i = 0
	while i + 64 < string.len(message) do
		o1 = (o1 + prime) % 256
		o2 = (o3 + array[(o2 + array[o1]) % 256]) % 256
		o3 = (o3 + o1 + array[o1]) % 256
		o4 = array[o1]
		array[o1] = array[o2]
		array[o2] = o4
		o5 = array[(o2 + array[(o1 + array[(o5 + o3) % 256]) % 256]) % 256]
		parsed = parsed .. string.char(string.byte(message, (i + 64)+1) ~ o5)
		i = i + 1
	end
	return parsed
end
