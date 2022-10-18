--base64 decoder public domain from https://github.com/iskolbin/lbase64
local base64 = {}
local extract = _G.bit32 and _G.bit32.extract -- Lua 5.2/Lua 5.3 in compatibility mode
if not extract then
	if _G.bit then -- LuaJIT
		local shl, shr, band = _G.bit.lshift, _G.bit.rshift, _G.bit.band
		extract = function( v, from, width )
			return band( shr( v, from ), shl( 1, width ) - 1 )
		end
	elseif _G._VERSION == "Lua 5.1" then
		extract = function( v, from, width )
			local w = 0
			local flag = 2^from
			for i = 0, width-1 do
				local flag2 = flag + flag
				if v % flag2 >= flag then
					w = w + 2^i
				end
				flag = flag2
			end
			return w
		end
	else -- Lua 5.3+
		extract = load[[return function( v, from, width )
			return ( v >> from ) & ((1 << width) - 1)
		end]]()
	end
end
function base64.makeencoder( s62, s63, spad )
	local encoder = {}
	for b64code, char in pairs{[0]='A','B','C','D','E','F','G','H','I','J',
		'K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y',
		'Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n',
		'o','p','q','r','s','t','u','v','w','x','y','z','0','1','2',
		'3','4','5','6','7','8','9',s62 or '+',s63 or'/',spad or'='} do
		encoder[b64code] = char:byte()
	end
	return encoder
end
function base64.makedecoder( s62, s63, spad )
	local decoder = {}
	for b64code, charcode in pairs( base64.makeencoder( s62, s63, spad )) do
		decoder[charcode] = b64code
	end
	return decoder
end
local DEFAULT_DECODER = base64.makedecoder()
local char, concat = string.char, table.concat
function base64.decode( b64, decoder, usecaching )
	decoder = decoder or DEFAULT_DECODER
	local pattern = '[^%w%+%/%=]'
	if decoder then
		local s62, s63
		for charcode, b64code in pairs( decoder ) do
			if b64code == 62 then s62 = charcode
			elseif b64code == 63 then s63 = charcode
			end
		end
		pattern = ('[^%%w%%%s%%%s%%=]'):format( char(s62), char(s63) )
	end
	b64 = b64:gsub( pattern, '' )
	local cache = usecaching and {}
	local t, k = {}, 1
	local n = #b64
	local padding = b64:sub(-2) == '==' and 2 or b64:sub(-1) == '=' and 1 or 0
	for i = 1, padding > 0 and n-4 or n, 4 do
		local a, b, c, d = b64:byte( i, i+3 )
		local s
		if usecaching then
			local v0 = a*0x1000000 + b*0x10000 + c*0x100 + d
			s = cache[v0]
			if not s then
				local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40 + decoder[d]
				s = char( extract(v,16,8), extract(v,8,8), extract(v,0,8))
				cache[v0] = s
			end
		else
			local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40 + decoder[d]
			s = char( extract(v,16,8), extract(v,8,8), extract(v,0,8))
		end
		t[k] = s
		k = k + 1
	end
	if padding == 1 then
		local a, b, c = b64:byte( n-3, n-1 )
		local v = decoder[a]*0x40000 + decoder[b]*0x1000 + decoder[c]*0x40
		t[k] = char( extract(v,16,8), extract(v,8,8))
	elseif padding == 2 then
		local a, b = b64:byte( n-3, n-2 )
		local v = decoder[a]*0x40000 + decoder[b]*0x1000
		t[k] = char( extract(v,16,8))
	end
	return concat( t )
end



function Init()
	local m = NewWebsiteModule()
	m.ID                         = '1a7b98800a114a3da5f48de91f45a880'
	m.Name                       = 'ReadComicOnline'
	m.RootURL                    = 'https://readcomiconline.li'
	m.Category                   = 'English'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.SortedList                 = true

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['datasaver'] = 'Data saver'
		},
		['id_ID'] = {
			['datasaver'] = 'Penghemat data'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionCheckBox('datasaver', lang:get('datasaver'), false)
end

function GetDirectoryPageNumber()
	local url = MODULE.RootURL .. '/ComicList/Newest'
	if HTTP.GET(url) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pager"]/li[last()]/a/@href'):match('=(%d+)$')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	local url = MODULE.RootURL .. '/ComicList/Newest'
	if URL ~= '0' then
		url = url .. '?page=' .. (URL + 1)
	end
	if HTTP.GET(url) then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//table[@class="listing"]/tbody/tr/td[1]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//a[@class="bigChar"]')
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@id="rightside"]//img/@src'))
		MANGAINFO.Authors   = x.XPathStringAll('//div[@class="barContent"]//span[starts-with(., "Author") or starts-with(., "Writer")]/parent::*/a')
		MANGAINFO.Artists   = x.XPathStringAll('//div[@class="barContent"]//span[starts-with(., "Artist")]/parent::*/a')
		MANGAINFO.Summary   = x.XPathString('//div[@class="barContent"]/div/p[starts-with(.,"Summary:")]//following-sibling::p[1]')
		MANGAINFO.Genres    = x.XPathStringAll('//div[@class="barContent"]//span[starts-with(., "Genre")]/parent::*/a')
		MANGAINFO.Status    = MangaInfoStatusIfPos((x.XPathString('//div[@class="barContent"]/div/p[starts-with(.,"Status:")]')))
		x.XPathHREFAll('//table[@class="listing"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL .. '&quality=hq')
	if MODULE.GetOption('datasaver') then
		u = MaybeFillHost(MODULE.RootURL, URL .. '&quality=lq')
	end
	sleep(3000)
	if HTTP.GET(u) then
		local body = HTTP.Document.ToString()
		local s = body:match('var%s+lstImages%s+.-;(.-)%s+var%s')
		local i; for i in s:gmatch("%('(.-)'%)") do
			--new code based on https://readcomiconline.li/Scripts/rguard.min.js?v=1.2.4
			wv = i:gsub("_x236", "d")
			wv = wv:gsub("_x945", "g")
			--if wv.indexOf("https") ~= 0 then
			if (string.find(wv, "https", nil, true) or 0) - 1 ~= 0 then
			    local m = wv
			    --local containsS = m.indexOf("=s0?") > 0
			    local containsS = (string.find(m, "=s0?", nil, true) or 0) - 1 > 0
			    --local x = m.substring(m.indexOf("?"))
			    --index of ?
			    local xq = string.find(m,"?",nil,true)
			    local x = string.sub(m, xq, -1)

			    if containsS then
			        --m = m.substring(0, m.indexOf("=s0?"))
			        --m0q index of =s0?
			        m0q = string.find(m, "=s0?",nil,true) - 1
			        m = string.sub(m, 1, m0q)
			    else
			        --m = m.substring(0, m.indexOf("=s1600?"))
			        m0q = string.find(m, "=s1600?",nil,true) - 1
			        m = string.sub(m, 1, m0q)
			    end
			    --error(m)
			    --m = m.substring(4, 22) + m.substring(25)
			    m = string.sub(m, 5,22) .. string.sub(m, 26)
			    --m = m.substring(0, m.length - 6) + m[m.length - 2] + m[m.length - 1]
			    m = string.sub(m, 1, -7) .. string.sub(m, -2, -2) .. string.sub(m, -1, -1)
			    --m = string.sub(m, 1, -7) .. "=="
			    --m = decodeURIComponent(escape(atob(m)))
			    m = base64.decode(m)

			    --m = m.substring(0, 13) + m.substring(17)
			    m = string.sub(m, 1, 13) .. string.sub(m, 18)
			    if containsS then
			        --m = m.substring(0, m.length - 2) + "=s0"
			        m = string.sub(m, 1, -3) .. "=s0"
			    else
			        --m = m.substring(0, m.length - 2) + "=s1600"
			        m = string.sub(m, 1, -3) .. "=s1600"
			    end
			    m = m .. x
			    wv = "https://2.bp.blogspot.com/" .. m
			end
			TASK.PageLinks.Add(wv)
		end
		return true
	else
		return false
	end
end
