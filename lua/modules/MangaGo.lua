----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '77c3c3cd38444bd7bb4e5f63f2c5c93c'
	m.Name                     = 'MangaGo'
	m.RootURL                  = 'https://www.mangago.me'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnDownloadImage          = 'DownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/genre/All/%s/?sortby=create_date'
local duktape = require('fmd.duktape')

----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

-- Decode a sojson.v4 obfuscated script.
local function SoJsonV4Deobfuscator(obfuscated_js)
	if not obfuscated_js:find("['sojson.v4']", 1, true) then return obfuscated_js end

	local s = obfuscated_js:sub(241, -60)
	local result = ''
	for char_code in s:gmatch('(%d+)') do
		result = result .. string.char(tonumber(char_code))
	end

	return result
end

-- Unscramble the decrypted image list string.
local function UnscrambleImageList(image_list_str, deobfuscated_js)
	local key_locations = {}
	for loc in deobfuscated_js:gmatch('str%.charAt%s*%(%s*(%d+)%s*%)') do
		table.insert(key_locations, tonumber(loc) + 1)
	end

	local unique_locations = {}
	local seen = {}
	for _, v in ipairs(key_locations) do
		if not seen[v] then
			table.insert(unique_locations, v)
			seen[v] = true
		end
	end

	if #unique_locations == 0 then return image_list_str end

	local unscramble_key = {}
	for _, loc in ipairs(unique_locations) do
		local digit = tonumber(image_list_str:sub(loc, loc))
		if digit == nil then
			return image_list_str
		end
		table.insert(unscramble_key, digit)
	end

	local imgListTable = {}
	for i = 1, #image_list_str do
		table.insert(imgListTable, image_list_str:sub(i, i))
	end

	local locations_to_remove = {}
	for _, loc in ipairs(unique_locations) do
		locations_to_remove[loc] = true
	end

	local cleanedImgListTable = {}
	for i = 1, #imgListTable do
		if not locations_to_remove[i] then
			table.insert(cleanedImgListTable, imgListTable[i])
		end
	end

	local cleanedImgList = table.concat(cleanedImgListTable)
	
	return StringUnscramble(cleanedImgList, unscramble_key)
end

-- Reorder a scrambled string based on an embedded key.
local function StringUnscramble(scrambled_str, keys)
	local s_table = {}
	for i = 1, #scrambled_str do
		table.insert(s_table, scrambled_str:sub(i, i))
	end

	for j = #keys, 1, -1 do
		local key_val = keys[j]
		for i = #s_table, key_val + 1, -1 do
			if (i - 1) % 2 ~= 0 then
				local idx1 = i - key_val
				local idx2 = i
				local temp = s_table[idx1]
				s_table[idx1] = s_table[idx2]
				s_table[idx2] = temp
			end
		end
	end

	return table.concat(s_table)
end

-- Generate the dynamic image descrambling key.
local function GetDescramblingKey(deobfuscated_js, image_url)
	local start_marker = 'var renImg = function(img,width,height,id){'
	local end_marker = 'key = key.split('

	local start_pos = deobfuscated_js:find(start_marker, 1, true)
	if not start_pos then return nil end

	local body_start = start_pos + #start_marker
	local body_end = deobfuscated_js:find(end_marker, body_start, true)
	if not body_end then return nil end

	local js_body_raw = deobfuscated_js:sub(body_start, body_end - 1)

	local js_filters = {'jQuery', 'document', 'getContext', 'toDataURL', 'getImageData', 'width', 'height'}
	local js_body_cleaned_lines = {}

	for line in js_body_raw:gmatch('[^\r\n]+') do
		local is_valid = true
		for _, filter in ipairs(js_filters) do
			if line:find(filter, 1, true) then
				is_valid = false
				break
			end
		end
		if is_valid then
			table.insert(js_body_cleaned_lines, line)
		end
	end

	local js_body = table.concat(js_body_cleaned_lines, '\n')
	js_body = js_body:gsub('img.src', 'url')

	local js_helpers = [[
		function replacePos(strObj, pos, replacetext) {
			var str = strObj.substr(0, pos) + replacetext + strObj.substring(pos + 1, strObj.length);
			return str;
		}
	]]

	local js_to_run = js_helpers ..
					'function getDescramblingKey(url) { ' .. js_body .. '; return key; }\n' ..
					'getDescramblingKey(\'' .. image_url .. '\');'

	local result = duktape.ExecJS(js_to_run)

	return result
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination:format(1)

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="pagination"]//li[last()-1]//span')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination:format(URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//span[@class="title"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//td[./label="Alternative:"]/normalize-space(text())')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="left cover"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//td[./label="Author:"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//td[./label="Genre(s):"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//td[./label="Status:"]/span'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="manga_summary"]/string-join(text(), "\r\n")')

	x.XPathHREFAll('//table[@id="chapter_table"]//a[not(@style)]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end
	
	local x = CreateTXQuery(HTTP.Document)
	local imgsrcs_script = x.XPathString('//script[contains(., "imgsrcs")]')
	local imgsrc_b64 = imgsrcs_script:match('var imgsrcs%s*=%s*\'([^\']+)\'')
	local chapter_js_url = x.XPathString('//script[contains(@src, "chapter.js")]/@src')

	if not HTTP.GET(chapter_js_url) then return false end

	local deobfuscated_js = SoJsonV4Deobfuscator(HTTP.Document.ToString())
	local key = deobfuscated_js:match('var key%s*=%s*CryptoJS%.enc%.Hex%.parse%("([0-9a-fA-F]+)"%)')
	local iv = deobfuscated_js:match('var iv%s*=%s*CryptoJS%.enc%.Hex%.parse%("([0-9a-fA-F]+)"%)')
	local decryption_script = string.format([[
		var CryptoJS = require('utils/crypto-js.min.js');

		function decryptData(b64_data, hex_key, hex_iv) {
			var key = CryptoJS.enc.Hex.parse(hex_key);
			var iv = CryptoJS.enc.Hex.parse(hex_iv);

			var decrypted = CryptoJS.AES.decrypt(
				b64_data,
				key,
				{
					iv: iv,
					mode: CryptoJS.mode.CBC,
					padding: CryptoJS.pad.ZeroPadding
				}
			);
			
			return decrypted.toString(CryptoJS.enc.Utf8);
		}

		decryptData('%s', '%s', '%s');
	]], imgsrc_b64, key, iv)

	local decrypted_image_list = duktape.ExecJS(decryption_script)
	local final_image_list = UnscrambleImageList(decrypted_image_list, deobfuscated_js)
	local cols = deobfuscated_js:match('var%s*widthnum%s*=%s*heightnum%s*=%s*(%d+);') or '0'

	for image_url in final_image_list:gmatch('([^,]+)') do
		local final_url = image_url
		if image_url:find('cspiclink', 1, true) then
			local descrambling_key = GetDescramblingKey(deobfuscated_js, image_url)
			final_url = image_url .. '#desckey=' .. descrambling_key .. '&cols=' .. cols
		end
		TASK.PageLinks.Add(final_url)
	end

	return true
end

-- Download and decrypt and/or descramble image given the image URL.
function DownloadImage()
	if not HTTP.GET(URL) then return false end

	local fragment = URL:match('[^#]+(#.+)')

	if fragment and fragment:find('desckey=', 1, true) then
		local desckey = fragment:match('desckey=([^&]+)')
		local cols = tonumber(fragment:match('cols=([^&]+)'))

		local puzzle = require('fmd.imagepuzzle').Create(cols, cols)
		local key_array = {}
		for val in desckey:gmatch('([^a]+)') do
			table.insert(key_array, tonumber(val) or 0)
		end

		for i = 1, #key_array do
			puzzle.Matrix[i - 1] = key_array[i]
		end

		puzzle.DeScramble(HTTP.Document, HTTP.Document)
	end

	return true
end