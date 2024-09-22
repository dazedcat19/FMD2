function Init()
	function AddWebsiteModule(id, name, rooturl, cat)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = rooturl
		m.Category                 = cat
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
		return m
	end

	local cat = 'English'
	AddWebsiteModule('8e7bd7b38aa041aa9bc1bddeec33b6f4', 'MangaHere', 'https://www.mangahere.cc', cat)

	local m = AddWebsiteModule('0d3653a8d9b747a381374f32e0a1641e', 'FanFox', 'https://fanfox.net', cat)
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['timeout'] = 'Timeout in (s)',
			['removewatermark'] = 'Remove watermark',
			['saveaspng'] = 'Save as PNG'
		},
		['id_ID'] = {
			['timeout'] = 'Batas waktu dalam (detik)',
			['removewatermark'] = 'Hapus watermark',
			['saveaspng'] = 'Simpan sebagai PNG'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.OnAfterImageSaved = 'AfterImageSaved'
	m.AddOptionSpinEdit('mf_timeout', lang:get('timeout'), 60)
	m.AddOptionCheckBox('mf_removewatermark', lang:get('removewatermark'), true)
	m.AddOptionCheckBox('mf_saveaspng', lang:get('saveaspng'), false)
	require('fmd.mangafoxwatermark').LoadTemplate(fmd.LuaDirectory .. 'extras\\mangafoxtemplate')
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. '/directory/?az') then
		local x = CreateTXQuery(HTTP.Document)
		PAGENUMBER = tonumber(x.XPathString('//div[@class="pager-list"]//a[last()-1]')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if MODULE.ID == '8e7bd7b38aa041aa9bc1bddeec33b6f4' then	-- mangahere
		if HTTP.GET(MODULE.RootURL .. '/mangalist/') then
			CreateTXQuery(HTTP.Document).XPathHREFAll('//p[@class="browse-new-block-content"]/a', LINKS, NAMES)
			return no_error
		else
			return net_problem
		end
	else
		if HTTP.GET(MODULE.RootURL .. '/directory/' .. (URL + 1) .. '.html?az') then
			CreateTXQuery(HTTP.Document).XPathHREFAll('//ul[contains(@class, "manga-list")]/li/a', LINKS, NAMES)
			return no_error
		else
			return net_problem
		end
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	HTTP.Cookies.Values['isAdult'] = '1'
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//span[@class="detail-info-right-title-font"]')
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="detail-info-cover-img"]/@src'))
		MANGAINFO.Authors   = x.XPathString('//p[@class="detail-info-right-say"]/a')
		MANGAINFO.Genres    = x.XPathStringAll('//p[@class="detail-info-right-tag-list"]/a')
		MANGAINFO.Summary   = x.XPathString('//p[@class="fullcontent"]')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="detail-info-right-title-tip"]'))
		for v in x.XPath('//ul[@class="detail-main-list"]/li/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'):gsub('1%.html$', ''))
			MANGAINFO.ChapterNames.Add(x.XPathString('./div/p[@class="title3"]', v))
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		HTTP.Reset()
		HTTP.Headers.Values['Referer'] = MANGAINFO.URL
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	TASK.PageNumber = 0
	HTTP.Cookies.Values['isAdult'] = '1'
	local aurl = MaybeFillHost(MODULE.RootURL, URL):gsub('1%.html$', '')
	local lurl = aurl .. '1.html'
	if HTTP.GET(lurl) then
		local duktape = require 'fmd.duktape'
		local x = CreateTXQuery(HTTP.Document)
		local key = duktape.ExecJS('var $=function(){return{val:function(){}}},newImgs,guidkey;' .. x.XPathString('//script[contains(., "eval")]') .. ';newImgs||guidkey;')
		if key:len() > 16 then
			TASK.PageLinks.CommaText = key
		else
			local s = x.XPathString('//script[contains(., "chapterid")]')
			local cid = s:match('chapterid%s*=%s*(.-)%s*;') or '0'
			TASK.PageNumber = tonumber(s:match('imagecount%s*=%s*(%d-)%s*;') or '0')
			if TASK.PageNumber == nil then TASK.PageNumber = 1 end
			local p = 1
			local ts = os.time()
			local timeout = tonumber(MODULE.GetOption('mf_timeout'))
			while p <= TASK.PageNumber do
				if os.time() >= ts + timeout then print(string.format("%s second timeout.", timeout)) break end
				HTTP.Reset()
				HTTP.Headers.Values['Pragma'] = 'no-cache'
				HTTP.Headers.Values['Cache-Control'] = 'no-cache'
				HTTP.Headers.Values['Referer'] = lurl
				if HTTP.XHR(aurl .. string.format('chapterfun.ashx?cid=%s&page=%d&key=%s', cid, p, key)) then
					s = HTTP.Document.ToString()
					if s ~= '' then
						s = duktape.ExecJS(s .. ';d;')
						for i in s:gmatch('[^,]+') do
							TASK.PageLinks.Add(i)
						end
					end
				end
				-- sometimes the server will give more images than the actual chapter have
				-- it is an ads or file not found(404 not found)
				-- remove invalid images here
				while TASK.PageLinks.Count > TASK.PageNumber do
					TASK.PageLinks.Delete(TASK.PageLinks.Count-1)
				end
				if TASK.PageLinks.Count >= TASK.PageNumber then break end
				p = TASK.PageLinks.Count + 1
				sleep(2000) -- without minimum delay of 2 seconds server will only give 2 images for each xhr request
			end
		end
		return true
	else
		return false
	end
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	return true
end

function AfterImageSaved()
	if MODULE.GetOption('mf_removewatermark') then
		require('fmd.mangafoxwatermark').RemoveWatermark(FILENAME, MODULE.GetOption('mf_saveaspng'))
	end
	return true
end
