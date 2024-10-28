function Init()
	local m = NewWebsiteModule()
	m.ID                         = '2889e1036e104c9081e0be59180b8354'
	m.Name                       = 'TeamX'
	m.RootURL                    = 'https://olympustaff.com'
	m.Category                   = 'Arabic-Scanlation'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. '/series') then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/series?page=' .. (URL + 1)) then
		CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="bsx"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathString('//h1')
		MANGAINFO.CoverLink  = x.XPathString('//div[@class="text-right"]/img/@src')
		MANGAINFO.Authors    = x.XPathString('//div[./small="الرسام:"]//a')
		MANGAINFO.Genres     = x.XPathStringAll('//div[@class="review-author-info"]/a')
		local status = x.XPathString('//div[./small="الحالة:"]//a')
		if (status == 'مستمرة') or (status == 'قادم قريبًا') then
			status = 'ongoing'
		else
			status = 'completed'
		end
		MANGAINFO.Status    = MangaInfoStatusIfPos(status)
		MANGAINFO.Summary   = x.XPathString('//div[@class="review-content"]/p')

		local p = 1
		local pages = tonumber(x.XPathString('//ul[@class="pagination"]/li[last()-1]/a'))
		if pages == nil then pages = 1 end
		while true do
			local v for v in x.XPath('//div[@class="eplister eplisterfull"]//li/a').Get() do
				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(x.XPathString('div[@class="epl-title"]', v))
			end
			p = p + 1
			if p > pages then
				break
			elseif HTTP.GET(MaybeFillHost(MODULE.RootURL, URL .. '?page=' .. tostring(p))) then
				x.ParseHTML(HTTP.Document)
			else
				break
			end
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="image_list"]//img/@src', TASK.PageLinks)
		return true
	else
		return false
	end
end
