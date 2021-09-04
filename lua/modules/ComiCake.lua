function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                        = id
		m.Name                      = name
		m.RootURL                   = url
		m.Category                  = 'English-Scanlation'
		m.OnGetNameAndLink          = 'GetNameAndLink'
		m.OnGetInfo                 = 'GetInfo'
		m.OnGetPageNumber           = 'GetPageNumber'
		m.OnGetDirectoryPageNumber  = 'GetDirectoryPageNumber'
	end
	AddWebsiteModule('380e42a9e93e488abcd74cf47b2cf148', 'LetItGoScans', 'https://reader.letitgo.scans.today')
	AddWebsiteModule('02a1221995a54021b084687364cbff50', 'WhimSubs', 'https://whimsubs.xyz')
end

function GetNameAndLink()
	if MODULE.ID == '02a1221995a54021b084687364cbff50' then -- whimsubs
		if HTTP.GET(MODULE.RootURL..'/r/directory/' .. (URL + 1)) then
			CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="mdc-card__media-title"]/a', LINKS, NAMES)
			return no_error
		else
			return net_problem
		end
	else
		if HTTP.GET(MODULE.RootURL..'/directory/' .. (URL + 1)) then
			CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="mdc-card__media-title"]/a', LINKS, NAMES)
			return no_error
		else
			return net_problem
		end
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//h1/text()')
		MANGAINFO.CoverLink = x.XPathString('//main//img/@src')
		MANGAINFO.Artists   = x.XPathStringAll('//main//table//tr[td="Artist:"]/td/a')
		MANGAINFO.Authors   = x.XPathStringAll('//main//table//tr[td="Author:"]/td/a')
		MANGAINFO.Genres    = x.XPathStringAll('//main//table//tr[td="Tags:"]/td/a')
		MANGAINFO.Summary   = x.XPathString('//main//pre')
		
		x.XPathHREFAll('//main//ul/li/span/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL .. '/manifest.json')) then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathStringAll('json(*).readingOrder().href', TASK.PageLinks)
	else
		return false
	end
	return true
end

function GetDirectoryPageNumber()
	if MODULE.ID == '02a1221995a54021b084687364cbff50' then -- whimsubs
		if HTTP.GET(MODULE.RootURL .. '/r/directory/') then
			PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="pagination"]//a[contains(., "last")]/@href'):match('/r/directory/(%d+)/'))
			return no_error
		else
			return net_problem
		end
	else
		if HTTP.GET(MODULE.RootURL .. '/directory/') then
			PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="pagination"]//a[contains(., "last")]/@href'):match('/(%d+)/'))
			return no_error
		else
			return net_problem
		end
	end
end