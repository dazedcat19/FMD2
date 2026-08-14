function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title=x.XPathString('//h2[contains(@class, "works-intro-title")]')
		MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL,x.XPathString('//div[contains(@class,"works-cover")]/a/img/@src'))
		MANGAINFO.Authors=x.XPathString('//p[@class="works-intro-digi"]/span[contains(., "作者")]/em/substring-before(., " ")')
		MANGAINFO.Summary=x.XPathString('//p[contains(@class,"works-intro-short")]')
		x.XPathHREFAll('//div[@id="chapter"]//ol[contains(@class, "chapter-page-all")]/li//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		local duktape = require 'fmd.duktape'
		local x=CreateTXQuery(HTTP.Document)
		local s = x.XPathString('//script[contains(., "window") and contains(., "eval")]')
		local nonce = duktape.ExecJS('var window={};'..s..';window.nonce;');
		s = x.XPathString('//script[contains(., "var DATA")]')
		local data = duktape.ExecJS(s..';DATA;');
		local script = x.XPathString('//script[contains(@src, "chapter")]/@src')
		if HTTP.GET(script) then
			s = HTTP.Document.ToString()
			s = '!function(){eval (function(p, a, c, k, e, r)'..GetBetween('eval (function(p, a, c, k, e, r)', '}();', s)..'}();'
			s = 'var W={nonce:"'..nonce..'",DATA:"'..data..'"};'..s..';JSON.stringify(_v);'
			s = duktape.ExecJS(s)
			x.ParseHTML(s)
			x.XPathStringAll('json(*).picture().url', TASK.PageLinks)
			return true
		else
			return false
		end
	else
		return false
	end
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	return true
end

function getdirectorypagenumber()
	if HTTP.GET(MODULE.RootURL .. '/Comic/all/search/hot/page/1') then
		x = CreateTXQuery(HTTP.Document)
		PAGENUMBER = tonumber(x.XPathString('//span[contains(@class,"ret-result-num")]/em')) or 1
	if PAGENUMBER > 1 then
		PAGENUMBER = math.ceil(PAGENUMBER / 12)
	end
		return no_error
	else
		return net_problem
	end
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL..'/Comic/all/search/hot/page/'..(URL + 1)) then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//ul[contains(@class, "ret-search-list")]/li//h3/a',LINKS,NAMES)
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'f48e28dc51bb4abd813337e3ab08a849'
	m.Category                 = 'Raw'
	m.Name                     = 'AcQQCom'
	m.RootURL                  = 'https://ac.qq.com'
	m.OnGetInfo                = 'getinfo'
	m.OnGetPageNumber          = 'getpagenumber'
	m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
	m.OnGetNameAndLink         = 'getnameandlink'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
end
