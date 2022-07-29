local ALPHA_LIST_UP = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'

function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		if MANGAINFO.Title == '' then
			MANGAINFO.Title = x.XPathString('//h1')
			MANGAINFO.Title = string.gsub(MANGAINFO.Title, '^Manga ', '')
			MANGAINFO.Title = string.gsub(MANGAINFO.Title, '^Manhua ', '')
			MANGAINFO.Title = string.gsub(MANGAINFO.Title, '^Manhwa ', '')
			MANGAINFO.Title = string.gsub(MANGAINFO.Title, ' VF$', '')
		end
		MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@id="main"]//img/@src'))
		MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathStringAll('//div[@id="main"]//p[contains(span, "Statut")]/text()', ''), 'En Cours', 'Termine')
		MANGAINFO.Authors=x.XPathStringAll('//div[@id="main"]//p[contains(span, "Auteur")]/text()', '')
		MANGAINFO.Artists=x.XPathStringAll('//div[@id="main"]//p[contains(span, "Artiste")]/text()', '')
		MANGAINFO.Genres=x.XPathStringAll('//div[@id="main"]//p[contains(span, "Type(s)")]/text()', '')
		MANGAINFO.Summary=x.XPathString('//div[@id="main"]//div[contains(text(), "Synopsis")]/following-sibling::*')
		x.XPathHREFAll('css("div#chapters_list div.chapters_list > a")', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x=CreateTXQuery(HTTP.Document)
		TASK.PageNumber = x.XPathCount('//select[@id="pages"]/option/@value')
	else
		return false
	end
	return true
end

function getimageurl()
	local s = URL:gsub('/+$', '') .. '/' ..(WORKID+1)..'.html'
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,s)) then
		TASK.PageLinks[WORKID]=CreateTXQuery(HTTP.Document).XPathString('//div[@id="image"]/@data-src')
		return true
	end
	return false
end

function getnameandlink()
	local s = '0-9'
	if MODULE.CurrentDirectoryIndex ~= 0 then
		s = ALPHA_LIST_UP:sub(MODULE.CurrentDirectoryIndex+1,MODULE.CurrentDirectoryIndex+1)
	end
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, '/mangas/' .. s .. '/' .. (URL + 1))) then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//div[@id="main"]//p/a[contains(@href, "/manga/")]', LINKS, NAMES)
		UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//ul[contains(@class, "pagination")]/li[last()]/a')) or 1
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID               = '7859a25e6e1940cebf719a521646a948'
	m.Category         = 'French'
	m.Name             = 'Japscan'
	m.RootURL          = 'http://www.japscan.me'
	m.LastUpdated      = 'April 6, 2018'
	m.OnGetInfo        = 'getinfo'
	m.OnGetPageNumber  = 'getpagenumber'
	m.OnGetNameAndLink = 'getnameandlink'
	m.OnGetImageURL    = 'getimageurl'
	m.TotalDirectory   = ALPHA_LIST_UP:len()
end
