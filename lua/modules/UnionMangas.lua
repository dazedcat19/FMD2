local dirurl = '/mangas'

function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		if MANGAINFO.Title == '' then
			MANGAINFO.Title = x.XPathString('//title/substring-before(.," - Union Mangás")')
		end
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="img-thumbnail"]/@src'))
		MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="table"]/div[@class="row"]/div[6]'), 'En Cours', 'Termine')
		MANGAINFO.Authors=x.XPathString('//h4[starts-with(./label,"Autor")]/substring-after(.,":")')
		MANGAINFO.Artists=x.XPathString('//h4[starts-with(./label,"Artista")]/substring-after(.,":")')
		MANGAINFO.Genres=x.XPathString('//h4[starts-with(./label,"Gênero")]/substring-after(.,":")')
		MANGAINFO.Summary=x.XPathString('//*[@class="panel-body"]')
		x.XPathHREFAll('//*[contains(@class,"capitulos")]/div[1]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
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
		x.XPathStringAll('//img[contains(@class, "img-manga") and contains(@src, "/leitor/")]/@src', TASK.PageLinks)
	else
		return false
	end
	return true
end

function getnameandlink()
	local s = MODULE.RootURL .. dirurl
	s = s .. '/a-z/' .. (URL + 1) .. '/*'
	if HTTP.GET(s) then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//div[contains(@class,"bloco-manga")]/a[2]', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function getdirectorypagenumber()
	if HTTP.GET(MODULE.RootURL .. dirurl) then
		local x = CreateTXQuery(HTTP.Document)
		PAGENUMBER = tonumber(x.XPathString('//ul[@class="pagination"]/li[last()]/a/substring-before(substring-after(@href,"a-z/"),"/")')) or 1
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m=NewWebsiteModule()
	m.ID                       = '6fffdd27e49c47f9958ea98f2fdca0b1'
	m.Category                 = 'Portuguese'
	m.Name                     = 'UnionMangas'
	m.RootURL                  = 'https://unionmangas.top'
	m.OnGetInfo                = 'getinfo'
	m.OnGetPageNumber          = 'getpagenumber'
	m.OnGetNameAndLink         = 'getnameandlink'
	m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end
