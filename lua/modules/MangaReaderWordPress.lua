function GetInfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL,URL)
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.CoverLink = x.XPathString('//*[@id="content"]//img/@src')
		MANGAINFO.Title = x.XPathString('//*[@id="titlemove"]/h1')
		MANGAINFO.Authors = x.XPathString('//div[contains(@class, "tsinfo")]/div[contains(text(), "Author")]/i')
		MANGAINFO.Artists = x.XPathString('//div[contains(@class, "tsinfo")]/div[contains(text(), "Artist")]/i')
		MANGAINFO.Genres = x.XPathStringAll('//a[@rel="tag"]')
		MANGAINFO.Status = MangaInfoStatusIfPos((x.XPathString('//div[contains(@class, "tsinfo")]/div[contains(text(), "Status")]/i')))
		summary = x.XPathString('//meta[@name="description"]/@content')

		local chapters=x.XPath('//*[@id="chapterlist"]/ul/li//*[@class="eph-num"]')
		if MODULE.Name == 'LuminousScans' then chapters = x.XPath('//*[@id="chapterlist"]/ul/li') end

		for i=1,chapters.Count do
			MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href',chapters.Get(i)))
			MANGAINFO.ChapterNames.Add(x.XPathString('.//*[@class="chapternum"]',chapters.Get(i)))
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		TASK.PageNumber=CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="readerarea"]//img/@src', TASK.PageLinks)
		return true
	else
		return false
	end
end

function GetNameAndLink()
	local chapters_path = 'manga'

	if MODULE.Name == 'LuminousScans' then chapters_path = 'series' end

	if HTTP.GET(MODULE.RootURL .. '/' .. chapters_path .. '/list-mode/') then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="soralist"]//li/a[@class="series"]', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function Init()
	function AddWebsiteModule(id, name, URL, category)
		local m = NewWebsiteModule()
		m.ID = id
		m.Name				= name
		m.RootURL				= URL
		m.Category				= category
		m.OnGetInfo				= 'GetInfo'
		m.OnGetPageNumber		= 'GetPageNumber'
		m.OnGetNameAndLink		= 'GetNameAndLink'
	end
	AddWebsiteModule('f794803973af4e5daab21683d4de873a', 'LuminousScans', 'https://www.luminousscans.com', 'English')
	AddWebsiteModule('86588503fd9e4277802c998cbccbc983', 'AlphaScans', 'https://alpha-scans.org', 'English')
	AddWebsiteModule('275b85bdaafb47fdbc40f51d2bea99e8', 'TheApolloTeam', 'https://theapollo.team', 'English')
end
