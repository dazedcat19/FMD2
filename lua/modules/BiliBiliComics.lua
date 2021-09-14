
local API_URL = ' https://www.bilibilicomics.com/twirp/comic.v2.Comic'
local API_PARAMS = '?device=pc&platform=web'

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ba5c1a22af434aaca6c8c6874b7f54ec'
	m.Name                     = 'BiliBiliComics'
	m.RootURL                  = 'https://www.bilibilicomics.com'
	m.Category                 = 'English'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

function GetNameAndLink()
	x, requestJSON = ApiCall(API_URL .. '/ClassPage' .. API_PARAMS .. '&page_size=1000&page_num=1')

	local results for results in x.XPath('json(*).data()').Get() do
		LINKS.Add('detail/mc' .. x.XPathString('season_id', results))
		NAMES.Add(x.XPathString('title', results))
	end
	return no_error
end

function GetInfo()

	-- Extract manga GUID which is needed for getting info and chapter list:
	local mangaId = URL:match('%d+')
	x, requestJSON = ApiCall(API_URL .. '/ComicDetail' .. API_PARAMS .. '&comic_id=' .. mangaId, true)

	MANGAINFO.Title     = x.XPathString('data/title', requestJSON)
	MANGAINFO.Summary   = x.XPathString('data/evaluate', requestJSON)
	MANGAINFO.Authors   = x.XPathStringAll('json(*).data.author_name()')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).data.styles()')
	MANGAINFO.CoverLink = x.XPathString('data/vertical_cover', requestJSON)

	-- Set status to 'completed' if it's not 'ongoing' or 'hiatus'. The status 'cancelled' will also be set to 'completed':
	local status = x.XPathString('data/status', requestJSON)
	-- TODO : list others status
	if (status == '0') then
		status = 'ongoing'
	else
		print('[' .. MODULE.Name .. '] Unknow manga status: ' .. status)
		status = 'completed'
	end
	MANGAINFO.Status = MangaInfoStatusIfPos(status, 'ongoing', 'completed')

	local chapters = x.XPath('json(*).data.ep_list()')

	for ic = 1, chapters.Count do
		local is_free   = x.XPathString('is_in_free', chapters.Get(ic))
		local is_locked = x.XPathString('is_locked', chapters.Get(ic))
		local number    = x.XPathString('short_title', chapters.Get(ic))
		local title     = x.XPathString('title', chapters.Get(ic))

		-- Add chapter name and link to the manga info list if manga can be read:
		if (is_free == 'true') or (is_locked == 'false') then
			MANGAINFO.ChapterLinks.Add(x.XPathString('id', chapters.Get(ic)))
			MANGAINFO.ChapterNames.Add('Chapter ' .. number .. ' - ' .. title)
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	local json = require "utils.json"
	
	x, requestJSON = ApiCall(API_URL .. '/GetImageIndex' .. API_PARAMS .. '&ep_id=' .. URL:gsub('/', ''))
	imagesPaths = {}
	local imagesData = x.XPath('json(*).data.images()')
	for ic = 1, imagesData.Count do
		local imagePath = x.XPathString('path', imagesData.Get(ic))
		local imageX = x.XPathString('x', imagesData.Get(ic))
		imagesPaths[ic] = imagePath .. '@' .. imageX .. 'w.jpg'
	end

	GetPageToken(json.encode(imagesPaths))
end

function GetPageToken(paths)
	x, requestJSON = ApiCall(API_URL .. '/ImageToken' .. API_PARAMS .. '&urls=' .. paths)

	local imagesToken = x.XPath('json(*).data()')
	for ic = 1, imagesToken.Count do
		local imageTokenUrl = x.XPathString('url', imagesToken.Get(ic))
		local imageToken = x.XPathString('token', imagesToken.Get(ic))
		TASK.PageLinks.Add(imageTokenUrl .. API_PARAMS .. '&token=' .. imageToken)
	end
end

function ApiCall(requestUrl, errorToSummary)
	local crypto = require 'fmd.crypto'

	if HTTP.POST(requestUrl) then
		local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))

		local requestJSON     = x.XPath('json(*)')
		local requestCode     = x.XPathString('code', requestJSON)
		local requestMessage  = x.XPathString('msg', requestJSON)

		if requestCode == '0' then
			return x, requestJSON
		else
			errorMessage = requestCode .. ': ' .. requestMessage
			if (errorToSummary == true) then MANGAINFO.Summary = errorMessage end
			print(errorMessage)
			return net_problem
		end
	else
		return net_problem
	end
end
