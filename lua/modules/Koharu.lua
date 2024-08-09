----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '7eafb0a70e59463fb4957950bd7831bd'
	m.Name                     = 'Koharu'
	m.RootURL                  = 'https://koharu.to'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.MaxTaskLimit             = 1
	m.MaxConnectionLimit       = 1
	m.SortedList               = true

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['imagesize'] = 'Image size:',
			['size'] = 'Original\n780x\n980x\n1280x\n1600x'
		},
		['id_ID'] = {
			['imagesize'] = 'Ukuran gambar:',
			['size'] = 'Asli\n780x\n980x\n1280x\n1600x'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionComboBox('imagesize', lang:get('imagesize'), lang:get('size'), 0)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://api.koharu.to/books'
DirectoryPagination = '?page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local x = nil
	local u = API_URL .. DirectoryPagination .. 1
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	PAGENUMBER = tonumber(math.floor(x.XPathString('json(*).total') / x.XPathString('json(*).limit') + 0.5)) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = API_URL .. DirectoryPagination .. (URL + 1)
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).entries()').Get() do
		LINKS.Add('g/' .. x.XPathString('id', v) .. '/' .. x.XPathString('public_key', v))
		NAMES.Add(x.XPathString('title', v))
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local imagesize, link, sel_imagesize, title, x = nil
	local u = API_URL .. '/detail/' .. URL:match('g/(.-)$')
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.CoverLink = x.XPathString('json(*).thumbnails.base') .. x.XPathString('json(*).thumbnails.main.path')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).tags()[namespace="1"].name')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags()[not(namespace="1") and not(namespace="4")].name')
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	imagesize = {'0', '780', '980', '1280', '1600'}
	sel_imagesize = (MODULE.GetOption('imagesize') or 0) + 1
	link = x.XPathString('json(*).id') .. '/' .. x.XPathString('json(*).public_key') .. '/' .. x.XPathString('json(*).data.' .. imagesize[sel_imagesize] .. '.id') .. '/' .. x.XPathString('json(*).data.' .. imagesize[sel_imagesize] .. '.public_key') .. '?v=' .. x.XPathString('json(*).updated_at') .. '&w=' .. imagesize[sel_imagesize]
	if string.find(link, '//') then
		title = 'No image(s) in this selected image size!'
	else
		title = MANGAINFO.Title
	end

	MANGAINFO.ChapterLinks.Add(link)
	MANGAINFO.ChapterNames.Add(title)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local base, v, x = nil
	local u = API_URL .. '/data' .. URL
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	base = x.XPathString('json(*).base')
	for v in x.XPath('json(*).entries().path').Get() do
		TASK.PageLinks.Add(base .. v.ToString())
	end

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return true
end