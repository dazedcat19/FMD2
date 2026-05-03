----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'e1bb1dd018ff498382dba79d28c401d5'
	m.Name                     = 'Vortex Scans'
	m.RootURL                  = 'https://vortexscans.org'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.OnLogin                  = 'Login'
	m.AccountSupport           = true

	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showpaidchapters'] = 'Show paid chapters'
		},
		['id_ID'] = {
			['showpaidchapters'] = 'Tampilkan bab berbayar'
		}
	}
	local lang = translations[slang] or translations.en
	m.AddOptionCheckBox('showpaidchapters', lang.showpaidchapters, false)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.Iken'
API_URL = 'https://api.vortexscans.org'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Sign in to the current website.
function Login()
	Template.Login()

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local mid = HTTP.Document.ToString():match('&quot;postId&quot;:%[0,(%d+)%]')

	if not HTTP.GET(API_URL .. '/api/post?postId=' .. mid) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local info = x.XPath('parse-json(.)?post')
	MANGAINFO.Title     = x.XPathString('postTitle', info)
	MANGAINFO.AltTitles = x.XPathString('alternativeTitles', info)
	MANGAINFO.CoverLink = x.XPathString('featuredImage', info)
	MANGAINFO.Authors   = x.XPathString('author', info)
	MANGAINFO.Artists   = x.XPathString('artist', info)
	MANGAINFO.Genres    = x.XPathString('string-join((genres?*/name, concat(upper-case(substring(seriesType, 1, 1)), lower-case(substring(seriesType, 2)))), ", ")', info)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('seriesStatus', info), 'COMING_SOON|MASS_RELEASED|ONGOING', 'COMPLETED', 'HIATUS', 'CANCELLED|DROPPED')
	MANGAINFO.Summary   = x.XPathString('postContent', info)

	local slug = x.XPathString('slug', info)
	local show_paid_chapters = MODULE.GetOption('showpaidchapters')

	for v in x.XPath('chapters?*', info).Get() do
		local is_accessible = v.GetProperty('isAccessible').ToString() ~= 'false'

		if show_paid_chapters or is_accessible then
			local cid = v.GetProperty('id').ToString()
			local number = v.GetProperty('number').ToString()
			local slug_ch = v.GetProperty('slug').ToString()
			local title = v.GetProperty('title').ToString()

			local chapter = 'Chapter ' .. number

			if title == '' then
				title = chapter
			elseif not title:find(chapter, 1, true) then
				title = chapter .. ' - ' .. title
			end

			MANGAINFO.ChapterLinks.Add(slug .. '/' .. slug_ch .. '/' .. cid)
			MANGAINFO.ChapterNames.Add(title)
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function GetPageNumber()
	local u = API_URL .. '/api/chapter?chapterId=' .. URL:match('/(%d+)$')

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).chapter.images().url', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	Template.BeforeDownloadImage()

	return true
end