----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.mghcdn.com/graphql'
local CDN_URL = 'https://imgx.mghcdn.com/'
local MangaPerPage = 30
local json = require 'utils.json'

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Set the required http header for making a request.
local function SetRequestHeaders()
	if not HTTP.GET(MODULE.RootURL) then return net_problem end

	local mhub_access = HTTP.Cookies.Values['mhub_access']
	HTTP.Reset()
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['X-Mhub-Access'] = mhub_access
	HTTP.MimeType = 'application/json'

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local json = require 'utils.json'
	local s = '{"query":"{search(x:' .. Variables .. ',q:\\"\\",genre:\\"all\\",mod:ALPHABET,count:true,offset:0){count}}"}'
	SetRequestHeaders()

	if not HTTP.POST(API_URL, s) then return net_problem end

	PAGENUMBER = math.ceil(json.decode(HTTP.Document.ToString()).data.search.count / MangaPerPage)

	return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local offset = MangaPerPage * tonumber(URL)
	local s = '{"query":"{search(x:' .. Variables .. ',q:\\"\\",genre:\\"all\\",mod:ALPHABET,count:true,offset:' .. tostring(offset) .. '){rows{title,slug}}}"}'
	SetRequestHeaders()

	if not HTTP.POST(API_URL, s) then return net_problem end

	local data = json.decode(HTTP.Document.ToString()).data.search.rows
	for _, v in ipairs(data) do
		LINKS.Add('manga/' .. v.slug)
		NAMES.Add(v.title)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local s = '{"query":"{manga(x:mh01,slug:\\"' .. URL:match('manga/(.-)$') .. '\\"){title,slug,status,image,author,artist,genres,description,alternativeTitle,chapters{number,title,slug}}}"}'
	SetRequestHeaders()

	if not HTTP.POST(API_URL, s) then return net_problem end

	local x = json.decode(HTTP.Document.ToString()).data.manga
	if x.errors ~= nil then MANGAINFO.Title = 'API rate limit excessed! Please try again later.' return no_error end
	MANGAINFO.Title     = x.title
	MANGAINFO.AltTitles = x.alternativeTitle
	MANGAINFO.CoverLink = 'https://thumb.mghcdn.com/' .. x.image
	MANGAINFO.Authors   = x.author
	MANGAINFO.Artists   = x.artist
	MANGAINFO.Genres    = x.genres
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.status)
	MANGAINFO.Summary   = x.description

	local slug = x.slug
	for _, v in ipairs(x.chapters) do
		MANGAINFO.ChapterLinks.Add(slug .. '/chapter-' .. v.number)
		MANGAINFO.ChapterNames.Add(v.title)
	end

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local chapter = URL:match('(%d+)$')
	local slug = URL:match('^/(.-)/')
	local s = '{"query":"{chapter(x:' .. Variables .. ',slug:\\"' .. slug ..'\\",number:' .. chapter .. '){pages}}"}'
	SetRequestHeaders()

	if not HTTP.POST(API_URL, s) then return false end

	local x = json.decode(HTTP.Document.ToString())
	if x.errors ~= nil then print('API rate limit excessed! Please try again later.') return true end
	local w = json.decode(x.data.chapter.pages)
	local p = w.p
	for _, v in ipairs(w.i) do
		TASK.PageLinks.Add(CDN_URL .. p .. v)
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M