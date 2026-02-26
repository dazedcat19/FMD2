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

math.randomseed(os.time())

local function RandomHex()
    local hex = ''
    for i = 1, 16 do
        hex = hex .. string.format('%02x', math.random(0, 255))
    end
    return hex
end

-- Set the required http header for making a request.
local function SetRequestHeaders()
	HTTP.Headers.Values['Origin'] = MODULE.RootURL
	HTTP.Headers.Values['X-Mhub-Access'] = RandomHex()
	HTTP.MimeType = 'application/json'
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
	local s = '{"query":"{manga(x:' .. Variables .. ',slug:\\"' .. URL:match('manga/(.-)$') .. '\\"){title,slug,status,image,author,artist,genres,description,alternativeTitle,chapters{number,title}}}"}'
	SetRequestHeaders()

	if not HTTP.POST(API_URL, s) then return net_problem end

	local x = json.decode(HTTP.Document.ToString())
	if x.errors then MANGAINFO.Title = 'Error: ' .. x.errors[1].message return no_error end
	MANGAINFO.Title     = x.data.manga.title
	MANGAINFO.AltTitles = x.data.manga.alternativeTitle
	MANGAINFO.CoverLink = 'https://thumb.mghcdn.com/' .. x.data.manga.image
	MANGAINFO.Authors   = x.data.manga.author
	MANGAINFO.Artists   = x.data.manga.artist
	MANGAINFO.Genres    = x.data.manga.genres
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.data.manga.status)
	MANGAINFO.Summary   = x.data.manga.description

	local slug = x.data.manga.slug
	for _, v in ipairs(x.data.manga.chapters) do
		local title = v.title:gsub('[\n\t]+', ' ')
		local number = v.number

		title = title:find(number, 1, true) and title or 'Chapter ' .. number .. (title ~= '' and ' - ' .. title or '')

		MANGAINFO.ChapterLinks.Add(slug .. '/chapter-' .. number)
		MANGAINFO.ChapterNames.Add(title)
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local chapter = URL:match('(%d+)$')
	local slug = URL:match('^/(.-)/')
	local s = '{"query":"{chapter(x:' .. Variables .. ',slug:\\"' .. slug ..'\\",number:' .. chapter .. '){pages}}"}'
	HTTP.Reset()
	SetRequestHeaders()

	if not HTTP.POST(API_URL, s) then return false end

	local x = json.decode(HTTP.Document.ToString())
	if x.errors then print('Error: ' .. x.errors[1].message) return true end
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