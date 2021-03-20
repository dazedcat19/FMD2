----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------
local API_URL = "https://jumpg-webapi.tokyo-cdn.com"
local protoc = require "utils.protoc"
local pb = require "pb"

local separator = "↣" -- Save Encryption key in the URL and separate it using obscure char (U+21A3)
local proto_file = "MangaPlus.proto"

-- Local Functions
local function splitString(s, delimiter)
	local result = {};
	for match in (s..delimiter):gmatch("(.-)"..delimiter) do
		table.insert(result, match);
	end
	return result;
end

local function hexToStr(str)
	return str:gsub("%x%x",function(c)return c.char(tonumber(c,16))end)
end

local function readFile(file)
	local f = assert(io.open(file, "rb"))
	local content = f:read("*all")
	f:close()
	return content
end

-- Read File and load it to proto

local curr_path = debug.getinfo(1,'S').source
local curr_script = curr_path:match("[^\\/]*.lua$")
local target_file = curr_path:gsub(curr_script, proto_file):gsub("@",'')

protoc:load(readFile(target_file))

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local url = MaybeFillHost(API_URL, "/api/title_detail?title_id=" .. URL:gsub("[^%d]",""))

	if not HTTP.GET(url) then return net_problem end

	local data = pb.decode("Response", HTTP.Document.ToString())
	if data["success"] == nil then return net_problem end

	local lang = " [en]"
	if data["success"]["titleDetailView"]["title"]["language"] ~= nil then lang = " [es]" end

	MANGAINFO.Title     = data["success"]["titleDetailView"]["title"]["name"] .. lang
	MANGAINFO.CoverLink = data["success"]["titleDetailView"]["titleImageUrl"]
	MANGAINFO.Authors   = data["success"]["titleDetailView"]["title"]["author"]
	MANGAINFO.Summary   = data["success"]["titleDetailView"]["overview"]

	local function addChapter(table)
	if table ~= nil then
		for _,v in pairs(table) do
			local chaptername = v["subTitle"]
			if chaptername == "" then chaptername = v["name"] end
			MANGAINFO.ChapterNames.Add(chaptername)
			MANGAINFO.ChapterLinks.Add(v["chapterId"])
		end
	end
	end

	local first_list = data["success"]["titleDetailView"]["firstChapterList"]
	local last_list = data["success"]["titleDetailView"]["lastChapterList"]
	addChapter(first_list)
	addChapter(last_list)

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(MaybeFillHost(API_URL, "/api/title_list/all")) then return net_problem end

	local data = pb.decode("Response", HTTP.Document.ToString())
	if data["success"] == nil then return net_problem end

	for _,v in pairs(data["success"]["allTitlesView"]["titles"]) do
		local lang = " [en]"
		if v["language"] ~= nil then lang = " [es]" end
		LINKS.Add(MaybeFillHost(MODULE.RootURL,("titles/" .. v["titleId"])))
		NAMES.Add(v["name"] .. lang)
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local url = MaybeFillHost(API_URL, "/api/manga_viewer?chapter_id=" .. string.gsub(URL, "[^%d]", "") .. "&img_quality=super_high&split=yes")

	if not HTTP.GET(url) then return net_problem end

	local data = pb.decode("Response", HTTP.Document.ToString())

	if data["success"] == nil then return net_problem end

	for _,v in pairs(data["success"]["mangaViewer"]["pages"]) do
		if v["mangaPage"] ~= nil then
			local image_url = v["mangaPage"]["imageUrl"]
			local encryption_key = v["mangaPage"]["encryptionKey"]
			TASK.PageLinks.Add(image_url .. separator .. encryption_key)
		end
	end
	return no_error
end

-- Download and decrypt image given the image URL.
function DownloadImage()
	local t = splitString(URL, separator)
	local url = t[1]
	local key = hexToStr(t[2])

	if not HTTP.GET(url) then return false end

	local data = HTTP.Document.ToString()
	local parsed = {}
	for i=1,data:len() do
		parsed[i] = string.char(string.byte(data, i) ~ string.byte(key, ((i-1)%string.len(key))+1))
	end
	HTTP.Document.WriteString(table.concat(parsed, ""))
	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = "f239e87c7a1248d29cdd2ea8a77df36c"
	m.Name                     = "MangaPlus"
	m.RootURL                  = "https://mangaplus.shueisha.co.jp"
	m.Category                 = "English"
	m.OnGetInfo                = "GetInfo"
	m.OnGetNameAndLink         = "GetNameAndLink"
	m.OnGetPageNumber          = "GetPageNumber"
	m.OnDownloadImage          = "DownloadImage"
end