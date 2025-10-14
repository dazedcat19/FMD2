----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '23eb3a472201427e8824ecdd5223bad7'
	m.Name                     = 'MangaFire'
	m.RootURL                  = 'https://mangafire.to'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['lang'] = 'Language:',
			['listtype'] = 'List type:',
			['type'] = 'Chapter\nVolume'
		},
		['id_ID'] = {
			['lang'] = 'Bahasa:',
			['listtype'] = 'Tipe daftar:',
			['type'] = 'Bab\nJilid'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}

	local items = 'None'
	local t = GetLangList()
	for k, v in ipairs(t) do items = items .. '\r\n' .. v end
	m.AddOptionComboBox('lang', lang:get('lang'), items, 1)
	m.AddOptionComboBox('listtype', lang:get('listtype'), lang:get('type'), 0)
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/newest?page='

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

local VRF_JS = [=[
function atob(data){if(arguments.length===0){throw new TypeError("1 argument required, but only 0 present.")}var keystr="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";function atobLookup(chr){var index=keystr.indexOf(chr);return index<0?undefined:index}data=""+data;data=data.replace(/[ \t\n\f\r]/g,"");if(data.length%4===0){data=data.replace(/==?$/,"")}if(data.length%4===1||/[^+/0-9A-Za-z]/.test(data)){return null}var output="";var buffer=0;var accumulatedBits=0;for(var i=0;i<data.length;i++){buffer<<=6;buffer|=atobLookup(data[i]);accumulatedBits+=6;if(accumulatedBits===24){output+=String.fromCharCode((buffer&16711680)>>16);output+=String.fromCharCode((buffer&65280)>>8);output+=String.fromCharCode(buffer&255);buffer=accumulatedBits=0}}if(accumulatedBits===12){buffer>>=4;output+=String.fromCharCode(buffer)}else if(accumulatedBits===18){buffer>>=2;output+=String.fromCharCode((buffer&65280)>>8);output+=String.fromCharCode(buffer&255)}return output}
function btoa(s){if(arguments.length===0){throw new TypeError("1 argument required, but only 0 present.")}var keystr="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";function btoaLookup(index){if(index>=0&&index<64){return keystr[index]}return undefined}var i;s=""+s;for(i=0;i<s.length;i++){if(s.charCodeAt(i)>255){return null}}var out="";for(i=0;i<s.length;i+=3){var groupsOfSix=[undefined,undefined,undefined,undefined];groupsOfSix[0]=s.charCodeAt(i)>>2;groupsOfSix[1]=(s.charCodeAt(i)&3)<<4;if(s.length>i+1){groupsOfSix[1]|=s.charCodeAt(i+1)>>4;groupsOfSix[2]=(s.charCodeAt(i+1)&15)<<2}if(s.length>i+2){groupsOfSix[2]|=s.charCodeAt(i+2)>>6;groupsOfSix[3]=s.charCodeAt(i+2)&63}for(var j=0;j<groupsOfSix.length;j++){if(typeof groupsOfSix[j]==="undefined"){out+="="}else{out+=btoaLookup(groupsOfSix[j])}}}return out}
var atob_=atob;var btoa_=btoa;var toBytes=function(str){var bytes=[];for(var i=0;i<str.length;i++){bytes.push(str.charCodeAt(i)&255)}return bytes};var fromBytes=function(bytes){var str="";for(var i=0;i<bytes.length;i++){str+=String.fromCharCode(bytes[i]&255)}return str};function rc4Bytes(key,input){var s=[];for(var i=0;i<256;i++){s[i]=i}var j=0;for(var i=0;i<256;i++){j=(j+s[i]+key.charCodeAt(i%key.length))&255;var temp=s[i];s[i]=s[j];s[j]=temp}var out=new Array(input.length);i=0;j=0;for(var y=0;y<input.length;y++){i=(i+1)&255;j=(j+s[i])&255;var temp=s[i];s[i]=s[j];s[j]=temp;var k=s[(s[i]+s[j])&255];out[y]=(input[y]^k)&255}return out}
function transform(input,initSeedBytes,prefixKeyString,prefixLen,schedule){var out=[];for(var i=0;i<input.length;i++){if(i<prefixLen)out.push(prefixKeyString.charCodeAt(i)&255);out.push(schedule[i%10]((input[i]^initSeedBytes[i%32])&255)&255)}return out}var add8=function(n){return function(c){return(c+n)&255}};var sub8=function(n){return function(c){return(c-n+256)&255}};var xor8=function(n){return function(c){return c^n}};var rotl8=function(n){return function(c){return((c<<n)|(c>>>(8-n)))&255}};
var scheduleC=[sub8(48),sub8(19),xor8(241),sub8(19),add8(223),sub8(19),sub8(170),sub8(19),sub8(48),xor8(8),];var scheduleY=[rotl8(4),add8(223),rotl8(4),xor8(163),sub8(48),add8(82),add8(223),sub8(48),xor8(83),rotl8(4),];var scheduleB=[sub8(19),add8(82),sub8(48),sub8(170),rotl8(4),sub8(48),sub8(170),xor8(8),add8(82),xor8(163),];var scheduleJ=[add8(223),rotl8(4),add8(223),xor8(83),sub8(19),add8(223),sub8(170),add8(223),sub8(170),xor8(83),];
var scheduleE=[add8(82),xor8(83),xor8(163),add8(82),sub8(170),xor8(8),xor8(241),add8(82),add8(176),rotl8(4),];function base64UrlEncodeBytes(bytes){var std=btoa_(fromBytes(bytes));return std.replace(/\+/g,"-").replace(/\//g,"_").replace(/=+$/,"")}function bytesFromBase64(b64){return toBytes(atob_(b64))}
var CONST={rc4Keys:{l:"u8cBwTi1CM4XE3BkwG5Ble3AxWgnhKiXD9Cr279yNW0=",g:"t00NOJ/Fl3wZtez1xU6/YvcWDoXzjrDHJLL2r/IWgcY=",B:"S7I+968ZY4Fo3sLVNH/ExCNq7gjuOHjSRgSqh6SsPJc=",m:"7D4Q8i8dApRj6UWxXbIBEa1UqvjI+8W0UvPH9talJK8=",F:"0JsmfWZA1kwZeWLk5gfV5g41lwLL72wHbam5ZPfnOVE=",},seeds32:{A:"pGjzSCtS4izckNAOhrY5unJnO2E1VbrU+tXRYG24vTo=",V:"dFcKX9Qpu7mt/AD6mb1QF4w+KqHTKmdiqp7penubAKI=",N:"owp1QIY/kBiRWrRn9TLN2CdZsLeejzHhfJwdiQMjg3w=",P:"H1XbRvXOvZAhyyPaO68vgIUgdAHn68Y6mrwkpIpEue8=",k:"2Nmobf/mpQ7+Dxq1/olPSDj3xV8PZkPbKaucJvVckL0=",},prefixKeys:{O:"Rowe+rg/0g==",v:"8cULcnOMJVY8AA==",L:"n2+Og2Gth8Hh",p:"aRpvzH+yoA==",W:"ZB4oBi0=",},};
function crc_vrf(input){var bytes=toBytes(encodeURIComponent(input));bytes=rc4Bytes(atob_(CONST.rc4Keys.l),bytes);bytes=transform(bytes,bytesFromBase64(CONST.seeds32.A),atob_(CONST.prefixKeys.O),7,scheduleC);bytes=rc4Bytes(atob_(CONST.rc4Keys.g),bytes);bytes=transform(bytes,bytesFromBase64(CONST.seeds32.V),atob_(CONST.prefixKeys.v),10,scheduleY);bytes=rc4Bytes(atob_(CONST.rc4Keys.B),bytes);bytes=transform(bytes,bytesFromBase64(CONST.seeds32.N),atob_(CONST.prefixKeys.L),9,scheduleB);bytes=rc4Bytes(atob_(CONST.rc4Keys.m),bytes);bytes=transform(bytes,bytesFromBase64(CONST.seeds32.P),atob_(CONST.prefixKeys.p),7,scheduleJ);bytes=rc4Bytes(atob_(CONST.rc4Keys.F),bytes);bytes=transform(bytes,bytesFromBase64(CONST.seeds32.k),atob_(CONST.prefixKeys.W),5,scheduleE);return base64UrlEncodeBytes(bytes)}
]=];

local function GenerateVRF(input)
	local script = VRF_JS .. "\ncrc_vrf('" .. input:gsub("'", "\\'") .. "');"
	local result = require 'fmd.duktape'.ExecJS(script)
	return result
end

local Langs = {
	["en"] = "English",
	["fr"] = "French",
	["ja"] = "Japanese",
	["pt-br"] = "Portuguese (Br)",
	["pt"] = "Portuguese (Pt)",
	["es-la"] = "Spanish (LATAM)",
	["es"] = "Spanish (Es)"
}

function GetLangList()
	local t = {}
	for k, v in pairs(Langs) do table.insert(t, v) end
	table.sort(t)
	return t
end

local function FindLanguage(lang)
	local t = GetLangList()
	for i, v in ipairs(t) do
		if i == lang then
			lang = v
			break
		end
	end
	for k, v in pairs(Langs) do
		if v == lang then return k end
	end
	return nil
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()]/a/@href/substring-after(., "=")')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="info"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.AltTitles = x.XPathString('//div[@class="manga-detail"]//h6')
	MANGAINFO.CoverLink = x.XPathString('(//div[@class="poster"])[1]//img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="meta"]/div[./span="Author:"]/span/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="meta"]/div[./span="Genres:"]/span/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="info"]/p'), 'Releasing', 'Completed', 'On_hiatus', 'Discontinued')
	MANGAINFO.Summary   = x.XPathString('string-join(//div[@class="modal-content p-4"]/text(), "\r\n")')

	local listtype     = {'chapter', 'volume'}
	local sel_listtype = (MODULE.GetOption('listtype') or 0) + 1
	local optlang      = MODULE.GetOption('lang')
	local optlangid    = FindLanguage(optlang)

	if optlangid == nil then langparam = '' else langparam = optlangid end
	local vrf = GenerateVRF(URL:match('%.(.-)$') .. '@' .. listtype[sel_listtype] .. '@' .. langparam)

	if not HTTP.GET(MODULE.RootURL .. '/ajax/read/' .. URL:match('%.(.-)$') .. '/' .. listtype[sel_listtype] .. '/' .. langparam .. '?vrf=' .. vrf) then return net_problem end

	x.ParseHTML(require 'utils.json'.decode(HTTP.Document.ToString()).result.html)
	for v in x.XPath('//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('data-id'))
		MANGAINFO.ChapterNames.Add(x.XPathString('text()', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local listtype     = {'chapter', 'volume'}
	local sel_listtype = (MODULE.GetOption('listtype') or 0) + 1
	local vrf = GenerateVRF(listtype[sel_listtype] .. '@' .. URL:match('/(.-)$'))

	local u = MODULE.RootURL .. '/ajax/read/' .. listtype[sel_listtype] .. URL .. '?vrf=' .. vrf

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).result.images()(1)', TASK.PageLinks)

	return true
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end