local _m = {}

function fileExist(s)
	local f = io.open(s, 'r')
	local r = false
	if f then 
		r = true
		f:close()
	end
	return r
end

function creatReloadStrings()
	local stringTable = {}
	table.insert(stringTable, "Attention Required! | Cloudflare")
	table.insert(stringTable, "Enable JavaScript and cookies to continue")
	return stringTable
end

function _m.sleepOrBreak(self, delay)
	local count = 0
	while count < delay do
		if HTTP.Terminated then 
			break
		end
		count = count + 250
		sleep(250)
	end
end

function _m.IUAMChallengeAnswer(self, body, url)
	local script = body:match('<script.->(.-)</script')

	if script == nil then
		LOGGER.SendError('WebsitBypass[cloudflare]: IUAM challenge detected but failed to parse the javascript\r\n' .. url)
		return nil
	end

	local rooturl = url:match('(https?://[^/]+)') or ''

	local challenge = string.format([[
function btoa(s) {return Duktape.enc('base64', s);};
function atob(s) {return new TextDecoder().decode(Duktape.dec('base64', s));};

var $$e = {}, window = {}, document = {}, navigator = {}, location = { hash: "" },
    setTimeout = function (f, t) { $$e.timeout = t; f(); },
    setInterval = function (f, t) { f(); };

window.addEventListener = function () {};
window.navigator = { userAgent: '%s' };

navigator.cookieEnabled = true;

document.addEventListener = function (e, b, c) { b(); };
document.body = { appendChild: function () {} };

document.getElementById = function (id) {
    if (!$$e[id]) $$e[id] = { style: {}, action: "", submit: function () {} };
    return $$e[id];
};

document.createElement = function (tag) {
    return { firstChild: { href: "%s" }, setAttribute: function () {} };
};

String.prototype.big = function () { return "<big>" + this + "</big>"; };
String.prototype.small = function () { return "<small>" + this + "</small>"; };
String.prototype.bold = function () { return "<b>" + this + "</b>"; };
String.prototype.italics = function () { return "<i>" + this + "</i>"; };
String.prototype.fixed = function () { return "<tt>" + this + "</tt>"; };
String.prototype.strike = function () { return "<strike>" + this + "</strike>"; };
String.prototype.sub = function () { return "<sub>" + this + "</sub>"; };
String.prototype.sup = function () { return "<sup>" + this + "</sup>"; };

]], HTTP.UserAgent, rooturl)
	local i, v; for i, v in body:gmatch('<div%s*id="(%w+%d+)">(.-)</div') do
		if v:find('[]', 1, true) then
			challenge = challenge .. string.format('$$e["%s"] = { innerHTML: "%s" };\r\n', i, v:gsub('"', '\"'))
		end
	end
	challenge = challenge .. script .. '\r\nJSON.stringify($$e);'

	local answer, timeout = duktape.ExecJS(challenge)
	if (answer == nil) or (answer == 'NaN') or (answer == '') then
		LOGGER.SendError('WebsiteBypass[cloudflare]: IUAM challenge detected but failed to solve the javascript challenge\r\n' .. url)
	else
		answer = answer:match('"jschl%-answer":.-"value":"(.-)"')
		timeout = tonumber(answer:match('"timeout":(%d+)')) or 4000
	end

	return timeout, answer
end

function _m.solveIUAMChallenge(self, body, url)
	local timeout, answer = self:IUAMChallengeAnswer(body, url)
	if (answer == nil) or (answer == 'NaN') or (answer == '') then
		return 0
	end

	local form, challengeUUID = body:match('<form (.-="challenge%-form" action="(.-__cf_chl_jschl_tk__=%S+)"(.-)</form>)')
	if (form == nil) or (challengeUUID == nil) then
		LOGGER.SendError('WebsiteBypass[cloudflare]: IUAM challenge detected but failed to parse the form\r\n' .. url)
		return 0
	end
	challengeUUID = challengeUUID:gsub('&amp;', '&')

	-- cloudflare requires a delay
	self:sleepOrBreak(timeout)

	local payload = {}
	local k, n, v = '', '', ''
	for k in form:gmatch('\n%s*<input%s(.-name=".-)/>') do
		n = k:match('name="(.-)"')
		v = k:match('value="(.-)"') or ''
		payload[n] = v
	end

	local i = 0; for _ in pairs(payload) do i = i + 1 end
	if i == 0 then
		LOGGER.SendError('WebsiteBypass[cloudflare]: IUAM challenge detected but failed to parse the form payload\r\n' .. url)
		return 0
	end
	payload['jschl_answer'] = answer

	local rawdata = ''
	for k, v in pairs(payload) do
		rawdata = rawdata .. k .. '=' .. crypto.EncodeURLElement(payload[k]) .. '&'
	end
	rawdata = rawdata:gsub('&$', '')

	local rooturl = url:match('(https?://[^/]+)') or ''

	-- no need to redirect if it a success
	HTTP.FollowRedirection = false
	HTTP.Reset()
	HTTP.Headers.Values['Origin'] = ' ' .. rooturl
	HTTP.Headers.Values['Referer'] = ' ' .. url
	HTTP.MimeType = "application/x-www-form-urlencoded"
	HTTP.Document.WriteString(rawdata)
	HTTP.FollowRedirection = true

	HTTP.Request('POST', rooturl .. challengeUUID)
	local rbody = HTTP.Document.ToString()
	if rbody:find('^Access denied%. Your IP') then
		HTTP.ClearCookiesStorage()
		LOGGER.SendError('WebsiteBypass[cloudflare]: the server has BANNED your IP!\r\n' .. url .. '\r\n' .. rbody)
		return 0
	end
	if HTTP.Cookies.Values["cf_clearance"] ~= "" then
		return 1
	end

	return 0
end

function _m.solveWithWebDriver(self, url)
	local rooturl = url:match('(https?://[^/]+)') or url
	local result = nil
	local cookies_result = nil
	local _status = 1
	local parsed_result = {}
	local cmd_ = {webdriver_exe}
	table.insert(cmd_, webdriver_script)
	table.insert(cmd_, rooturl)
	
	if webdriver_testing then
		table.insert(cmd_, "--testing")
	end
	
	if webdriver_debug then
		table.insert(cmd_, "--debug")
	end
	
	print('WebsiteBypass[cloudflare]: using webdriver ' .. table.concat(cmd_, " "))
	_status, result, _errors = subprocess.RunCommandHide(table.unpack(cmd_))
	
	if not _status then
		LOGGER.SendError("WebsiteBypass[cloudflare]: Please make sure python is installed.")
		return -1
	else
		if webdriver_debug or webdriver_testing then
			print('WebsiteBypass[cloudflare]: ' .. result)
		end
		local result_json = json.decode(result)
		
		if tonumber(result_json['flaresolver_status_code']) == 200 then
			cookies_result = result_json['flaresolver_result']
			print('WebsiteBypass[cloudflare]: Flaresolver returned code ' .. result_json['flaresolver_status_code'] .. ' for ' .. url .. ' with cookies')
		else
			LOGGER.SendError('WebsiteBypass[cloudflare]: Flaresolver returned code ' .. result_json['flaresolver_status_code'] .. ' for ' .. url .. ' without cookies')
		end
		
		if cookies_result then
			parsed_result = json.decode(cookies_result)
		end
		
		if not parsed_result then
			LOGGER.SendError("WebsiteBypass[cloudflare]: webdriver failed to parse response\r\n" .. url)
			return -1
		end
		
		self:applyCookies(parsed_result, url)
		return 2
	end
end

function _m.solveWithWebDriver2(self, url, headless)
	--Blank function
end

function _m.solveChallenge(self, url)
	local body = HTTP.Document.ToString()
	local rc = HTTP.ResultCode
	local result = 0
	
	-- webdriver cloudflare bypass
	if use_webdriver and (result <= 0) then
		result = self:solveWithWebDriver(url)
	end
	
	-- IUAM challenge
	if ((rc == 429) or (rc == 503)) and body:find('<form .-="challenge%-form" action="/.-__cf_chl_jschl_tk__=%S+"') then
		result = self:solveIUAMChallenge(body, url)
	end
	
	if (result <= 0) then
		LOGGER.SendWarning('WebsiteBypass[cloudflare]: no Cloudflare solution found!\r\n' .. url)
	end
	return result	
end

function _m.applyCookies(self, parsedJSON, url, http_m)
	local next = next
	if next(parsedJSON) == nil then
		return
	end
	local rooturl = url:match('(https?://[^/]+)') or url
	if http_m == nil then
		http_m = HTTP
	end
	
	http_m.FollowRedirection = false
	http_m.Reset()
	http_m.Headers.Values['Origin'] = ' ' .. rooturl
	http_m.Headers.Values['Referer'] = ' ' .. url
	http_m.MimeType = "application/x-www-form-urlencoded"
	http_m.FollowRedirection = true
	http_m.ClearCookiesStorage()
	
	local key, value for key, value in pairs(parsedJSON) do
		if key == "user_agent" and value ~= "" then
			http_m.Headers.Values["user-agent"] = value
			http_m.Headers.Values["User-Agent"] = value
			http_m.UserAgent = value
		else
			http_m.Headers.Values["cookie"] = http_m.Headers.Values["cookie"] .. key .. "=" .. value .. ";"
			http_m.Headers.Values["Set-Cookie"] = http_m.Headers.Values["Set-Cookie"] .. key .. "=" .. value .. ";"
			http_m.Cookies.Values[key] = value
		end
	end
end

function load_config()
	local config_json = [[lua\websitebypass\websitebypass_config.json]]
	if not (fileExist(config_json)) then
		local config_table = {
		use_webdriver = false,
		testing = false,
		debug = false
		}
		
		local json_string = json.encode(config_table)
		
		local file_w, err_w = io.open(config_json, "w")
		
		if not file_w then
			-- Handle the error if the file couldn't be opened
			LOGGER.SendError("Error opening file: " .. tostring(err_w))
		else
			-- 3. Write the JSON string to the file
			file_w:write(json_string)
			
			-- 4. Close the file handle
			file_w:close()
			print("WebsiteBypass[cloudflare]: Successfully wrote data to " .. config_json)
		end
	end
	
	local file, err = io.open(config_json, "r")
	
	if not file then
		-- Handle the error if the file couldn't be opened
		LOGGER.SendError("Error opening file: " .. tostring(err))
	else
		-- Read the entire file content into a string
		local content = file:read("*a")
		
		-- Close the file
		file:close()
		
		local config_table = json.decode(content)
		use_webdriver = config_table['use_webdriver']
		webdriver_testing = config_table['testing']
		webdriver_debug = config_table['debug']
	end
	
	

end

function testing_url(METHOD, URL)
	--Blank function for future
end

function _m.bypass(self, METHOD, URL)
	fmd = require 'fmd.env'
	duktape = require 'fmd.duktape'
	crypto = require 'fmd.crypto'
	subprocess = require "fmd.subprocess"
	json = require "utils.json"
	local result = 0
	local maxretry = 3

	load_config()
	
	webdriver_exe = 'python'
	webdriver_script = [[lua\websitebypass\cloudflare.py]]
	flaresolverr = false
	-- use FlareSolverr url to check if it running
	HTTP.GET('http://127.0.0.1:8191/')
	if (HTTP.ResultCode == 200) then
		local x = CreateTXQuery(HTTP.Document)
		local requestJSON = x.XPath('json(*)')
		if (x.XPathString('msg', requestJSON) == "FlareSolverr is ready!") then
			print('WebsiteBypass[cloudflare]: FlareSolverr is running')
			flaresolverr = true
		end
	end
	
	if HTTP.RetryCount > maxretry then 
		maxretry = HTTP.RetryCount
	end
	MODULE.Storage["reload"] = "false"
	
	while maxretry > 0 do
		maxretry = maxretry - 1
		result = self:solveChallenge(URL)
		if webdriver_testing or webdriver_debug then
			testing_url(METHOD, URL)
		end
		if result ~= 0 or HTTP.Terminated then
			break 
		end
		-- delay before retry
		self:sleepOrBreak(1000)
		if not flaresolverr then
			HTTP.Reset()
			HTTP.Request(METHOD, URL)
		end
	end
	
	HTTP.RetryCount = maxretry
	
	if result == 2 then -- need to reload
		HTTP.Request(METHOD, URL)
		if flaresolverr then
			local response = HTTP.Document.ToString()
			for k, v in pairs(creatReloadStrings()) do
				if response:find(v) then
					MODULE.Storage["reload"] = "true"
				end
			end
		end
	end
	
	return (result >= 1)
end

return _m