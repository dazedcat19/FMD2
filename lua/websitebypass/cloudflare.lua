local _m = {}

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
		-- LOGGER.SendError('WebsitBypass[cloudflare]: IUAM challenge detected but failed to solve the javascript challenge\r\n' .. url .. '\r\n' .. body)
		-- LOGGER.SendError('WebsitBypass[cloudflare]: IUAM challenge detected but failed to solve the javascript challenge\r\n' .. url .. '\r\n' .. challenge)
		LOGGER.SendError('WebsitBypass[cloudflare]: IUAM challenge detected but failed to solve the javascript challenge\r\n' .. url)
	else
		answer = answer:match('"jschl%-answer":.-"value":"(.-)"')
		timeout = tonumber(answer:match('"timeout":(%d+)')) or 4000
	end

	return timeout, answer
end

function _m.sleepOrBreak(self, delay)
	local count = 0
	while count < delay do
		if HTTP.Terminated then break end
		count = count + 250
		sleep(250)
	end
end

function _m.solveIUAMChallenge(self, body, url)
	local timeout, answer = self:IUAMChallengeAnswer(body, url)
	if (answer == nil) or (answer == 'NaN') or (answer == '') then
		return 0
	end

	local form, challengeUUID = body:match('<form (.-="challenge%-form" action="(.-__cf_chl_jschl_tk__=%S+)"(.-)</form>)')
	if (form == nil) or (challengeUUID == nil) then
		LOGGER.SendError('WebsitBypass[cloudflare]: IUAM challenge detected but failed to parse the form\r\n' .. url)
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
		LOGGER.SendError('WebsitBypass[cloudflare]: IUAM challenge detected but failed to parse the form payload\r\n' .. url)
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
		LOGGER.SendError('WebsitBypass[cloudflare]: the server has BANNED your IP!\r\n' .. url .. '\r\n' .. rbody)
		return 0
	end
	if HTTP.Cookies.Values["cf_clearance"] ~= "" then
		return 1
	end

	return 0
end

function _m.solveWithWebDriver(self, url)
	local rooturl = url:match('(https?://[^/]+)') or url

	local s = nil
	print(string.format('WebsitBypass[cloudflare]: using webdriver "%s" "%s" "%s" "%s"',webdriver_exe, webdriver_script, rooturl, HTTP.UserAgent))
	_, s = require("fmd.subprocess").RunCommandHide(webdriver_exe, webdriver_script, rooturl, HTTP.UserAgent)

	if not(s) or (s=="") then
		LOGGER.SendError("WebsitBypass[cloudflare]: webdriver doesn't return anything (timeout)\r\n" .. url)
		return 0
	end
	if not s:lower():find('cf_clearance',1,true) then
		LOGGER.SendError("WebsitBypass[cloudflare]: webdriver can't find cookie 'cf_clearance'\r\n" .. url)
		return 0
	end

	s = require("utils.json").decode(s) or nil
	if not s then
		LOGGER.SendError("WebsitBypass[cloudflare]: webdriver failed to parse cookies\r\n" .. url)
		return 0
	end

	local cookies = {}
	local c for _, c in ipairs(s) do
		cookie = {}
		table.insert(cookie,c["name"].."=" .. c["value"])
		c["name"]=nil
		c["value"]=nil
		if c["expiry"] then
			c["expires"]=os.date('!%a, %d %b %Y %H:%M:%H GMT',c["expiry"])
			c["expiry"]=nil
		end
		for k, v in pairs(c) do
			if type(v) == "boolean" then
				if v == true then table.insert(cookie,tostring(k)) end
			else
				table.insert(cookie,tostring(k).."="..tostring(v))
			end
		end
		table.insert(cookies,table.concat(cookie,"; "))
	end

	if #cookies > 0 then
		HTTP.AddServerCookies(table.concat(cookies,"\n"))
		return 2
	end
end

function _m.solveWithWebDriver2(self, url, headless)
	local rooturl = url:match('(https?://[^/]+)') or url

	local result = nil
	print(string.format('WebsitBypass[cloudflare]: using webdriver "%s" "%s" "%s"', customwebdriver_exe, py_customcloudflare, rooturl))
	_status, result, _errors = require("fmd.subprocess").RunCommandHide(customwebdriver_exe, py_customcloudflare, rooturl)
	
	if not _status then
		LOGGER.SendError("WebsitBypass[cloudflare]: Please make sure FlareSolverr is running.")
		return -1
	end
	
	if result:find("Error") then
		print(result)
		return 0
	end
	
	local parsed_result = {}

	-- for each key-value pair in the JSON object...
	for key, value in result:gmatch('"([^"]+)":%s*"([^"]*)"') do
		-- remove quotes from the key
		key = key:gsub('"', '')
		-- handle strings and other types of values
		value = value:gsub('"', '')

		-- store the key-value pair in the parsed JSON object
		parsed_result[key] = value
	end
	
	if not parsed_result then
		LOGGER.SendError("WebsitBypass[cloudflare]: webdriver2 failed to parse response\r\n" .. url)
		return -1
	end
	
	HTTP.FollowRedirection = false
	HTTP.Reset()
	HTTP.Headers.Values['Origin'] = ' ' .. rooturl
	HTTP.Headers.Values['Referer'] = ' ' .. url
	HTTP.MimeType = "application/x-www-form-urlencoded"
	HTTP.FollowRedirection = true
	HTTP.ClearCookiesStorage()

	for key, value in pairs(parsed_result) do
		if key == 'user_agent' then
			HTTP.Headers.Values['user-agent'] = value
			HTTP.UserAgent = value
		elseif key == "cf_clearance" or key == "csrftoken" then
			HTTP.Headers.Values['cookie'] = HTTP.Headers.Values['cookie'] .. key .. '=' .. value .. ';'
			HTTP.Headers.Values['Set-Cookie'] = HTTP.Headers.Values['cookie'] .. key .. '=' .. value .. ';'
			HTTP.Cookies.Values[key] = value

		end
	end
	return 2
end

function _m.solveChallenge(self, url)
	local body = HTTP.Document.ToString()
	local rc = HTTP.ResultCode
	local result = 0

	-- IUAM challenge
	if ((rc == 429) or (rc == 503)) and body:find('<form .-="challenge%-form" action="/.-__cf_chl_jschl_tk__=%S+"') then
		result = self:solveIUAMChallenge(body, url)
	end

	-- custom cloudflare bypass
	if use_webdriver and customcloudflare and (result <= 0) then
		--result = self:solveWithWebDriver2(url, "true")
		if not (result >= 1) then 
			result = self:solveWithWebDriver2(url, "false")
		end
	end

	if use_webdriver and (result <= 0) then
		result = self:solveWithWebDriver(url)
	end

	if (result <= 0) then
		LOGGER.SendWarning('WebsitBypass[cloudflare]: no Cloudflare solution found!\r\n' .. url)
	end
	return result
end

function fileExist(s)
	local f = io.open(s, 'r')
	local r = false
	if f then r = true f:close() end
	return r
end

function creatReloadStrings()
	local stringTable = {}
	table.insert(stringTable, "Attention Required! | Cloudflare")
	return stringTable
end

function _m.bypass(self, METHOD, URL)
	duktape = require 'fmd.duktape'
	crypto = require 'fmd.crypto'
	fmd = require 'fmd.env'

	use_webdriver = false
	local py_cloudflare = [[lua\websitebypass\cloudflare.py]]
	local js_cloudflare = [[lua\websitebypass\cloudflare.js]]
	py_customcloudflare = [[lua\websitebypass\customcloudflare.py]]
	flaresolverr = [[lua\websitebypass\flaresolverr\flaresolverr.exe]]
	customcloudflare = false
	if fileExist([[lua\websitebypass\use_webdriver]]) then
		if fileExist(py_cloudflare) then
			use_webdriver = true
			webdriver_exe = 'python'
			webdriver_script = py_cloudflare
		elseif fileExist(js_cloudflare) then
			use_webdriver = true
			webdriver_exe = 'node'
			webdriver_script = js_cloudflare
		end
		if fileExist(py_customcloudflare) and fileExist(flaresolverr) then
			use_webdriver = true
			customcloudflare = true
			customwebdriver_exe = 'python'
		end
	end

	local result = 0
	local maxretry = 3
	if HTTP.RetryCount > maxretry then maxretry = HTTP.RetryCount end
	MODULE.Storage["reload"] = "false"

	while maxretry > 0 do
		maxretry = maxretry - 1
		result = self:solveChallenge(URL)
		if result ~= 0 then break end
		if HTTP.Terminated then break end
		-- delay before retry
		self:sleepOrBreak(1000)
		if not customcloudflare then
			HTTP.Reset()
			HTTP.Request('GET', URL)
		end
	end

	HTTP.RetryCount = maxretry

	if result == 2 then -- need to reload
		HTTP.Request(METHOD, URL)
		--x = CreateTXQuery(HTTP.Document)
		--print("-Cookies: " .. HTTP.Cookies.Text)
		--print("-Body: " .. x.XPathString('//body'))
		if customcloudflare then
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
