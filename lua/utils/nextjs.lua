local json = require 'utils.json'
local NextJs = {}

local function UnescapeFlightString(s)
	local ok, decoded = pcall(json.decode, '"' .. s .. '"')
	if ok then
		return decoded
	end
end

function NextJs.ExtractFlightText(html)
	local t = {}

	for _, payload in html:gmatch('self%.__next_f%.push%(%[(%d+)%s*,%s*"(.-)"%]%)') do
		t[#t + 1] = UnescapeFlightString(payload)
	end

	return table.concat(t, '\n')
end

local function ParseJsonRecord(line)
	local id, value = line:match('^([%w_]+):.-(%b{})$')
	if not id then
		id, value = line:match('^([%w_]+):.-(%b[])$')
	end

	if value then
		local ok, obj = pcall(json.decode, value)
		if ok then
			return id, obj
		end
	end
end

function NextJs.ParseFlightRecords(html)
	local result = {}

	local text = NextJs.ExtractFlightText(html)

	for line in text:gmatch('[^\n]+') do
		local id, obj = ParseJsonRecord(line)
		if id then
			result[id] = obj
		end
	end

	return result
end

function NextJs.FindKey(obj, key)
	if type(obj) ~= 'table' then
		return nil
	end

	if obj[key] ~= nil then
		return obj[key]
	end

	for _, v in pairs(obj) do
		local r = NextJs.FindKey(v, key)
		if r ~= nil then
			return r
		end
	end

	return nil
end

function NextJs.FindObject(obj, predicate)
	if type(obj) ~= 'table' then
		return nil
	end

	if predicate(obj) then
		return obj
	end

	for _, v in pairs(obj) do
		local r = NextJs.FindObject(v, predicate)
		if r then
			return r
		end
	end

	return nil
end

function NextJs.GetRootObjects(html)
	local records = NextJs.ParseFlightRecords(html)

	local roots = {}

	for _, obj in pairs(records) do
		roots[#roots + 1] = obj
	end

	return roots
end

return NextJs