----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------
function Init()
	local m = NewWebsiteModule()
	m.ID                = '854aec3d388e458eb639c3deb60da1e1'
	m.Name              = 'Cubari'
	m.RootURL           = 'https://cubari.moe'
	m.OnGetInfo         = 'GetInfo'
	m.OnGetPageNumber   = 'GetPageNumber'

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['showgroup'] = 'Show group name'
		},
		['id_ID'] = {
			['showgroup'] = 'Tampilkan nama grup'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionCheckBox('showgroup', lang:get('showgroup'), false)
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for the current manga.
function GetInfo()
	local chapters, group, group_id, id, json, title, url, vol, w, x = nil
	local chapter, v = {}
	local source, slug = URL:match('read/(.-)/(.-)/')
	local optgroup = MODULE.GetOption('showgroup')
	local u = MODULE.RootURL .. '/read/api/' .. source .. '/series/' .. slug .. '/'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	json = x.XPath('json(*)')
	MANGAINFO.Title     = x.XPathString('title', json)
	MANGAINFO.CoverLink = x.XPathString('cover', json)
	MANGAINFO.Authors   = x.XPathString('author', json)
	MANGAINFO.Artists   = x.XPathString('artist', json)
	MANGAINFO.Summary   = x.XPathString('description', json)

	chapters = [[
	for $k in jn:keys(chapters)
	return jn:object(object(("chapter_id", $k)), (chapters)($k))
	]]

	for v in x.XPath(chapters, json).Get() do
		for w in x.XPath('jn:keys(groups)', v).Get() do
			vol   = tonumber(x.XPathString('volume', v))
			id    = x.XPathString('chapter_id', v)
			title = x.XPathString('title', v)

			vol = type(vol) == 'number' and string.format('Vol. %s ', vol) or ''
			id = id ~= 'null' and string.format('Ch. %s', id) or ''
			title = title ~= 'null' and title ~= '' and string.format(' - %s', title) or ''

			group_id = w.ToString()
			group = ' [' .. x.XPathString('(groups)(' .. group_id .. ')', json) .. ']'
			if not optgroup then group = '' end

			url = 'read/' .. source .. '/' .. slug .. '/' .. x.XPathString('chapter_id', v):gsub('%.', '-') .. '/#' .. group_id
			title = vol .. id .. title .. group
			table.insert(chapter, {url=url,title=title})
		end
	end
	table.sort(chapter, function (a,b) return (tonumber(a.title:match('Ch. (%d+%.?%d*)')) or 0) < (tonumber(b.title:match('Ch. (%d+%.?%d*)')) or 0) end)
	for _, v in ipairs(chapter) do
		MANGAINFO.ChapterLinks.Add(v.url)
		MANGAINFO.ChapterNames.Add(v.title)
	end
	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local group_content, json, v, x = nil
	local ch = URL:match('/([%d%-]+)/#%d-$'):gsub('-', '.')
	local group_id = URL:match('/#(%d+)$')
	local source, slug = URL:match('read/(.-)/(.-)/')
	local u = MODULE.RootURL .. '/read/api/' .. source .. '/series/' .. slug .. '/'

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	json = x.XPath('(json(*).chapters)("' .. ch .. '")')
	group_content = x.XPathString('(groups)(' .. group_id .. ')', json)
	if group_content:match('^https?://') then
		for v in x.XPath('jn:members((groups)(' .. group_id .. '))', json).Get() do
			TASK.PageLinks.Add(v.ToString())
		end
	elseif group_content:match('^/.-/api/') then
		u = MODULE.RootURL .. group_content
		if not HTTP.GET(u) then return false end
		x = CreateTXQuery(HTTP.Document)
		x.XPathStringAll('json(*)()', TASK.PageLinks)
		if TASK.PageLinks == 0 then x.XPathStringAll('json(*)().src', TASK.PageLinks) end
	else
		for v in x.XPath('jn:members((groups)(' .. group_id .. '))', json).Get() do
			TASK.PageLinks.Add(v.GetProperty('src').ToString())
		end
	end

	return true
end