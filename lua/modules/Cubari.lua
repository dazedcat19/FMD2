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
	local chapters, group, group_id, json, id, title, url, w, x = nil
	local chapter, v = {}
	local source, slug = URL:match('read/([%a-]+)/([%w-]+)/')
	local optgroup = MODULE.GetOption('showgroup')
	local u = string.format('%s/read/api/%s/series/%s/', MODULE.RootURL, source, slug)

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
		id = x.XPathString('chapter_id', v)
		for w in x.XPath('jn:keys(groups)', v).Get() do
			group_id = w.ToString()
			group = '[' .. x.XPathString('(groups)(' .. group_id .. ')', json) .. ']'
			if not optgroup then group = '' end

			title = x.XPathString('title', v)
			title = title ~= 'null' and title ~= '' and string.format('- %s', title) or ''

			url = string.format('/read/%s/%s/%s/#%s', source, slug, id, group_id):gsub('%.', '-')
			title = string.format('%s %s %s', id, title, group)
			table.insert(chapter, {url=url,title=title})
		end
	end
	table.sort(chapter, function (a,b) return (tonumber(a.title:match('^%d+')) or 0) < (tonumber(b.title:match('^%d+')) or 0) end)
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
	local source, slug = URL:match('read/([%a-]+)/([%w-]+)/')
	local u = string.format('%s/read/api/%s/series/%s/', MODULE.RootURL, source, slug)

	if not HTTP.GET(u) then return false end

	x = CreateTXQuery(HTTP.Document)
	json = x.XPath('(json(*).chapters)("' .. ch .. '")')
	group_content = x.XPathString('(groups)(' .. group_id .. ')', json)
	if group_content:match('^https?://') then
		for v in x.XPath('jn:members((groups)(' .. group_id .. '))', json).Get() do
			TASK.PageLinks.Add(v.ToString())
		end
	else
		u = MODULE.RootURL .. group_content
		if not HTTP.GET(u) then return false end
		CreateTXQuery(HTTP.Document).XPathStringAll('json(*)()', TASK.PageLinks)
	end
	if TASK.PageLinks.Count == 0 then
		for v in x.XPath('jn:members((groups)(' .. group_id .. '))', json).Get() do
			TASK.PageLinks.Add(v.GetProperty('src').ToString())
		end
	end

	return true
end