----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Template Configuration
----------------------------------------------------------------------------------------------------

local GigaApiPath = '/graphql'
local json = require 'utils.json'

local SERIES_LIST_FRAGMENT = [[
fragment SeriesList on Node {
  ... on SerialGroup { series { edges { node { ...SeriesListItem } } } }
}
fragment SeriesListItem on Series {
  title
  firstEpisode { permalink }
  firstVolume { permalink }
}]]

local SERIES_QUERY = [[
query Series($id: String!) {
  series(databaseId: $id) {
    title
    author { name }
    description
    thumbnailUri
    firstEpisode { permalink }
    firstVolume { permalink }
    latestVolume { thumbnailUri }
    episodes: readableProducts(types: [EPISODE], first: 0) { totalCount }
    volumes: readableProducts(types: [VOLUME], first: 0) { totalCount }
  }
}]]


----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Build the series list query for the given number of node ids.
local function SeriesListQuery(count)
	local definitions, nodes = {}, {}
	for i = 1, count do
		definitions[#definitions + 1] = string.format('$id%d: ID!', i)
		nodes[#nodes + 1] = string.format('id%d: node(id: $id%d) { ...SeriesList }', i, i)
	end
	return 'query(' .. table.concat(definitions, ', ') .. ') { ' .. table.concat(nodes, ' ')
		.. ' }\n' .. SERIES_LIST_FRAGMENT
end

-- Send a GraphQL request and return the decoded 'data' table.
local function GraphQLRequest(query, variables)
	HTTP.Reset()
	HTTP.MimeType = 'application/json'
	HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
	if not HTTP.POST(MODULE.RootURL .. GigaApiPath, json.encode({ query = query, variables = variables })) then
		return nil
	end
	local ok, response = pcall(json.decode, HTTP.Document.ToString())
	if not ok then return nil end
	return response.data
end

-- Fetch the series of the given serial group ids.
local function FetchSeriesList(ids, items, seen)
	local batch_size = 10
	for start = 1, #ids, batch_size do
		local batch = {}
		for i = start, math.min(start + batch_size - 1, #ids) do
			batch[#batch + 1] = ids[i]
		end

		local variables = {}
		for i, id in ipairs(batch) do
			variables['id' .. i] = require 'fmd.crypto'.EncodeBase64('SerialGroup:' .. id)
		end

		local data = GraphQLRequest(SeriesListQuery(#batch), variables)
		if not data then return nil end

		for i = 1, #batch do
			local node = data['id' .. i]
			if node and node.series and node.series.edges then
				for _, edge in ipairs(node.series.edges) do
					local item = edge.node
					local permalink = ''
					if item then
						if item.firstEpisode and item.firstEpisode.permalink then
							permalink = item.firstEpisode.permalink
						elseif item.firstVolume and item.firstVolume.permalink then
							permalink = item.firstVolume.permalink
						end
					end
					local key = permalink:gsub('^%a+://[^/]+', '')
					if key ~= '' and not seen[key] then
						seen[key] = true
						items[#items + 1] = { title = item.title, permalink = permalink }
					end
				end
			end
		end
	end
	return items
end

-- Scrape a series page.
local function FetchSeriesPage(path, items, seen)
	local over = GigaSeriesPageXPath or {}
	local item_xpath  = over.item_xpath  or '//ul[contains(@class, "series-list")]/li/a'
	local link_xpath  = over.link_xpath  or '@href'
	local title_xpath = over.title_xpath or 'h2[contains(@class, "series-list-title")]'

	if not HTTP.GET(MODULE.RootURL .. path) then return nil end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath(item_xpath).Get() do
		local link = x.XPathString(link_xpath, v)
		local title = x.XPathString(title_xpath, v)
		local key = link:gsub('^%a+://[^/]+', '')
		if link ~= '' and title ~= '' and not seen[key] then
			seen[key] = true
			items[#items + 1] = { title = title, permalink = link }
		end
	end
	return items
end

-- Fetch all chapters of one type.
local function FetchReadableProducts(series_id, product_type, count)
	local products = {}
	local offset = 0
	while #products < count and offset < 1000 do
		local u = MODULE.RootURL .. '/api/viewer/pagination_readable_products?type=' .. product_type
			.. '&aggregate_id=' .. series_id .. '&sort_order=desc&offset=' .. offset
		if not HTTP.XHR(u) then return nil end

		local ok, batch = pcall(json.decode, HTTP.Document.ToString())
		if not ok or #batch == 0 then break end
		for _, product in ipairs(batch) do
			products[#products + 1] = product
		end
		offset = offset + #batch
	end
	return products
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local items, seen = {}, {}

	if #GigaDirectoryList > 0 and not FetchSeriesList(GigaDirectoryList, items, seen) then
		return net_problem
	end
	for _, path in ipairs(GigaSeriesPages or {}) do
		if not FetchSeriesPage(path, items, seen) then return net_problem end
	end

	for _, item in ipairs(items) do
		LINKS.Add(item.permalink)
		NAMES.Add(item.title)
	end

	return no_error
end

-- Get info and chapter list for the current manga.
function _M.GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local series_id = x.XPathString('//script[contains(@class, "js-valve")]/@data-giga_series')
	if series_id == '' then
		local episode_json = x.XPathString('//script[@id="episode-json"]/@data-value')
		if episode_json ~= '' then
			local ok, episode = pcall(json.decode, episode_json)
			if ok then series_id = episode.readableProduct.series.id or '' end
		end
	end
	if series_id == '' then return net_problem end

	local data = GraphQLRequest(SERIES_QUERY, { id = series_id })
	if not data or not data.series then return net_problem end
	local series = data.series

	MANGAINFO.Title     = series.title
	MANGAINFO.Authors   = series.author and series.author.name or ''
	MANGAINFO.Summary   = series.description

	MANGAINFO.CoverLink = series.thumbnailUri
	if MANGAINFO.CoverLink == '' and series.latestVolume then
		MANGAINFO.CoverLink = series.latestVolume.thumbnailUri
	end

	local episodes = FetchReadableProducts(series_id, 'episode', tonumber(series.episodes and series.episodes.totalCount) or 0)
	local volumes  = FetchReadableProducts(series_id, 'volume', tonumber(series.volumes and series.volumes.totalCount) or 0)
	if not episodes or not volumes then return net_problem end

	local show_paid = MODULE.GetOption('showpaid')
	local show_unavailable = MODULE.GetOption('showunavailable')

	for _, list in ipairs({ episodes, volumes }) do
		for i = #list, 1, -1 do
			local product = list[i]
			local purchase = product.purchase_info or {}
			local can_read = purchase.can_read
			local unavailable
			if GigaIsUnavailable then
				unavailable = GigaIsUnavailable(product)
			else
				unavailable = purchase.unavailable
			end
			if can_read or (show_paid and not can_read) or (show_unavailable and unavailable) then
				local product_type = 'episode'
				if product.viewer_uri and product.viewer_uri:find('/volume/', 1, true) then
					product_type = 'volume'
				end

				local prefix = ''
				if unavailable then
					prefix = '🔒 '
				elseif not can_read then
					prefix = '💴 '
				end
				if product_type == 'volume' then
					prefix = prefix .. '(Volume) '
				end

				local link = product.viewer_uri
				if not link then
					link = MODULE.RootURL .. '/' .. product_type .. '/' .. product.readable_product_id
				end
				MANGAINFO.ChapterLinks.Add(link)
				MANGAINFO.ChapterNames.Add(prefix .. (product.title or ''))
			end
		end
	end

	return no_error
end

-- Get the page count and/or page links for the current chapter.
function _M.GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if not HTTP.GET(u) then return false end

	local x = CreateTXQuery(HTTP.Document)
	local s = x.XPathString('//script[@id="episode-json"]/@data-value')
	if s == '' then return false end

	local ok, episode = pcall(json.decode, s)
	if not ok then return false end

	local pageStructure = episode.readableProduct.pageStructure
	if not pageStructure or not pageStructure.pages or #pageStructure.pages == 0 then
		print('This chapter is either unavailable or must be purchased.')
		return false
	end

	local scrambled = pageStructure.choJuGiga == 'baku'
	for _, page in ipairs(pageStructure.pages) do
		if page.type == 'main' and page.src and page.src ~= '' then
			TASK.PageLinks.Add(scrambled and (page.src .. '#scramble') or page.src)
		end
	end

	return true
end

-- Download, decrypt and/or descramble image given the image URL.
function _M.DownloadImage()
	if not HTTP.GET(URL) then return false end

	if URL ~= URL:gsub('%#scramble$', '') then
		local fragment_size = 4
		local puzzle = require('fmd.imagepuzzle').Create(fragment_size, fragment_size)
		puzzle.Multiply = 8
		for i = 0, (fragment_size * fragment_size) - 1 do
			puzzle.Matrix[i] = (i % fragment_size) * fragment_size + (i // fragment_size)
		end
		puzzle.DeScramble(HTTP.Document, HTTP.Document)
	end

	return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function _M.AddOptions(m)
	local slang = require 'fmd.env'.SelectedLanguage
	local translations = {
		['en'] = {
			['showpaid'] = 'Show paid chapters',
			['showunavailable'] = 'Show unavailable chapters'
		},
		['id_ID'] = {
			['showpaid'] = 'Tampilkan bab berbayar',
			['showunavailable'] = 'Tampilkan bab tak tersedia'
		}
	}
	local lang = translations[slang] or translations['en']
	m.AddOptionCheckBox('showpaid', lang.showpaid, false)
	m.AddOptionCheckBox('showunavailable', lang.showunavailable, false)
end

----------------------------------------------------------------------------------------------------
-- Template After-Initialization
----------------------------------------------------------------------------------------------------

return _M
