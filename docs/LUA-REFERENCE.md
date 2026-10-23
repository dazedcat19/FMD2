# FMD2 Lua Module Development Guide

## Table of Contents

1. [Overview](#overview)
2. [Module Structure](#module-structure)
    - [Basic Module Template](#basic-module-template)
    - [Event Handler Reference](#event-handler-reference)
    - [Status Constants and Return Values](#status-constants-and-return-values)
3. [Injected Pascal Objects](#injected-pascal-objects)
    - [HTTP Object (THTTPSendThread)](#http-object-thttpsendthread)
        - [HTTP.Document (TMemoryStream)](#httpdocument-tmemorystream)
    - [MODULE Object (TModuleContainer)](#module-object-tmodulecontainer)
    - [Event Handlers - Complete Workflow](#event-handlers---complete-workflow)
        - [1. Module Initialization (`Init`)](#1-module-initialization-init)
        - [2. Before Update List (`OnBeforeUpdateList`)](#2-before-update-list-onbeforeupdatelist)
        - [3. Get Directory Page Number (`OnGetDirectoryPageNumber`)](#3-get-directory-page-number-ongetdirectorypagenumber)
        - [4. Get Name And Link (`OnGetNameAndLink`)](#4-get-name-and-link-ongetnameandlink)
        - [5. After Update List (`OnAfterUpdateList`)](#5-after-update-list-onafterupdatelist)
        - [6. Get Info (`OnGetInfo`)](#6-get-info-ongetinfo)
        - [7. Task Start (`OnTaskStart`)](#7-task-start-ontaskstart)
        - [8. Get Page Number (`OnGetPageNumber`)](#8-get-page-number-ongetpagenumber)
        - [9. Get Image URL (`OnGetImageURL`)](#9-get-image-url-ongetimageurl)
        - [10. Before Download Image (`OnBeforeDownloadImage`)](#10-before-download-image-onbeforedownloadimage)
        - [11. Download Image (`OnDownloadImage`)](#11-download-image-ondownloadimage)
        - [12. Save Image (`OnSaveImage`)](#12-save-image-onsaveimage)
        - [13. After Image Saved (`OnAfterImageSaved`)](#13-after-image-saved-onafterimagesaved)
        - [14. Login (`OnLogin`)](#14-login-onlogin)
        - [15. Account State (`OnAccountState`)](#15-account-state-onaccountstate)
        - [16. Check Site (`OnCheckSite`)](#16-check-site-onchecksite)
    - [Complete Event Flow Example](#complete-event-flow-example)
    - [MANGAINFO Object (TMangaInfo)](#mangainfo-object-tmangainfo)
    - [TASK Object (TTaskContainer)](#task-object-ttaskcontainer)
    - [LINKS and NAMES (TStringList)](#links-and-names-tstringlist)
    - [UPDATELIST Object (TUpdateListManagerThread)](#updatelist-object-tupdatelistmanagerthread)
    - [PAGENUMBER (Global Variable)](#pagenumber-global-variable)
    - [URL (Global Variable)](#url-global-variable)
4. [Built-in Functions](#built-in-functions)
    - [TXQuery (XML/HTML/JSON Parser)](#txquery-xmlhtmljson-parser)
        - [Query Object Methods](#query-object-methods)
        - [Value/Node Object Methods](#valuenode-object-methods)
        - [XPath Extensions](#xpath-extensions)
    - [Utility Functions](#utility-functions)
        - [String Manipulation](#string-manipulation)
        - [Status Helper](#status-helper)
        - [Other Functions](#other-functions)
5. [Lua Libraries](#lua-libraries)
    - [fmd.* Modules (Injected by the host application)](#fmd-modules-injected-by-the-host-application)
        - [fmd.crypto](#fmdcrypto)
        - [fmd.duktape](#fmdduktape)
        - [fmd.env](#fmdenv)
        - [fmd.fileutil](#fmdfileutil)
        - [fmd.logger](#fmdlogger)
        - [fmd.subprocess](#fmdsubprocess)
        - [fmd.mangafoxwatermark](#fmdmangafoxwatermark)
        - [fmd.gzip](#fmdgzip)
        - [fmd.pcre2](#fmdpcre2)
        - [fmd.imagepuzzle](#fmdimagepuzzle)
        - [fmd.strings](#fmdstrings)
    - [utils.* Modules (Lua Files)](#utils-modules-lua-files)
        - [utils.json](#utilsjson)
        - [utils.lzstring](#utilslzstring)
        - [utils.jsunpack](#utilsjsunpack)
        - [utils.nodejs](#utilsnodejs)
        - [utils.nextjs](#utilsnextjs)
        - [utils.sha256](#utilssha256)
        - [utils.protoc (with the `pb` runtime)](#utilsprotoc-with-the-pb-runtime)
6. [Templates](#templates)
    - [Using Templates](#using-templates)
    - [Template Customization](#template-customization)
7. [Common Patterns](#common-patterns)
    - [Error Handling](#error-handling)
    - [Account Support](#account-support)
    - [User Options](#user-options)
    - [JSON API Handling](#json-api-handling)
    - [Multi-Category Sites](#multi-category-sites)
    - [Iterating Over Nodes](#iterating-over-nodes)
8. [Examples](#examples)
    - [Example 1: Simple Module](#example-1-simple-module)
    - [Example 2: API-Based Module with JSON](#example-2-api-based-module-with-json)
    - [Example 3: Module with Login](#example-3-module-with-login)
9. [Development Tips](#development-tips)
    - [1. Use `--lua-dofile` for Development](#1-use---lua-dofile-for-development)
    - [2. Use Templates When Possible](#2-use-templates-when-possible)
    - [3. Set Proper Headers](#3-set-proper-headers)
    - [4. Handle JavaScript-Heavy Sites](#4-handle-javascript-heavy-sites)
    - [5. Check for Paywalls](#5-check-for-paywalls)
    - [6. Use Persistent Storage](#6-use-persistent-storage)
    - [7. Debug with Logging](#7-debug-with-logging)
    - [8. Generate Unique IDs](#8-generate-unique-ids)
10. [File Locations](#file-locations)
11. [Additional Resources](#additional-resources)

---

## Overview

**Free Manga Downloader 2 (FMD2)** is a manga downloading application written in Object Pascal (Lazarus) that uses Lua scripts for website modules. The application supports 600+ manga websites through a modular Lua-based plugin system.

**Modules:** 600+ website modules
**Templates:** 35+ reusable templates
**Lua Version:** 5.4.8 (also supports LuaJIT)
**JavaScript Engine:** Duktape 2.5.0 (embedded) + Node.js (optional)

---

## Module Structure

### Basic Module Template

Every Lua module follows a consistent structure:

```lua
----------------------------------------------------------------------------------------------------
-- Module Constants (optional)
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.website.com'
local Template = require 'templates.Madara'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function GetNameAndLink()
    -- Fetch manga list and populate LINKS and NAMES
end

function GetInfo()
    -- Fetch manga details and populate MANGAINFO
end

function GetPageNumber()
    -- Fetch chapter pages and populate TASK.PageLinks
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
    local m = NewWebsiteModule()
    m.ID                       = 'unique_hex_identifier'  -- 32-character hex string
    m.Name                     = 'Website Name'
    m.RootURL                  = 'https://website.com'
    m.Category                 = 'Category-Name'
    m.OnGetNameAndLink         = 'GetNameAndLink'
    m.OnGetInfo                = 'GetInfo'
    m.OnGetPageNumber          = 'GetPageNumber'
    -- Optional handlers:
    -- m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
    -- m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
    -- m.OnDownloadImage          = 'DownloadImage'
    -- m.OnAfterImageSaved        = 'AfterImageSaved'
    -- m.OnGetImageURL            = 'GetImageURL'
    -- m.OnLogin                  = 'Login'
    -- m.AccountSupport           = true
    -- m.SortedList               = true
    -- m.TotalDirectory           = number_of_categories
end
```

### Event Handler Reference

Register a handler in `Init()` by assigning the **function name as a string** to
the matching `On...` property (for example `m.OnGetInfo = 'GetInfo'`). Each
handler is explained in
[Event Handlers - Complete Workflow](#event-handlers---complete-workflow).

| Handler property | Required | Returns | Injected objects | Injected globals |
|------------------|----------|---------|------------------|------------------|
| `OnGetNameAndLink` | **Yes** | status code | `HTTP`, `LINKS`, `NAMES`, `UPDATELIST` | `URL` |
| `OnGetInfo` | **Yes** | status code | `HTTP`, `MANGAINFO` | `URL` |
| `OnGetPageNumber` | **Yes** | `true` / `false` | `HTTP`, `TASK` | `URL` |
| `OnBeforeUpdateList` | No | `true` = continue, `false` = cancel | `UPDATELIST` | none |
| `OnGetDirectoryPageNumber` | No | status code | `HTTP`, `UPDATELIST` | `PAGENUMBER`, `WORKPTR` |
| `OnAfterUpdateList` | No | `true` / `false` | `UPDATELIST` | none |
| `OnTaskStart` | No | `true` = continue, `false` = cancel | `TASK` | none |
| `OnGetImageURL` | No | `true` / `false` | `HTTP`, `TASK` | `URL`, `WORKID` |
| `OnBeforeDownloadImage` | No | `true` = continue, `false` = skip image | `HTTP`, `TASK` | `URL`, `WORKID` |
| `OnDownloadImage` | No | `true` / `false` | `HTTP`, `TASK` | `URL`, `WORKID` |
| `OnSaveImage` | No | filename (string) | `HTTP` | `PATH`, `FILENAME` |
| `OnAfterImageSaved` | No | `true` / `false` | none | `FILENAME` |
| `OnLogin` | No | `true` / `false` | `HTTP` | account status constants |
| `OnAccountState` | No | `true` / `false` | none | account status constants |
| `OnCheckSite` | No | `true` / `false` | none | account status constants |

"Status code" means one of the [status constants](#status-constants-and-return-values).
`Init` has no injected objects; it creates the module with `NewWebsiteModule()`.

> [!IMPORTANT]
> - `MODULE` is **always available** in every handler except `Init`.
> - `HTTP` is **not available** in `OnAccountState`, `OnCheckSite`, `OnAfterImageSaved` and `OnTaskStart`, so those handlers cannot make network requests.
> - `URL` means different things in different handlers; see [URL](#url-global-variable).
> - Status constants (`no_error`, `net_problem`, ...) are available wherever the table lists a status code or account status constants.

### Status Constants and Return Values

```lua
-- Status codes (returned by GetNameAndLink, GetInfo, GetDirectoryPageNumber)
no_error              = 0  -- Success
net_problem           = 1  -- Network error
information_not_found = 2  -- Content not found

-- Boolean returns (all other handlers)
true   -- Success / continue
false  -- Failure / cancel

-- Account status (MODULE.Account.Status)
asUnknown  = 0  -- Unknown status
asChecking = 1  -- Validating credentials
asValid    = 2  -- Credentials valid
asInvalid  = 3  -- Credentials invalid
```

---

## Injected Pascal Objects

These objects are automatically injected by the host application and available globally in Lua.

### HTTP Object (THTTPSendThread)

HTTP request handler for making web requests.

```lua
-- Basic usage
if HTTP.GET('https://example.com') then
    local html = HTTP.Document.ToString()
end

-- POST request
HTTP.MimeType = 'application/json'
if HTTP.POST('https://api.example.com/data', '{"key":"value"}') then
    -- process response
end

-- XHR request
HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
HTTP.XHR('https://example.com/ajax')
```

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `HTTP.Document` | TMemoryStream | Request/response body — see [HTTP.Document](#httpdocument-tmemorystream) |
| `HTTP.ResultCode` | Integer | HTTP status code (200, 404, etc.) |
| `HTTP.Terminated` | Boolean | Check if download was terminated |
| `HTTP.MimeType` | String | Request content type |
| `HTTP.UserAgent` | String | User agent string |
| `HTTP.RetryCount` | Integer | Number of retry attempts |
| `HTTP.Headers` | TStrings | Request headers (same object kind as `LINKS` — full API in [fmd.strings](#fmdstrings)) |
| `HTTP.Cookies` | TStrings | Cookies (same object kind as `LINKS` — full API in [fmd.strings](#fmdstrings)) |
| `HTTP.EnabledCookies` | Boolean | Enable cookie handling |
| `HTTP.FollowRedirection` | Boolean | Automatically follow 301/302/303/307 redirects (default `true`). Set to `false` before a request to receive the raw 3xx response and inspect `HTTP.Headers.Values['Location']` yourself |
| `HTTP.MaxRedirect` | Integer | Maximum number of redirects followed per request while `HTTP.FollowRedirection` is `true` (default `5`) |
| `HTTP.AllowServerErrorResponse` | Boolean | When `false` (default), a 5xx response is treated as a failure and the request is retried; set to `true` to receive 5xx bodies as-is |
| `HTTP.Compress` | Boolean | When `true` (default), `Reset()`/`ResetBasic()` add `Accept-Encoding: gzip, deflate, br, zstd` and compressed responses are auto-decompressed |
| `HTTP.Timeout` | Integer | Socket connection/read timeout in milliseconds (default `15000`); assigning applies it to the underlying socket immediately |
| `HTTP.ResultString` | String | HTTP status text ("OK", "Not Found", etc.) |
| `HTTP.LastURL` | String | URL of the last completed request |

#### Methods

| Method | Description |
|--------|-------------|
| `HTTP.GET(url)` | Perform HTTP GET request |
| `HTTP.POST(url, data)` | Perform HTTP POST request (`data` is written into the document first; passing `''` sends whatever the document already holds) |
| `HTTP.XHR(url)` | Perform XMLHttpRequest (GET with `X-Requested-With: XMLHttpRequest`) |
| `HTTP.HEAD(url)` | Perform HTTP HEAD request |
| `HTTP.Request(method, url)` | Perform a request with an arbitrary HTTP method, e.g. `HTTP.Request('POST', url)` |
| `HTTP.Reset()` | Reset HTTP state |
| `HTTP.ResetBasic()` | Reset basic settings |
| `HTTP.ClearCookies()` | Clear all cookies |
| `HTTP.GetCookies()` | Get cookies string |
| `HTTP.AddServerCookies(url, cookie_string)` | Add cookies to persistent storage for the URL's domain (first argument may be omitted) |
| `HTTP.ClearCookiesStorage()` | Clear the module's persistent cookie storage |
| `HTTP.ParseServerCookies()` | Parse `Set-Cookie` headers from the last response into cookie storage |
| `HTTP.MergeCookies(cookie_string)` | Merge a `'name=value; other=value'` string into `HTTP.Cookies` (values for existing names are overwritten) |
| `HTTP.RemoveCookie(name)` | Remove the cookie `name` from `HTTP.Cookies` |
| `HTTP.SetProxy(proxyType, host, port, username, password)` | Configure proxy — `proxyType` is `'HTTP'`, `'SOCKS4'` or `'SOCKS5'` (case-insensitive) |

> [!NOTE]
> `HTTP.FollowRedirection`, `HTTP.MaxRedirect`, `HTTP.AllowServerErrorResponse`,
> `HTTP.Compress` and `HTTP.Timeout` are **not** touched by `HTTP.Reset()` or
> `HTTP.ResetBasic()` — once set they stay in effect for later requests. If you
> turn redirection off for a single request, always set it back to `true`
> afterwards.

#### HTTP.Document (TMemoryStream)

`HTTP.Document` is both the request body and the response body of the `HTTP`
object. `GET`/`POST`/`XHR`/`HEAD`/`Request` write the response into it, and
whatever it holds when a request starts is sent as the request body (that is
how `HTTP.Request('POST', url)` sends a body — `HTTP.POST(url, data)` writes
`data` into it for you and clears it first).

Right after a request the stream position is `0`, so `ToString()` returns the
whole body and `WriteString()` overwrites it from the start.

| Member | Description |
|--------|-------------|
| `HTTP.Document.ToString()` | Return the stream content from the current position to the end as a string (the position is restored — after a request that means the whole body) |
| `HTTP.Document.ReadString()` | Alias of `ToString()` |
| `HTTP.Document.WriteString(s)` | Write the raw bytes of `s` at the **current position**, advancing it — does **not** clear or resize the stream by itself |
| `HTTP.Document.Clear()` | Empty the stream (size and position back to 0) |
| `HTTP.Document.LoadFromFile(path)` | Replace the stream content with a file's content |
| `HTTP.Document.SaveToFile(path)` | Write the stream content to a file |
| `HTTP.Document.Size` | Stream size in bytes (read/write; setting it grows or truncates the stream) |

Real usage — decrypt the image in place in `OnDownloadImage`
(`lua/modules/MangaPlus.lua`; the same pattern appears in `PhiliaScans.lua`
and `Comix.lua`):

```lua
function DownloadImage()
    if not HTTP.GET(url) then return false end
    local data = HTTP.Document.ToString()
    local key = ...  -- from the page
    local out = {}
    for i = 1, data:len() do
        out[i] = string.char(string.byte(data, i) ~ string.byte(key, ((i - 1) % key:len()) + 1))
    end
    HTTP.Document.WriteString(table.concat(out, ''))  -- same length = exact overwrite
    return true
end
```

> [!NOTE]
> - `WriteString` writes at the current position without resizing the stream:
>   writing a **shorter** string leaves the old tail at the end, writing a
>   **longer** one grows the stream. Call `Clear()` first when replacing the
>   content with something of a different length.
> - To send a custom request body with an arbitrary method:
>   `HTTP.Document.Clear(); HTTP.Document.WriteString(body);
>   HTTP.MimeType = 'application/...'; HTTP.Request('POST', url)`.
> - `HTTP.Document` is a `TStream`, so it can be fed directly to
>   [fmd.imagepuzzle](#fmdimagepuzzle) — in-place descrambling works:
>   `puzzle.DeScramble(HTTP.Document, HTTP.Document)`.

---

### MODULE Object (TModuleContainer)

Current module information and settings. This is the central object for module configuration and runtime control.

```lua
function Init()
    local m = NewWebsiteModule()
    m.ID = 'unique_hex_identifier'

    -- Configure module properties
    m.Name = 'Website Name'
    m.RootURL = 'https://website.com'
    m.Category = 'English'

    -- Add user options
    m.AddOptionSpinEdit('timeout', 'Timeout (seconds)', 60)
    m.AddOptionCheckBox('showgroup', 'Show group name', false)
end

function GetNameAndLink()
    -- Access module properties
    local timeout = MODULE.GetOption('timeout')
    local root = MODULE.RootURL

    if not HTTP.GET(root .. '/manga') then return net_problem end
    -- process
    return no_error
end
```

#### Properties

| Property | Type | Access | Description |
|----------|------|--------|-------------|
| `MODULE.ID` | String | Read-only | Module's unique identifier (32-char hex) |
| `MODULE.Name` | String | Read-only | Module display name |
| `MODULE.RootURL` | String | Read-only | Website root URL |
| `MODULE.Category` | String | Read-only | Module category (English, Vietnamese, etc.) |
| `MODULE.MaxTaskLimit` | Integer | Read/Write | Maximum concurrent tasks for this module |
| `MODULE.MaxThreadPerTaskLimit` | Integer | Read/Write | Maximum threads per task |
| `MODULE.MaxConnectionLimit` | Integer | Read/Write | Maximum HTTP connections |
| `MODULE.ActiveTaskCount` | Integer | Read-only | Currently active tasks |
| `MODULE.ActiveConnectionCount` | Integer | Read-only | Currently active connections |
| `MODULE.SortedList` | Boolean | Read/Write | Whether manga list is sorted |
| `MODULE.InformationAvailable` | Boolean | Read/Write | Whether manga info is available |
| `MODULE.FavoriteAvailable` | Boolean | Read/Write | Whether favorites are supported |
| `MODULE.DynamicPageLink` | Boolean | Read/Write | Whether page links are dynamic |
| `MODULE.CurrentDirectoryIndex` | Integer | Read/Write | Current category index (for multi-category sites) |
| `MODULE.TotalDirectory` | Integer | Read/Write | Total number of categories |
| `MODULE.AccountSupport` | Boolean | Read/Write | Whether module supports user accounts |
| `MODULE.LastUpdated` | String | Read/Write | Last update timestamp |
| `MODULE.Tag` | Integer | Read/Write | Custom tag for internal use |
| `MODULE.Storage` | TStringsStorage | Read-only | Persistent key-value storage |
| `MODULE.Account` | TWebsiteModuleAccount | Read-only | User account information |
| `MODULE.Guardian` | TCriticalSection | Read-only | Thread synchronization object |

#### Event Handler Properties

The `On...` properties that register event handlers are listed in the
[Event Handler Reference](#event-handler-reference).

#### Methods

| Method | Parameters | Description |
|--------|-----------|-------------|
| `MODULE.GetOption(name)` | `name` (string) | Get user-configured option value |
| `MODULE.AddOptionCheckBox(name, caption, default)` | All strings/boolean | Add checkbox option |
| `MODULE.AddOptionEdit(name, caption, default)` | All strings | Add text input option |
| `MODULE.AddOptionSpinEdit(name, caption, default)` | name, caption (string), default (int) | Add numeric option |
| `MODULE.AddOptionComboBox(name, caption, items, default)` | items is newline-separated | Add dropdown option |
| `MODULE.AddServerCookies(domain, cookie_string)` | Both strings | Add cookies for domain |
| `MODULE.GetServerCookies(domain, path)` | Both strings | Get cookies for domain |
| `MODULE.RemoveCookies(domain, path)` | Both strings | Remove specific cookies |
| `MODULE.ClearCookies()` | None | Clear all cookies |

#### Account Properties

| Property | Type | Access | Description |
|----------|------|--------|-------------|
| `MODULE.Account.Enabled` | Boolean | Read/Write | Is account enabled |
| `MODULE.Account.Username` | String | Read/Write | Username |
| `MODULE.Account.Password` | String | Read/Write | Password |
| `MODULE.Account.Status` | Integer | Read/Write | Account status (see below) |
| `MODULE.Account.Cookies` | String | Read/Write | Account cookies |
| `MODULE.Account.Guardian` | TCriticalSection | Read-only | Thread sync object |

For the `Status` values (`asUnknown`, `asChecking`, `asValid`, `asInvalid`) see
[Status Constants and Return Values](#status-constants-and-return-values).

---

### Event Handlers - Complete Workflow

This section explains how FMD2 calls your module's event handlers and in what order.

Which objects and globals each handler receives is listed in the
[Event Handler Reference](#event-handler-reference).

---

#### 1. Module Initialization (`Init`)

**When:** When FMD2 starts and loads Lua modules
**Called by:** Module loader during application startup
**Purpose:** Register module metadata and event handlers

See the [Basic Module Template](#basic-module-template) for a complete `Init()`.

**Workflow:**
1. FMD2 scans `lua/modules/` folder for `.lua` files
2. For each file, calls `Init()` function
3. `NewWebsiteModule()` creates module object
4. Module properties are set (ID, Name, RootURL, etc.)
5. Event handler strings are stored (function names)
6. Module is added to FMD2's module list
7. If ID or Name is empty, module is discarded

---

#### 2. Before Update List (`OnBeforeUpdateList`)

**When:** Before manga list update begins (optional)
**Called by:** UpdateListManagerThread

```lua
function BeforeUpdateList()
    -- Prepare for update (login, set cookies, etc.)
    return true
end
```

**Workflow:**
1. User clicks "Update" in manga list
2. FMD2 calls `OnBeforeUpdateList` (if defined)
3. If returns `false`, update is cancelled
4. If returns `true`, continues to `OnGetDirectoryPageNumber`

---

#### 3. Get Directory Page Number (`OnGetDirectoryPageNumber`)

**When:** Determining total pages in manga list
**Called by:** UpdateListManagerThread
**Sets:** `PAGENUMBER` global variable

```lua
function GetDirectoryPageNumber()
    if not HTTP.GET(MODULE.RootURL .. '/manga-list?page=1') then return net_problem end
    local x = CreateTXQuery(HTTP.Document)
    PAGENUMBER = tonumber(x.XPathString('//last-page')) or 1
    return no_error
end
```

**Workflow:**
1. FMD2 needs to know total pages
2. Calls `OnGetDirectoryPageNumber`
3. Module sets `PAGENUMBER` to total page count
4. FMD2 uses this for pagination UI

**Note:** besides `PAGENUMBER`, FMD2 also injects the `WORKPTR` global here —
the 0-based index of the directory (category) whose page count is being
fetched. For multi-category sites (`MODULE.TotalDirectory` > 1) use it
together with `MODULE.CurrentDirectoryIndex` to find out which category is
being updated.

---

#### 4. Get Name And Link (`OnGetNameAndLink`)

**When:** Fetching manga list from website
**Called by:** UpdateListManagerThread (repeatedly for each page)
**Sets:** `LINKS` and `NAMES` global lists

```lua
function GetNameAndLink()
    local page = URL  -- URL contains current page number (0-based)
    local u = MODULE.RootURL .. '/manga-list?page=' .. (page + 1)

    if not HTTP.GET(u) then return net_problem end
    local x = CreateTXQuery(HTTP.Document)
    x.XPathHREFAll('//div[@class="manga"]/a', LINKS, NAMES)
    return no_error
end
```

**Workflow:**
1. FMD2 iterates through pages (0 to PAGENUMBER-1)
2. For each page, calls `OnGetNameAndLink`
3. Module adds manga URLs to `LINKS` and names to `NAMES`
4. FMD2 collects all LINKS/NAMES for display
5. Can call `UPDATELIST.UpdateStatusText()` to show progress
6. Can access `UPDATELIST.CurrentDirectoryPageNumber`

---

#### 5. After Update List (`OnAfterUpdateList`)

**When:** After manga list update completes (optional)
**Called by:** UpdateListManagerThread

```lua
function AfterUpdateList()
    -- Cleanup or final processing
    return true
end
```

**Workflow:**
1. All pages have been fetched
2. FMD2 calls `OnAfterUpdateList` (if defined)
3. Module can perform cleanup or additional processing

---

#### 6. Get Info (`OnGetInfo`)

**When:** Fetching manga details (title, chapters, etc.)
**Called by:** TMangaInformation thread
**Sets:** `MANGAINFO` object properties

```lua
function GetInfo()
    local u = MaybeFillHost(MODULE.RootURL, URL)

    if not HTTP.GET(u) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)
    MANGAINFO.Title     = x.XPathString('//h1')
    MANGAINFO.CoverLink = x.XPathString('//img/@src')
    MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres"]/a')

    -- Fetch chapters
    x.XPathHREFAll('//ul[@class="chapters"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    -- Sites usually list the newest chapter first; reverse both lists
    MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

    return no_error
end
```

**Workflow:**
1. User clicks on manga or refreshes info
2. FMD2 calls `OnGetInfo`
3. Module fills `MANGAINFO` properties:
   - `Title`, `AltTitles`, `CoverLink`
   - `Authors`, `Artists`, `Genres`, `Status`, `Summary`
   - `ChapterLinks`, `ChapterNames` (TStringList)
4. FMD2 displays manga information
5. Chapters are shown in chapter list

---

#### 7. Task Start (`OnTaskStart`)

**When:** Before downloading a chapter (optional)
**Called by:** TTaskContainer

```lua
function TaskStart()
    -- Initialize task, set custom options
    TASK.PageNumber = 10  -- Set expected page count
    return true
end
```

**Workflow:**
1. User starts downloading a chapter
2. FMD2 calls `OnTaskStart` (if defined)
3. Module can set initial values or perform setup
4. If returns `false`, download is cancelled

---

#### 8. Get Page Number (`OnGetPageNumber`)

**When:** Fetching chapter pages (image URLs)
**Called by:** TTaskThread
**Sets:** `TASK.PageLinks` or `TASK.PageContainerLinks`

```lua
function GetPageNumber()
    local u = MaybeFillHost(MODULE.RootURL, URL)

    if not HTTP.GET(u) then return false end

    local x = CreateTXQuery(HTTP.Document)

    -- For single-page chapters (all images on one page)
    x.XPathStringAll('//div[@class="reader"]/img/@src', TASK.PageLinks)

    -- OR for multi-page chapters (each image on separate page)
    -- x.XPathHREFAll('//div[@class="pages"]/a', TASK.PageContainerLinks)

    return true
end
```

**Workflow:**
1. FMD2 starts downloading a chapter
2. Calls `OnGetPageNumber`
3. Module has two options:
   - **Single-page:** Add all image URLs to `TASK.PageLinks`
   - **Multi-page:** Add page URLs to `TASK.PageContainerLinks`, then `OnGetImageURL` will be called for each
4. Can set `TASK.PageNumber` for progress tracking
5. Can set `TASK.FileNames` for custom filenames

---

#### 9. Get Image URL (`OnGetImageURL`)

**When:** Extracting image URL from chapter page (for multi-page chapters)
**Called by:** TDownloadThread (for each page in `TASK.PageContainerLinks`)
**Sets:** `TASK.PageLinks[WORKID]`

```lua
function GetImageURL()
    if not HTTP.GET(URL) then return false end
    local x = CreateTXQuery(HTTP.Document)
    local img_url = x.XPathString('//img[@id="page"]/@src')
    TASK.PageLinks[WORKID] = MaybeFillHost(MODULE.RootURL, img_url)
    return true
end
```

**Workflow:**
1. Only called if `TASK.PageContainerLinks` has entries
2. FMD2 calls `OnGetImageURL` for each page
3. `WORKID` global contains current work index
4. Module must set `TASK.PageLinks[WORKID]` to image URL
5. FMD2 then downloads the image

---

#### 10. Before Download Image (`OnBeforeDownloadImage`)

**When:** Before downloading each image
**Called by:** TDownloadThread
**Purpose:** set request headers/cookies before the image download

```lua
function BeforeDownloadImage()
    -- Set required headers
    HTTP.Headers.Values['Origin'] = MODULE.RootURL
    HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

    return true
end
```

**Workflow:**
1. FMD2 is about to download an image
2. Calls `OnBeforeDownloadImage`
3. `WORKID` global contains image index
4. Module can set headers and cookies; to change the URL itself, rewrite `TASK.PageLinks[WORKID]` here (a later retry picks it up — the current download still uses the old URL), or handle the download in `OnDownloadImage`
5. If returns `false`, image download is skipped

---

#### 11. Download Image (`OnDownloadImage`)

**When:** Custom image download logic (optional, replaces default)
**Called by:** TDownloadThread

```lua
function DownloadImage()
    -- Custom download logic (e.g., decrypt, decode, etc.)
    if not HTTP.GET(URL) then return false end
    -- Image data is in HTTP.Document
    return true
end
```

**Workflow:**
1. Only called if `OnDownloadImage` is defined
2. Replaces default image download behavior
3. `WORKID` global contains image index
4. Module handles downloading image data
5. Image data should be in `HTTP.Document` after GET

---

#### 12. Save Image (`OnSaveImage`)

**When:** Custom image save logic (optional)
**Called by:** TDownloadThread

```lua
function SaveImage()
    -- Custom filename
    local ext = '.jpg'
    local filename = PATH .. '/' .. string.format('%03d', WORKID) .. ext
    return filename
end
```

**Workflow:**
1. Only called if `OnSaveImage` is defined
2. `PATH` global contains target directory
3. `FILENAME` global contains default filename
4. Module returns custom filename
5. FMD2 saves image with returned filename

---

#### 13. After Image Saved (`OnAfterImageSaved`)

**When:** After image is saved to disk
**Called by:** TDownloadThread

```lua
function AfterImageSaved()
    -- Post-processing (e.g., remove watermark)
    require('fmd.mangafoxwatermark').RemoveWatermark(FILENAME, true)
    return true
end
```

**Workflow:**
1. Image has been saved to disk
2. FMD2 calls `OnAfterImageSaved`
3. `FILENAME` global contains saved file path
4. Module can process the saved file
5. Common use: watermark removal

---

#### 14. Login (`OnLogin`)

**When:** Authenticating user account
**Called by:** THTTPSendThread (when account is enabled)
**Sets:** `MODULE.Account.Status`

```lua
function Login()
    if not MODULE.Account.Enabled then
        return false
    end

    local crypto = require 'fmd.crypto'
    local data = 'username=' .. crypto.EncodeURLElement(MODULE.Account.Username) ..
                 '&password=' .. crypto.EncodeURLElement(MODULE.Account.Password)

    MODULE.Account.Status = asChecking

    if HTTP.POST(MODULE.RootURL .. '/login', data) then
        if HTTP.ResultCode == 200 then
            MODULE.Account.Status = asValid
            return true
        else
            MODULE.Account.Status = asInvalid
            return false
        end
    end

    MODULE.Account.Status = asUnknown
    return false
end
```

**Workflow:**
1. User enables account in FMD2 settings
2. FMD2 calls `OnLogin` when needed
3. Module should:
   - Check `MODULE.Account.Enabled`
   - Access `MODULE.Account.Username` and `.Password`
   - Set `MODULE.Account.Status = asChecking`
   - Perform login request
   - Set status to `asValid`, `asInvalid`, or `asUnknown`
4. Return `true` if login successful

---

#### 15. Account State (`OnAccountState`)

**When:** Checking account status (optional)
**Called by:** FMD2 account verification

```lua
function AccountState()
    -- Verify account is still logged in
    -- Note: HTTP is NOT injected in this function
    -- Use MODULE.Storage or other logic if needed
    return MODULE.Account.Enabled
end
```

**Workflow:**
1. FMD2 periodically checks account status
2. Calls `OnAccountState` (if defined)
3. Module should return `true` if account is still valid
4. Use this for simple state checks only

---

#### 16. Check Site (`OnCheckSite`)

**When:** Checking if website is accessible (optional)
**Called by:** FMD2 site availability check

```lua
function CheckSite()
    -- Check if site is accessible
    -- Note: HTTP is NOT injected in this function
    -- Return true if module should be considered active
    return true
end
```

**Workflow:**
1. FMD2 checks if website is online
2. Calls `OnCheckSite` (if defined)
3. Module should return `true` if site should be considered active
4. Use this for module-specific state checks only

---

### Complete Event Flow Example

**Scenario: User updates manga list and downloads a chapter**

```
1. Init()                              -- Module loads at startup
2. OnBeforeUpdateList()                -- Optional: prepare for update
3. OnGetDirectoryPageNumber()          -- Get total pages (sets PAGENUMBER)
4. OnGetNameAndLink()                  -- Called for each page (fills LINKS/NAMES)
   - Page 0: GetNameAndLink()
   - Page 1: GetNameAndLink()
   - ...
   - Page N: GetNameAndLink()
5. OnAfterUpdateList()                 -- Optional: cleanup

-- User clicks on manga to view info
6. OnGetInfo()                         -- Fetch manga details (fills MANGAINFO)

-- User double-clicks chapter to download
7. OnTaskStart()                       -- Optional: initialize task
8. OnGetPageNumber()                   -- Get chapter pages (fills TASK.PageLinks)

-- For each image in chapter:
9. OnBeforeDownloadImage()             -- Set headers, modify URL if needed
10. OnDownloadImage()                  -- Optional: custom download logic
    -- OR default download if not defined
11. OnSaveImage()                      -- Optional: custom filename
12. OnAfterImageSaved()                -- Post-process saved image

-- Repeat 9-12 for each image
```

**Scenario: User has account enabled**

```
1. Init()
2. OnLogin()                           -- Called when account is enabled
   - Sets MODULE.Account.Status
3. [Normal workflow continues]
4. OnAccountState()                    -- Periodically called to verify account
```

---

### MANGAINFO Object (TMangaInfo)

Manga information container.

```lua
function GetInfo()
    MANGAINFO.Title = 'My Manga'
    MANGAINFO.ChapterLinks.Add('/chapter/1')
    MANGAINFO.ChapterNames.Add('Chapter 1')
    return no_error
end
```

A full example is in [Get Info](#6-get-info-ongetinfo).

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `MANGAINFO.URL` | String | Current manga URL (absolute — the host fills it from `MODULE.RootURL` + link before `OnGetInfo` runs) |
| `MANGAINFO.Link` | String | Manga link relative to the site root — pre-filled from the manga list; if still empty after `OnGetInfo`, the host fills it from `URL` with the host part removed |
| `MANGAINFO.Title` | String | Manga title |
| `MANGAINFO.AltTitles` | String | Alternative titles |
| `MANGAINFO.CoverLink` | String | Cover image URL |
| `MANGAINFO.Authors` | String | Author(s) |
| `MANGAINFO.Artists` | String | Artist(s) |
| `MANGAINFO.Genres` | String | Genre(s) |
| `MANGAINFO.Status` | String | Publication status |
| `MANGAINFO.Summary` | String | Description/synopsis |
| `MANGAINFO.ChapterLinks` | TStringList | Chapter URLs |
| `MANGAINFO.ChapterNames` | TStringList | Chapter names |

---

### TASK Object (TTaskContainer)

Current download task information.

```lua
function GetPageNumber()
    TASK.PageLinks.Add('https://example.com/image1.jpg')
    TASK.PageLinks.Add('https://example.com/image2.jpg')
    return true
end
```

A full example is in [Get Page Number](#8-get-page-number-ongetpagenumber).

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `TASK.PageLinks` | TStringList | Image URLs for current chapter |
| `TASK.ChapterLinks` | TStringList | All chapter URLs |
| `TASK.ChapterNames` | TStringList | Chapter names |
| `TASK.PageContainerLinks` | TStringList | Chapter page URLs (multi-page chapters) |
| `TASK.FileNames` | TStringList | Custom filenames for images |
| `TASK.PageNumber` | Integer | Total page count |
| `TASK.CurrentDownloadChapterPtr` | Integer | 0-based index into `ChapterLinks` of the chapter being downloaded (useful for Referer headers) |
| `TASK.CurrentMaxFileNameLength` | Integer | Maximum file name length enforced for this task |
| `TASK.Link` | String | Current download link |

---

### LINKS and NAMES (TStringList)

Global lists for manga URLs and names.

```lua
function GetNameAndLink()
    LINKS.Add('/manga/one')
    NAMES.Add('Manga One')
    return no_error
end
```

`x.XPathHREFAll(xpath, LINKS, NAMES)` fills both lists in one call; see
[Get Name And Link](#4-get-name-and-link-ongetnameandlink).

#### Methods

| Method | Description |
|--------|-------------|
| `LINKS.Add(url)` | Add manga URL |
| `NAMES.Add(name)` | Add manga name |
| `LINKS.Count` | Get number of items |
| `LINKS.Clear()` | Clear all items |
| `LINKS.Reverse()` | Reverse list order |
| `LINKS[i]` | Access item by index (0-based) |

Same methods apply to `NAMES`. Both are `TStrings` objects, so the **full
API** (`Text`, `Values`, `IndexOf`, `Sort`, `SaveToFile`, …) from
[fmd.strings](#fmdstrings) works on them — and on `HTTP.Headers`,
`HTTP.Cookies`, `TASK.PageLinks`, `MANGAINFO.ChapterLinks`, … alike.

---

### UPDATELIST Object (TUpdateListManagerThread)

Manga list update manager.

```lua
function GetNameAndLink()
    -- Update status message
    UPDATELIST.UpdateStatusText('Loading page ' .. (URL + 1))

    -- Access current page number
    local page = UPDATELIST.CurrentDirectoryPageNumber
end
```

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `UPDATELIST.CurrentDirectoryPageNumber` | Integer (read/write) | Current page in manga list |

#### Methods

| Method | Description |
|--------|-------------|
| `UPDATELIST.UpdateStatusText(text)` | Update status message in UI |

---

### PAGENUMBER (Global Variable)

Total pages in manga list directory.

```lua
PAGENUMBER = tonumber(x.XPathString('//last-page')) or 1
```

Set it in `OnGetDirectoryPageNumber`; see
[Get Directory Page Number](#3-get-directory-page-number-ongetdirectorypagenumber).

---

### URL (Global Variable)

The current URL being processed. What it holds depends on the handler:

| Handler | `URL` contains |
|---------|----------------|
| `OnGetNameAndLink` | Current page number of the manga list (0-based) |
| `OnGetInfo` | Manga URL |
| `OnGetPageNumber` | Chapter URL |
| `OnGetImageURL` | URL of the current chapter page |
| `OnBeforeDownloadImage`, `OnDownloadImage` | Image URL (read-only in `OnBeforeDownloadImage`; the host does not read changes back) |

---

## Built-in Functions

### TXQuery (XML/HTML/JSON Parser)

Powerful XPath-based parser for HTML, XML, and JSON.

```lua
local x = CreateTXQuery(HTTP.Document)
-- or
local x = CreateTXQuery()
x.ParseHTML(html_string)
```

There are **two distinct objects** involved: the **query object** `x` itself,
and the **value/node object** returned by `x.XPath(...)` (and every item its
iterator yields). They have different methods — mixing them up is the most
common TXQuery mistake.

#### Query Object Methods

| Method | Description |
|--------|-------------|
| `x.ParseHTML(html)` | Parse HTML string or stream (replaces the previous tree) |
| `x.XPathString(xpath)` | Get single string result |
| `x.XPathString(xpath, context)` | Same, but evaluated against `context` (a value returned by `x.XPath(...)`, e.g. parsed JSON) instead of the whole document |
| `x.XPathStringAll(xpath)` | Get all matching strings (comma-separated) |
| `x.XPathStringAll(xpath, separator)` | Get all strings with custom separator |
| `x.XPathStringAll(xpath, list)` | Append all matches to a `TStrings` list (`TASK.PageLinks`, `MANGAINFO.Genres`, a `fmd.strings` list, …) |
| `x.XPathHREFAll(xpath, linksList, namesList)` | Extract href and text to lists |
| `x.XPathHREFTitleAll(xpath, linksList, namesList)` | Extract href and title attribute |
| `x.XPath(xpath)` | Get node list / value object for iteration |
| `x.XPathCount(xpath)` | Count matching nodes |

All of these methods (not just `XPathString`) also accept an optional
trailing **context** argument — a value object returned by `x.XPath(...)` —
and then evaluate the expression relative to that value instead of the whole
document; see [Parsing once, querying many times](#parsing-once-querying-many-times).

#### Value/Node Object Methods

Returned by `x.XPath(...)`; each item from an iteration is the same kind of
object.

| Member | Description |
|--------|-------------|
| `v.Count` | Number of items in the sequence |
| `v.Get(i)` | Get item at index (1-based) |
| `v.Get()` | Get iterator — `for v in x.XPath(...).Get() do ... end` |
| `v.GetProperty(name)` | Get JSON property value (on JSON objects) |
| `v.GetAttribute(name)` | Get HTML attribute value (on element nodes) |
| `v.InnerHTML()` | Inner HTML of the node |
| `v.OuterHTML()` | Outer HTML of the node |
| `v.InnerText()` | Text content of the node |
| `v.ToString()` | String conversion |

Full pipeline example (from `lua/modules/HitomiLa.lua`):

```lua
for image in CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString())).XPath('json(substring-after(., "var galleryinfo = ")).files()').Get() do
    local hash = image.GetProperty('hash').ToString()
    ...
end
```

#### XPath Extensions

FMD2 uses the latest [internettools](https://github.com/benibela/internettools) as its query engine.
It supports **XPath 3.1 and XQuery 3.1**, so everything from those standards works (maps, arrays, the `?` lookup operator,
`let`/`for` expressions, `string-join`, `tokenize`, `matches`, `replace`, `parse-json`, and so on).
On top of that, internettools adds its own shortcuts, mainly for JSON.

##### Reading JSON

There are three ways to read JSON that have been tested in FMD2. They all
reach the same data, so pick whichever you find easiest to read.

| Style | Example | Notes |
|-------|---------|-------|
| `json(*)` with dot syntax | `json(*).data.items()` | internettools shortcut. `.name` reads a property, `.items()` returns the array's members as a sequence. |
| `json(*)` with `?` lookup | `json(*)?data?items?*` | Standard XPath 3.1 syntax. `?name` reads a key, `?*` returns every member of an array or map. |
| `parse-json(.)` | `parse-json(.)?data?items?*` | Standard XPath 3.1. Parses the current text node, then uses the same `?` lookup. |

All three examples above return the items of `data.items` in a JSON response
like this:

```json
{ "data": { "items": [ { "name": "A" }, { "name": "B" } ] } }
```

What the pieces mean:

| Piece | Meaning |
|-------|---------|
| `json(*)` | Parse the page content as JSON. `*` is the document's root element, because `CreateTXQuery` wraps the response as HTML. |
| `parse-json(.)` | Same idea, but written in standard XPath 3.1 form. `.` is the current context item. |
| `.name` | (dot style) Get the property `name`. |
| `.items()` | (dot style) Get all members of the array `items`. |
| `?name` | (lookup style) Get the key `name`. |
| `?*` | (lookup style) Get all members of the array or map. |

###### JSON examples

```lua
local x = CreateTXQuery(HTTP.Document)

-- One value
local title = x.XPathString('json(*)?data?title')

-- All values of a field into a list (lookup style)
x.XPathStringAll('json(*)?data?items?*?name', MANGAINFO.Genres)

-- Same thing, dot style
x.XPathStringAll('json(*).data.items().name', MANGAINFO.Genres)

-- Using parse-json
local title2 = x.XPathString('parse-json(.)?data?title')

-- Combine with string functions
local names = x.XPathString('string-join(json(*)?data?items?*?name, ", ")')
```

Iterating over a JSON array from Lua:

```lua
local x = CreateTXQuery(HTTP.Document)
for v in x.XPath('json(*)?data?items?*').Get() do
    local name = v.GetProperty('name').ToString()
    local id   = v.GetProperty('id').ToString()
    -- ...
end

-- Equivalent, dot style
for v in x.XPath('json(*).data.items()').Get() do
    local name = v.GetProperty('name').ToString()
end
```

> **Tip:** if the JSON contains `<`, `&` and similar characters and
> `json(*)` fails, escape the text first with `crypto.HTMLEncode` (see
> [The `HTMLEncode` + TXQuery trick](#fmdcrypto)).

##### Parsing once, querying many times

Every `x.XPathString(...)` call that starts with `json(*)` parses the JSON
again. If you need several values, parse once with `x.XPath('json(*)')` and
pass the result as the second argument. The query is then evaluated **relative
to that value**, so you leave out `json(*)` and start at the key name.

```lua
local x = CreateTXQuery(HTTP.Document)

-- One call: parse and read together
local s = x.XPathString('json(*).data.field')

-- Two steps: parse once, then reuse the result
local json = x.XPath('json(*)')
local s1 = x.XPathString('data?field', json)
local s2 = x.XPathString('data?other', json)
```

Both ways give the same result for `s`. In the two-step form, move down
into the JSON with `?` or `/` (for example `data?field` or `data/field`).

##### Standard string functions

These are normal XPath functions and work on HTML and JSON alike:

```lua
-- Join multiple strings
x.XPathString('string-join(//title, ", ")')

-- Substring operations
x.XPathString('substring-before(//text, "delimiter")')
x.XPathString('substring-after(//text, "prefix")')

-- Parse JSON that is embedded in a script or a text node
x.XPath('json(substring-after(., "var galleryinfo = "))')
```

##### JSON navigation functions (`jn:`)

The older JSONiq-style helpers from internettools can also be used. They are
less common than the lookup styles above.

```lua
jn:keys($obj)      -- Get JSON object keys
jn:members($arr)   -- Get JSON array members

local count = x.XPathCount('jn:keys($obj)')
for v in x.XPath('jn:members($arr)').Get() do ... end
```

### Utility Functions

#### String Manipulation

```lua
-- Get text between delimiters
local text = GetBetween('<div>', '</div>', html)

-- Split string
local left = SeparateLeft('a,b,c', ',')  -- returns 'a'
local right = SeparateRight('a,b,c', ',') -- returns 'b,c'

-- Trim whitespace
local trimmed = Trim('  hello  ')  -- returns 'hello'

-- URL manipulation
local full_url = MaybeFillHost('https://example.com', '/page')
-- returns 'https://example.com/page'
```

| Function | Description |
|----------|-------------|
| `GetBetween(left, right, text)` | Extract text between delimiters |
| `SeparateLeft(text, delimiter)` | Get left part of split string |
| `SeparateRight(text, delimiter)` | Get right part of split string |
| `Trim(text[, chars])` | Remove whitespace (or the given characters) |
| `MaybeFillHost(rootURL, url)` | Ensure URL has full host prefix |

#### Status Helper

```lua
-- Convert status text to standard format
MANGAINFO.Status = MangaInfoStatusIfPos(
    status_text,
    'Ongoing|Releasing',      -- Ongoing keywords
    'Completed|Finished',     -- Completed keywords
    'Hiatus|On Hold',         -- Hiatus keywords
    'Canceled|Dropped'        -- Dropped keywords
)
```

The keyword lists are optional — `MangaInfoStatusIfPos` accepts 1 to 5
arguments (later lists default to none).

#### Other Functions

```lua
sleep(1000)  -- Pause for 1000ms
print('Debug message')  -- Output to log
```

---

## Lua Libraries

### fmd.* Modules (Injected by the host application)

#### fmd.crypto

Crypto, hashing, and encoding functions, exposed natively (Object Pascal) and
loaded with:

```lua
local crypto = require 'fmd.crypto'
```

The library merges two registration tables from `baseunits/lua/LuaCrypto.pas`
(43 methods from `BaseCrypto.pas` and 16 methods from the Synapse `synacode`
unit), so all **59 functions** below live on the same `crypto` table.

**Conventions**

- Lua strings are 8-bit clean — raw binary digests, keys, IVs and ciphertexts
  can be stored in ordinary Lua strings. Use `StrToHexStr` to *inspect* binary
  values with `print()` (raw bytes are displayed as garbage).
- **`...Hex` functions return lowercase** hex; `StrToHexStr` returns
  **UPPERCASE** hex; `HexToStr` accepts both. Compare with `:lower()` /
  `:upper()` as needed.
- Crypto functions return an **empty string `''`** on invalid input (empty
  argument, wrong key length, failed Base64/hex validation, failed
  authentication) instead of raising — check `if result == '' then` when the
  input can be malformed. The one partial exception:
  `AESDecryptCBCHexBase64ZerosPadding` passes its hex key/IV through an
  internal converter that expects only hex characters — non-hex input there
  can raise an internal error, so validate scraped values first.

**Quick reference (59 functions)**

| Category | Functions |
|----------|-----------|
| Hashing | `MD4` `MD5` `MD5Hex` `MD5LongHash` `SHA1` `SHA1Hex` `SHA1LongHash` `SHA256` `SHA256Hex` `SHA512` `SHA512Hex` |
| HMAC | `HMAC_MD5` `HMAC_MD5Hex` `HMAC_SHA1` `HMAC_SHA1Hex` `HMAC_SHA256` `HMAC_SHA256Hex` `HMAC_SHA512` `HMAC_SHA512Hex` |
| AES (raw modes) | `AESEncryptCBC` `AESDecryptCBC` `AESEncryptECBPkcs7` `AESDecryptECBPkcs7` `AESEncryptCFB` `AESDecryptCFB` `AESCTR` `AESOFB` `AESEncryptGCM` `AESDecryptGCM` |
| AES (composite) | `AESEncryptCBCSHA256Base64Pkcs7` `AESDecryptCBCSHA256Base64Pkcs7` `AESDecryptCBCMD5Base64ZerosPadding` `AESDecryptCBCHexBase64ZerosPadding` |
| Other ciphers | `RC4` |
| KDF / key exchange | `PBKDF2SHA256` `X25519_PublicKey` `X25519_SharedSecret` `P256_GenerateKeyPair` `P256_PublicKey` `P256_SharedSecret` |
| Authenticated streaming | `SecretStream_InitPull` `SecretStream_Pull` |
| Encoding / decoding | `EncodeBase64` `DecodeBase64` `EncodeBase64URL` `DecodeBase64URL` `EncodeURL` `DecodeURL` `EncodeURLElement` `HTMLEncode` `HTMLDecode` `EncodeUU` `DecodeUU` `HexToStr` `StrToHexStr` |
| Checksums | `CRC16` `CRC32` |
| FMD2-internal | `EncryptString` `DecryptString` |

##### Hash functions

With the exception of the `...Hex` variants, all hash functions return the
**raw binary digest** as a Lua string (16 bytes for MD4/MD5, 20 for SHA-1,
32 for SHA-256, 64 for SHA-512). Hash values you want to compare against the
site's JavaScript should be produced with the `Hex` variants or converted
with `StrToHexStr`.

| Function | Signature | Returns |
|----------|-----------|---------|
| `MD4` | `MD4(s)` | raw 16-byte digest (legacy hash, some old image hosts) |
| `MD5` | `MD5(s)` | raw 16-byte digest |
| `MD5Hex` | `MD5Hex(s)` | 32-char **lowercase** hex |
| `SHA1` | `SHA1(s)` | raw 20-byte digest |
| `SHA1Hex` | `SHA1Hex(s)` | 40-char **lowercase** hex |
| `SHA256` | `SHA256(s)` | raw 32-byte digest |
| `SHA256Hex` | `SHA256Hex(s)` | 64-char **lowercase** hex |
| `SHA512` | `SHA512(s)` | raw 64-byte digest |
| `SHA512Hex` | `SHA512Hex(s)` | 128-char **lowercase** hex |
| `MD5LongHash` | `MD5LongHash(s, len)` | raw 16 bytes; MD5 of `s` repeated to exactly `len` bytes |
| `SHA1LongHash` | `SHA1LongHash(s, len)` | raw 20 bytes; SHA-1 of `s` repeated to exactly `len` bytes |

The `LongHash` variants are legacy helpers used by a few image hosts: the
input string is concatenated with itself until it reaches exactly `len`
bytes (a partial copy at the end), and that extended string is hashed.

```lua
local crypto = require 'fmd.crypto'

print(crypto.MD5Hex('data'))     -- 8d777f385d3dfec8815d20f7496026dc
print(crypto.SHA1Hex('data'))    -- a17c9aaa61e80a1bf71d0d850af4e5baa9800bbd
print(crypto.SHA256Hex('data'))  -- 3a6eb0790f39ac87c94f3856b2dd2c5d110e6811602261a9a923d3bb23adc8b7

-- raw digest -> hex by hand (StrToHexStr is UPPERCASE!)
print(crypto.StrToHexStr(crypto.MD5('data')))          -- 8D777F385D3DFEC8815D20F7496026DC
print(crypto.StrToHexStr(crypto.MD5('data')):lower())  -- 8d777f385d3dfec8815d20f7496026dc

-- MD5LongHash('abc', 10) == MD5('abcabcabca')
print(crypto.StrToHexStr(crypto.MD5LongHash('abc', 10)):lower())
-- 4ec647ce99227abb788106c127320f96
```

> [!NOTE]
> The hash functions hash the empty string normally
> (e.g. `MD5Hex('')` → `d41d8cd98f00b204e9800998ecf8427e`, `SHA1Hex('')` →
> `da39a3ee5e6b4b0d3255bfef95601890afd80709`), so an empty result means the
> argument itself was empty or invalid, never "hash of nothing".

##### HMAC functions

All HMAC functions take the arguments in the order **`(data, key)`** — data
first, key second. The non-`Hex` variants return the **raw binary** MAC;
`Hex` variants return lowercase hex. HMAC keys longer than the block size
are hashed first (per RFC 2104); keys of any other length work as-is.

| Function | Signature | Returns |
|----------|-----------|---------|
| `HMAC_MD5` | `HMAC_MD5(data, key)` | raw 16 bytes |
| `HMAC_MD5Hex` | `HMAC_MD5Hex(data, key)` | 32-char lowercase hex |
| `HMAC_SHA1` | `HMAC_SHA1(data, key)` | raw 20 bytes |
| `HMAC_SHA1Hex` | `HMAC_SHA1Hex(data, key)` | 40-char lowercase hex |
| `HMAC_SHA256` | `HMAC_SHA256(data, key)` | raw 32 bytes |
| `HMAC_SHA256Hex` | `HMAC_SHA256Hex(data, key)` | 64-char lowercase hex |
| `HMAC_SHA512` | `HMAC_SHA512(data, key)` | raw 64 bytes |
| `HMAC_SHA512Hex` | `HMAC_SHA512Hex(data, key)` | 128-char lowercase hex |

```lua
local crypto = require 'fmd.crypto'

print(crypto.HMAC_SHA256Hex('data', 'key'))
-- 5031fe3d989c6d1537a013fa6e739da23463fdaec3b70137d828e36ace221bd0

print(crypto.HMAC_MD5Hex('data', 'key'))
-- 9d5c73ef85594d34ec4438b7c97e51d8
```

##### AES — raw modes (CBC, ECB, CTR, CFB, OFB, GCM)

These are the low-level primitives. **Key and IV are raw byte strings**, and
the **key length selects the AES variant**: 16 bytes → AES-128, 24 bytes →
AES-192, 32 bytes → AES-256. Any other key length makes the function return
`''`. If the site's JavaScript carries hex strings, convert them first:

```lua
local key = crypto.HexToStr('000102030405060708090a0b0c0d0e0f')  -- 16 raw bytes -> AES-128
local iv  = crypto.HexToStr('101112131415161718191a1b1c1d1e1f')  -- 16 raw bytes
```

The IV for CBC/CTR/CFB/OFB is also raw bytes: if it is shorter than 16
bytes it is padded with zeros at the end; if longer than 16 bytes it is
truncated to 16.

| Function | Signature | Notes |
|----------|-----------|-------|
| `AESEncryptCBC` | `AESEncryptCBC(s, key, iv)` | Raw in/out. Standard CBC on full 16-byte blocks. |
| `AESDecryptCBC` | `AESDecryptCBC(s, key, iv)` | Inverse of `AESEncryptCBC`. |
| `AESEncryptECBPkcs7` | `AESEncryptECBPkcs7(s, key)` | ECB + PKCS7; any plaintext length (padded to a 16-byte multiple, all blocks encrypted). |
| `AESDecryptECBPkcs7` | `AESDecryptECBPkcs7(s, key)` | Inverse; ciphertext length must be a multiple of 16 (otherwise `''`). |
| `AESCTR` | `AESCTR(s, key, iv)` | CTR keystream XOR; symmetric (the same call encrypts and decrypts); IV = initial 128-bit counter, big-endian increment of the whole block. |
| `AESEncryptCFB` | `AESEncryptCFB(s, key, iv)` | CFB-128 (ciphertext feedback), full multi-block support, any input length. |
| `AESDecryptCFB` | `AESDecryptCFB(s, key, iv)` | Inverse of `AESEncryptCFB`. |
| `AESOFB` | `AESOFB(s, key, iv)` | OFB keystream XOR; symmetric, any length. |
| `AESEncryptGCM` | `AESEncryptGCM(s, key, iv[, aad])` | Returns `ciphertext ‖ 16-byte tag` concatenated. |
| `AESDecryptGCM` | `AESDecryptGCM(s, key, iv[, aad])` | Input `ciphertext ‖ tag`; verifies the tag (constant-time) and returns plaintext, or `''` if the tag or length is wrong. |

> [!NOTE]
> - **CBC and non-block-aligned input:** the underlying DCPcrypt processes
>   full 16-byte blocks in standard CBC, then handles a trailing partial block
>   by XOR-ing it with the encrypted chaining value. Round-trips inside FMD2
>   work for any length, but the partial-block tail is *not* standard CBC and
>   will not match CryptoJS/OpenSSL output. For cross-platform interop, feed
>   CBC only block-aligned data (site ciphertext normally is).
> - **CTR compatibility:** the whole 128-bit counter block is incremented
>   big-endian, matching CryptoJS `mode.CTR`. (GCM internally uses the NIST
>   32-bit counter — different, but that is internal and standard.)
> - **GCM IV:** 12 raw bytes is the standard/recommended size; other IV
>   lengths are supported through the GHASH path. The optional 4th `aad`
>   argument is additional authenticated data — it is authenticated but not
>   encrypted, and must be passed identically to both calls.

```lua
local crypto = require 'fmd.crypto'

-- CBC round-trip with raw 16-byte key/IV
local key = '0123456789abcdef'          -- 16 bytes -> AES-128
local iv  = 'abcdef9876543210'
local pt  = 'sixteen-byte!!'            -- exactly 16 bytes
local ct  = crypto.AESEncryptCBC(pt, key, iv)
print(crypto.AESDecryptCBC(ct, key, iv) == pt)      -- true

-- The typical "decrypt what the page gave us" pattern
local b64 = 'bXlzdGVyeWRhdGE='
local ct2 = crypto.DecodeBase64(b64)                -- decode FIRST
-- local key2 = crypto.HexToStr(site_key_hex)       -- hex keys likewise
-- local pt2  = crypto.AESDecryptCBC(ct2, key2, iv2)

-- GCM round-trip
local gkey = '0123456789abcdef'
local giv  = '000000000002'              -- 12 raw bytes
local blob = crypto.AESEncryptGCM('secret', gkey, giv)      -- ciphertext+tag
print(crypto.AESDecryptGCM(blob, gkey, giv))                -- secret
print(#blob)                                               -- 6 + 16 tag = 22
```

##### AES — composite decryptors (Base64 in, site-friendly)

These wrap CBC with the exact key-derivation, IV format, transport encoding
and padding combinations used by common website JavaScript. Pick the one that
matches the site's JS — the names encode the recipe.

| Function | Signature | Key | IV | Input | Padding |
|----------|-----------|-----|----|-------|---------|
| `AESEncryptCBCSHA256Base64Pkcs7` | `(s, key, iv)` | passphrase, internally SHA-256-hashed → always AES-256 | **hex string** (exactly 32 hex chars = 16 bytes) | plaintext (encrypt) | PKCS7, Base64 out |
| `AESDecryptCBCSHA256Base64Pkcs7` | `(s, key, iv)` | same derivation | same hex IV | **Base64** ciphertext | PKCS7 stripped |
| `AESDecryptCBCMD5Base64ZerosPadding` | `(s, key, iv)` | passphrase → `MD5Hex(key)` (32-char *hex text*) used as raw key bytes → AES-256 | **raw** bytes (zero-padded to 16) | **Base64** ciphertext | trailing NUL bytes stripped |
| `AESDecryptCBCHexBase64ZerosPadding` | `(s, key, iv)` | **hex string** (32/48/64 hex chars → AES-128/192/256) | **hex string** (exactly 32 hex chars) | **Base64** ciphertext | trailing NUL bytes stripped |

```lua
local crypto = require 'fmd.crypto'

-- matches CryptoJS-style "AES.encrypt(text, passphrase)" with fixed IV
local key = 'my site passphrase'
local ivhex = '000102030405060708090a0b0c0d0e0f'   -- 32 hex chars = 16 bytes
local b64 = crypto.AESEncryptCBCSHA256Base64Pkcs7('hello', key, ivhex)
print(crypto.AESDecryptCBCSHA256Base64Pkcs7(b64, key, ivhex))  -- hello

-- hex-key variant (key + IV as hex, Base64 payload)
-- local pt = crypto.AESDecryptCBCHexBase64ZerosPadding(b64ct, keyhex, ivhex)
```

> [!NOTE]
> The two *ZerosPadding* functions are **decrypt-only** (there are no encrypt
> counterparts). All four functions return `''` on empty/malformed input; the
> hex-IV functions require an IV of exactly 32 hex characters. For
> `AESDecryptCBCHexBase64ZerosPadding` the key and IV must consist of hex
> characters only — validate scraped values (`key:match('^%x+$')`) before
> calling it, since non-hex input is not caught as a normal `''` return.

##### RC4

`RC4(data, key)` — note the order: **data first, key second**. RC4 is a
symmetric stream cipher, so the same call encrypts and decrypts. Key length
must be 1–256 bytes (longer keys return `''`); `data` of any length works.
The output is raw binary — hex/base64 it yourself if you need text.

```lua
local crypto = require 'fmd.crypto'
local ct = crypto.RC4('secret data', 'key123')
print(crypto.RC4(ct, 'key123') == 'secret data')   -- true
```

##### Key derivation and key exchange

**`PBKDF2SHA256(password, salt, iterations, dkLen)`** — standard
PBKDF2-HMAC-SHA256 (RFC 2898/8018). `iterations` and `dkLen` are integers.
Returns the derived key as **raw binary** (`dkLen` bytes); hex it with
`StrToHexStr` when the site expects text. Returns `''` if `password` or
`salt` is empty or the integers are < 1.

```lua
local crypto = require 'fmd.crypto'
-- matches hashlib.pbkdf2_hmac('sha256', b'data', b'salt', 1000, 16)
print(crypto.StrToHexStr(crypto.PBKDF2SHA256('data', 'salt', 1000, 16)))
-- D204735BDB5BD30638F3A45B5CDE2A84
```

**`X25519_PublicKey(privKey)`** — derives the 32-byte public key from a
32-byte raw private key.
**`X25519_SharedSecret(privKey, pubKey)`** — X25519 ECDH: combines your
32-byte raw private key with the peer's 32-byte raw public key and returns
the 32-byte raw shared secret. Both return `''` unless the inputs are
exactly 32 bytes.

**`P256_GenerateKeyPair()`** — generates a fresh P-256 (secp256r1) key pair
using OpenSSL's CSPRNG and returns two values: the 32-byte raw private key
and the 65-byte uncompressed public key (`0x04 || X || Y`). Returns no
values on failure.

```lua
local crypto = require 'fmd.crypto'
local priv, pub = crypto.P256_GenerateKeyPair()
```

**`P256_PublicKey(privKey)`** — derives the 65-byte uncompressed public key
from a 32-byte raw big-endian private scalar.
**`P256_SharedSecret(privKey, pubKey)`** — P-256 ECDH: combines your 32-byte
raw private key with the peer's 65-byte uncompressed public key and returns
the 32-byte shared secret. The peer point is validated to be on the curve
while parsing; both return `''` on malformed input.

> [!IMPORTANT]
> Keys must be raw bytes — convert hex keys with `HexToStr` first.
> The shared secret is typically hashed (e.g. `SHA256`) before use as a symmetric key.

##### SecretStream (libsodium-style authenticated stream, receive side)

`SecretStream_InitPull` / `SecretStream_Pull` implement the *decrypting*
half of libsodium's `crypto_secretstream_*` API (ChaCha20-Poly1305 based,
with automatic rekeying). There is no push/encrypt side exposed to Lua —
these are for consuming streams produced by libsodium (or compatible code).

- **`SecretStream_InitPull(header, key)`** — `header` is the 24-byte stream
  header, `key` the 32-byte raw key. Returns an opaque `state` string, or
  **`nil`** if the sizes are wrong.
- **`SecretStream_Pull(state, chunk)`** — `chunk` is one encrypted chunk
  (minimum 17 bytes: 1 encrypted tag byte + ciphertext + 16-byte MAC).
  Returns **three values** — the *updated* `state` (pass it to the next
  call!), the decrypted `message`, and the integer `tag` — or **no values**
  (all `nil`) if authentication fails.

The `tag` follows libsodium's constants:

| Tag | Value | Meaning |
|-----|-------|---------|
| `MESSAGE` | 0 | normal chunk |
| `FINAL` | 1 | last chunk of the stream |
| `PUSH` | 2 | logical boundary (end of a multi-part message) |
| `REKEY` | 3 | stream rekeyed after this chunk |

```lua
local crypto = require 'fmd.crypto'

local state = crypto.SecretStream_InitPull(header24, key32)
if not state then return false end

for _, chunk in ipairs(chunks) do
    state, msg, tag = crypto.SecretStream_Pull(state, chunk)
    if not state then return false end  -- MAC failure
    TASK.PageLinks.Add(msg)             -- or accumulate / write to disk
    if tag == 1 then break end          -- FINAL
end
```

##### Encoding and decoding

| Function | Signature | Description |
|----------|-----------|-------------|
| `EncodeBase64` | `EncodeBase64(s)` | raw → standard Base64 (`+`, `/`, `=` padding, no line breaks) |
| `DecodeBase64` | `DecodeBase64(s)` | standard Base64 → raw |
| `EncodeBase64URL` | `EncodeBase64URL(s)` | raw → URL-safe Base64 (`-`, `_`, **no padding**) |
| `DecodeBase64URL` | `DecodeBase64URL(s)` | URL-safe Base64 → raw (re-adds padding as needed) |
| `EncodeURL` | `EncodeURL(s)` | percent-encode only *unsafe* chars (controls, space, `<>"%{}\|^[]`, DEL, non-ASCII); keeps `/ ? & = #` etc. — use for **whole URLs/paths** |
| `EncodeURLElement` | `EncodeURLElement(s)` | additionally encodes `; / ? : @ = & # +` — use for **query values / form fields** |
| `DecodeURL` | `DecodeURL(s)` | `%XX` sequences → raw |
| `HTMLEncode` | `HTMLEncode(s)` | escape HTML-special characters (`<` → `&lt;`, `&` → `&amp;`, …) |
| `HTMLDecode` | `HTMLDecode(s)` | decode HTML entities (named and numeric) back to text |
| `EncodeUU` | `EncodeUU(s)` | UUencode **data only** (no `begin`/`end` lines) |
| `DecodeUU` | `DecodeUU(s)` | UUdecode data only |
| `HexToStr` | `HexToStr(h)` | hex string → raw bytes (case-insensitive); returns `''` on empty, odd-length or non-hex input |
| `StrToHexStr` | `StrToHexStr(s)` | raw bytes → **UPPERCASE** hex |

```lua
local crypto = require 'fmd.crypto'

print(crypto.EncodeBase64('data'))      -- ZGF0YQ==
print(crypto.DecodeBase64('ZGF0YQ=='))  -- data
print(crypto.EncodeBase64URL('data'))   -- ZGF0YQ

print(crypto.EncodeURLElement('hello world'))  -- hello%20world
print(crypto.DecodeURL('hello%20world'))       -- hello world

print(crypto.HTMLEncode('<script>'))           -- &lt;script&gt;
print(crypto.HTMLDecode('&lt;script&gt;'))     -- <script>

print(crypto.StrToHexStr('AB'))                -- 4142
print(crypto.HexToStr('41424344'))             -- ABCD
```

**The `HTMLEncode` + TXQuery trick** (used by the MangaDex and BiliBili
Comics modules): `CreateTXQuery` expects HTML; feeding it raw JSON can break
the parser when JSON strings contain `<`, `&`, etc. Escaping first makes the
`json(*)` XPath parser reliable:

```lua
local x = CreateTXQuery(crypto.HTMLEncode(HTTP.Document.ToString()))
local data = x.XPath('json(*)')
```

**Hex-obfuscated HTML** (NewToki pattern): some sites ship page fragments as
hex strings inside JavaScript:

```lua
x.ParseHTML(crypto.HexToStr(hex_string_from_the_page))
```

##### Checksums

| Function | Signature | Returns |
|----------|-----------|---------|
| `CRC16` | `CRC16(s)` | integer — Synapse variant (init `$FFFF`, **no final inversion**); differs from many JS `crc16` libs — verify against the site's values |
| `CRC32` | `CRC32(s)` | integer — standard CRC-32 (same as zlib/PNG) |

```lua
local crypto = require 'fmd.crypto'
print(crypto.CRC32('data'))   -- 2918445923  (0xADF3F363)
```

##### FMD2-internal string obfuscation

`EncryptString(s)` / `DecryptString(s)` — FMD2's own reversible obfuscation
(AES under an internal SHA-512-derived key, Base64-style output). It exists
for storing account passwords in FMD2's own databases; it is **not** a
general-purpose cipher and round-trips only inside FMD2. Modules almost never
need these — mentioned here only because they appear in the function list.

##### Common patterns from real modules

```lua
-- 1) Login form data (EHentai and many others)
local crypto = require 'fmd.crypto'
local data = 'username=' .. crypto.EncodeURLElement(MODULE.Account.Username) ..
             '&password=' .. crypto.EncodeURLElement(MODULE.Account.Password)

-- 2) Base64-encoded image URLs hidden in <script> (MangaTV)
for url in s:gmatch('|Ly9pbWc[^%s]+') do
    TASK.PageLinks.Add('https:' .. crypto.DecodeBase64(url .. '=='))
end

-- 3) Hex-obfuscated page fragments (NewToki)
x.ParseHTML(crypto.HexToStr(s:match("html_data%+='(.*)';")))

-- 4) AES-CBC image/link decryptors on key-and-IV sites
local key = crypto.HexToStr(key_hex)                  -- hex key from site JS
local iv  = crypto.HexToStr(iv_hex)                   -- hex IV from site JS
local pt  = crypto.AESDecryptCBC(                     -- raw decrypt
    crypto.DecodeBase64(b64_ciphertext), key, iv)

-- 5) HMAC-signed API request (conceptual)
local sign = crypto.HMAC_SHA256Hex(path .. params, secret_key)
HTTP.GET(base .. path .. '?' .. params .. '&sign=' .. sign)
```

##### Notes

- Decode **before** decrypting: `AESDecryptCBC` takes raw bytes — Base64
  first (`DecodeBase64`), hex keys/IVs first (`HexToStr`).
- Raw digests print as garbage; use the `...Hex` variants or `StrToHexStr`
  (uppercase — lowercase it for comparisons).
- Key length must be exactly 16/24/32 bytes; anything else → `''`. Hex keys
  are 32/48/64 characters.
- CBC with a non-multiple-of-16 length uses a non-standard partial tail —
  fine for internal round-trips, not for cross-library interop.
- `AESDecryptECBPkcs7` rejects ciphertext that is not a multiple of 16
  bytes (returns `''`).
- Check `''` returns — they signal malformed input, wrong key size, failed
  Base64/hex validation, or a failed GCM/SecretStream authentication, not
  "empty plaintext".
- `HexToStr` returns `''` (not an error) on invalid hex — validate scraped
  values with `s:match('^%x+$')` when you need to tell bad input apart.
- `SecretStream_Pull` returns three values — always keep the new `state`.
- `X25519_*` and `P256_*` require OpenSSL 3 on the system; keys are raw
  bytes (`X25519`: 32-byte keys, `P256`: 32-byte private key / 65-byte
  uncompressed public key).

#### fmd.duktape

JavaScript execution engine (Duktape 2.5, ES5.1+ parts of ES6).

```lua
local duktape = require 'fmd.duktape'

-- returns the value of the script's LAST expression,
-- or nil on a JS error (the error is logged)
local result = duktape.ExecJS(script)
if not result then return net_problem end

-- Common pattern for obfuscated JS
local script = x.XPathString('//script[contains(., "eval")]')
local decrypted = duktape.ExecJS(script .. ';decryptedVariable;')

-- scripts can require() the JS utilities shipped in lua/utils/:
local out = duktape.ExecJS([[
    var CryptoJS = require("utils/crypto-js.min.js");
    CryptoJS.AES.encrypt('data', 'pass').toString();
]])
```

#### fmd.env

Environment information.

```lua
local fmd = require 'fmd.env'

fmd.Directory         -- FMD2 program directory
fmd.ExeName           -- executable name
fmd.Version           -- FMD2 version string
fmd.Revision          -- revision number
fmd.LuaDirectory      -- lua scripts directory
fmd.SelectedLanguage  -- selected UI language
```

#### fmd.fileutil

File utilities.

```lua
local fileutil = require 'fmd.fileutil'

fileutil.ExtractFileName('/path/file.txt')      -- 'file.txt'
fileutil.ExtractFileNameOnly('/path/file.txt')  -- 'file'

-- takes ONE TStrings argument (NAMES, LINKS, a fmd.strings list, ...);
-- prefixes numeric-looking names so sort order is preserved on reload
fileutil.SerializeAndMaintainNames(NAMES)
```

#### fmd.logger

Logging functionality.

```lua
local logger = require 'fmd.logger'

logger.Send('Info message')
logger.SendWarning('Warning message')
logger.SendError('Error message')
```

#### fmd.subprocess

Run external commands. There is no `run` function — use:

```lua
local subprocess = require 'fmd.subprocess'

-- varargs: executable + arguments (all strings);
-- returns four values: ok, stdout, stderr, exitcode
local ok, out, err, code = subprocess.RunCommandHide('node', 'script.js', '--opt', 'value')
if not ok then print(err) end
```

`RunCommand` shows the console window while running; `RunCommandHide`
doesn't (use it for helper executables on Windows so no console flashes).
`ok` is `false` when the process exits with a non-zero status.
(`subprocess.New()` / `subprocess.Create()` also exist — a raw `TProcess`
object with no bound methods; rarely useful from Lua.)

#### fmd.mangafoxwatermark

Watermark removal for FanFox/MangaHere.

```lua
local mf = require 'fmd.mangafoxwatermark'

function AfterImageSaved()
    mf.RemoveWatermark(FILENAME, true)  -- true = re-save as PNG
    return true
end

mf.LoadTemplate(filename)               -- optional: load an alternate template
```

#### fmd.gzip

GZip/zlib decompression. One function:

**`gzip.Inflate(data)`** — decompresses a binary string and returns the
decompressed string, or **`nil`** if the data is not valid deflate/gzip/zlib
(detection is automatic by magic header; gzip members with filename/comment
extra fields are handled). Errors are logged, not raised.

```lua
local gzip = require 'fmd.gzip'

local raw = gzip.Inflate(compressed_data)
if not raw then return net_problem end
```

Real usage — `lua/modules/Happymh.lua` (the site returns an encrypted gzip
blob with a 4-byte `SC01` magic; after the module's custom decryption it
inflates the remainder):

```lua
local plain = table.concat(out)                  -- decrypted bytes
if plain:sub(1, 4) ~= 'SC01' then error('Decrypting scans failed') end
return require 'fmd.gzip'.Inflate(plain:sub(5))
```

There is no deflate/compress counterpart — modules only ever *decompress*.

#### fmd.pcre2

Full **PCRE2** regular expressions (8-bit), compiled with the `PCRE2_UTF`
flag. Use it whenever a site's regex needs look-arounds, backreferences,
non-greedy control, possessive quantifiers, or Unicode classes that Lua
patterns don't have.

All functions take the **subject first, pattern second** (like Lua's
`string.find`, not like `re.exec` in JS):

| Function | Signature | Returns |
|----------|-----------|---------|
| `exec` | `re.exec(subject, pattern[, init])` | `true`/`false` — does the pattern match anywhere |
| `find` | `re.find(subject, pattern[, init])` | `start, end` (1-based, inclusive, like `string.find`) or nothing when no match |
| `match` | `re.match(subject, pattern[, init])` | with capture groups: all groups; without: the whole match; nothing when no match (like `string.match`) |
| `gmatch` | `re.gmatch(subject, pattern[, init])` | iterator over successive matches (like `string.gmatch`) |
| `gsub` | `re.gsub(subject, pattern, replacement[, init])` | string with **all** matches replaced (global) |

`init` is an optional 1-based start offset. Patterns are compiled fresh on
every call — for hot loops prefer one `gmatch` over repeated `match` calls.

**Replacement syntax is PCRE2, not Lua:** use `$1`, `$2`, `${name}`, `$0`
(whole match), `$$` (a literal `$`). Lua's `%1` does **not** work here. (The
`PCRE2_SUBSTITUTE_EXTENDED` option is not enabled, so `\n`-style escapes are
not interpreted in the replacement.)

```lua
local re = require 'fmd.pcre2'

-- lookbehind — impossible with Lua patterns
if re.exec(s, '(?<=//cdn)[\\w./-]+\\.(?:jpg|png)') then ... end

-- capture groups, find positions
local start, fin = re.find(s, 'chapter[_-]?(\\d+)')
local num = re.match(s, 'chapter[_-]?(\\d+)')          -- group 1 only

-- replace all, PCRE2 syntax
local clean = re.gsub(s, '\\s{2,}', ' ')               -- collapse whitespace

-- iteration
for url in re.gmatch(s, 'https?://[^"\\s]+') do
    TASK.PageLinks.Add(url)
end
```

> [!WARNING]
> - A pattern that can match the **empty string** makes `gmatch` loop forever
>   (the iterator does not advance past zero-width matches, unlike Lua's
>   `string.gmatch`).
> - Breaking out of a `gmatch` loop early leaks the internal engine object —
>   avoid `break` in long loops over huge subjects, or let the loop run out.
> - A compile error logs a message and makes the call fail *softly*: `exec`
>   returns `false`, `find`/`match` return nothing, `gsub` returns the subject
>   unchanged, `gmatch` returns an iterator that yields nothing.
> - The engine holds a pointer to the subject string — keep the subject alive
>   (a local variable) for the whole iteration.

#### fmd.imagepuzzle

Native image tile **descrambler** for sites that cut pages into a grid and
shuffle the tiles (plus optional flips). It decodes the image (JPG/PNG/WebP
are handled natively), rearranges the blocks, and writes the fixed image —
no external tool needed.

**Constructor:**

```lua
local imagepuzzle = require 'fmd.imagepuzzle'
local puzzle = imagepuzzle.Create(hblocks, vblocks)   -- or .New(...)
```

`hblocks`/`vblocks` are the grid size (columns × rows of tiles). New puzzles
start **unscrambled** — `Matrix[i] = i`, `Flips[i] = 0`.

**Members:**

| Member | Access | Description |
|--------|--------|-------------|
| `puzzle.HorBlock` | read | number of horizontal blocks (columns) |
| `puzzle.VerBlock` | read | number of vertical blocks (rows) |
| `puzzle.Matrix[i]` | read/write | destination tile index for source tile `i` — **0-based**, range `0 .. hblocks*vblocks-1` |
| `puzzle.Flips[i]` | read/write | flip flags for tile `i` (0 = none) — 0-based |
| `puzzle.Multiply` | read/write | scaling multiplier, default 1 (used when the scrambled image is scaled up/down) |
| `puzzle:DeScramble(input, output)` | method | descrambles a `TStream` into another `TStream` (`input` = scrambled, `output` = fixed) |

`HTTP.Document` is a `TMemoryStream`, so the usual pattern is to descramble
**in place** during `OnDownloadImage`.

Real usage — `lua/modules/Comix.lua` (the site sends an `X-Scramble-Grid`
response header and a seed; the module computes the permutation matrix with
a small LCG/xorshift in Lua, then fixes the image):

```lua
function DownloadImage()
    if not HTTP.GET(URL) then return false end
    local grid_header = HTTP.Headers.Values['X-Scramble-Grid']
    local cols, rows = ParseGrid(grid_header)
    local grid_size = cols * rows

    local puzzle = require 'fmd.imagepuzzle'.Create(cols, rows)
    local matrix = GetPermutationMatrixLcg(seed, grid_size)   -- module-side math

    for src_idx = 0, grid_size - 1 do          -- 0-based!
        puzzle.Matrix[src_idx] = matrix[src_idx]
    end

    puzzle.DeScramble(HTTP.Document, HTTP.Document)  -- in place
    return true
end
```

> [!NOTE]
> - `Matrix` mapping semantics: `Matrix[src] = dst` — source tile `src` is
>   moved to position `dst`.
> - The streams can be the same object (in-place) or two different streams
>   (both are `TStream` userdata — in practice `HTTP.Document`).
> - The output format follows the input image (JPG/PNG/WebP all supported).
> - This is CPU-bound native code — keep grids modest and expect a short
>   delay per image.

#### fmd.strings

A full `TStringList` binding — create extra string lists beyond the injected
`NAMES` / `LINKS` / `TASK.PageLinks` etc. Its main value: `TStrings` objects
can be passed **directly** to `XPathStringAll` / `XPathHREFAll` / XPath
list-filling methods (they append in place, no Lua table conversion needed),
and they can be sorted and reversed natively.

```lua
local strings = require 'fmd.strings'
local list = strings.New()      -- or strings.Create()
```

| Member | Description |
|--------|-------------|
| `list.Add(s)` | append a line |
| `list.AddText(s)` | append multi-line text (splits on line breaks) |
| `list.Text` | get/set the whole content (lines joined with `\r\n`) |
| `list.CommaText` | get/set as comma-separated values (CSV quoting rules) |
| `list.DelimitedText` | get/set using `Delimiter` + `StrictDelimiter`-style CSV |
| `list.Delimiter` | delimiter char for `DelimitedText` (one character) |
| `list.NameValueSeparator` | the `name=value` separator char (default `=`) |
| `list[i]` | 0-based line access (read/write) — the default indexer; the first line is `list[0]` |
| `list.Strings[i]` | same as `list[i]` |
| `list.Values['name']` | `name=value` pair access (read/write) |
| `list.Count` | number of lines |
| `list.Sort()` | sort ascending (case-sensitive) |
| `list.Reverse()` | reverse the order |
| `list.IndexOf(s)` | first index of `s` (0-based) or `-1` if absent |
| `list.IndexOfName(name)` | first index whose `name=` prefix matches (0-based), or `-1` |
| `list.Delete(i)` | delete 0-based line `i` |
| `list.Clear()` | empty the list |
| `list.LoadFromFile(path)` / `list.SaveToFile(path)` | file I/O |
| `list.LoadFromStream(stream)` / `list.SaveToStream(stream)` | stream I/O |
| `list.Get(i)` / `list.Set(i, s)` | 0-based accessor methods behind `list[i]` |

Typical use with XPath list-filling:

```lua
local strings = require 'fmd.strings'
local genres = strings.New()
CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="genre"]/a', genres)
MANGAINFO.Genres = genres.Text
```

Note that the injected objects (`NAMES`, `LINKS`, `HTTP.Headers`,
`HTTP.Cookies`, `TASK.PageLinks`, `MANGAINFO.ChapterNames`, …) are **the same
kind of object** — everything in this table works on them too. The classic
chapter-list reversal pattern:

```lua
MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
```

---

### utils.* Modules (Lua Files)

#### utils.json

JSON encode/decode (rxi's implementation).

```lua
local json = require 'utils.json'

-- Decode JSON
local data = json.decode(json_string)
print(data.field)

-- Encode JSON
local json_string = json.encode({key = 'value', array = {1, 2, 3}})
```

#### utils.lzstring

LZ-String decompression.

```lua
local lz = require 'utils.lzstring'

local decompressed = lz.decompressFromBase64(compressed_base64)
```

#### utils.jsunpack

Unpacker for `p,a,c,k,e,d` *string-table* obfuscation (NOT the classic
eval-packer — for that use `fmd.duktape`). Three functions:

| Function | Signature | Description |
|----------|-----------|-------------|
| `unpack36` | `unpack36(text, a, c, words)` | Substitutes the base-36 words of the `p,a,c,k,e,d` dictionary back into the source. `words` is the decoded word table (array of strings). |
| `replacehex` | `replacehex(text)` | Replaces `\xNN`-style escapes with characters |
| `splitstr` | `splitstr(str, delimiter)` | Splits a string into a Lua table by a single-character delimiter |

Real usage — `lua/modules/ManHuaGui.lua` (the site ships the word table
LZ-String-compressed):

```lua
local js = require 'utils.jsunpack'
local lz = require 'utils.lzstring'

local w = js.splitstr(lz.decompressFromBase64(GetBetween(",'", "'", s)), '|')
s = js.unpack36(text, a, c, w)
```

#### utils.nodejs

Node.js integration for JS-heavy sites (requires Node.js + Puppeteer, see
`lua/utils/nodejs.lua`).

```lua
local nodejs = require 'utils.nodejs'

local html = nodejs.run_html_load(url)                     -- render page with JS
local result = nodejs.run_html_load_with_js(url, js_code)  -- render, then run JS
local out = nodejs.run_js(js_code)                         -- run JS in page context
```

#### utils.nextjs

Helpers for **Next.js sites** — extracts and parses the `self.__next_f.push`
flight-data payloads that embed the page's real JSON data in `<script>` tags.

```lua
local NextJs = require 'utils.nextjs'
```

| Function | Returns | Description |
|----------|---------|-------------|
| `NextJs.ExtractFlightText(html)` | string | all flight payloads, JSON-unescaped and joined with `\n` |
| `NextJs.ParseFlightRecords(html)` | table | map of `id → decoded JSON value` for each `id: {...}` / `id: [...]` record in the flight data |
| `NextJs.GetRootObjects(html)` | array | all decoded record values (for scanning with `FindObject`) |
| `NextJs.FindKey(obj, key)` | value | recursive search for the first `key` in a nested table |
| `NextJs.FindObject(obj, predicate)` | table | recursive search for the first table for which `predicate(t)` is truthy |

Real usage — `lua/modules/Wurmz.lua` and `lua/modules/PhiliaScans.lua`:

```lua
local NextJs = require 'utils.nextjs'

local roots = NextJs.GetRootObjects(HTTP.Document.ToString())
for _, root in ipairs(roots) do
    local data = NextJs.FindObject(root, function(v)
        return v.images ~= nil            -- locate the object that has the images
    end)
    if data then
        for _, img in ipairs(data.images) do ... end
    end
end
```

For single values deep in an unknown structure, `FindKey` is the shortcut:

```lua
local images = NextJs.FindKey(NextJs.ParseFlightRecords(html), 'images')
```

#### utils.sha256

> [!WARNING]
> **Temporary module.** `utils.sha256` exists only because the **current public
> release of FMD2 has no native SHA-256**. A native implementation is now on
> the `master` branch and will ship in the **next release** as
> `fmd.crypto.SHA256` / `SHA256Hex`. When that happens, `utils.sha256` will be
> **removed**, so use it only in modules that must run on the current release,
> and plan to migrate them.

Pure-Lua SHA-256 and HMAC-SHA256 (no native dependency).

```lua
local sha256 = require 'utils.sha256'
```

| Function | Returns | Description |
|----------|---------|-------------|
| `sha256.sha256(msg)` | hex string (lowercase) | SHA-256 digest |
| `sha256.sha256_binary(msg)` | raw 32 bytes | binary digest |
| `sha256.hmac_sha256(key, text)` | hex string | HMAC. Note the order: **key first, text second** (opposite of `fmd.crypto.HMAC_SHA256(data, key)`) |

Real usage, `lua/modules/KLManga.lua` (request signing):

```lua
local function SetRequestHeaders()
    local key = 'KL9K40zaSyC9K40vOMLLbEcepIFBhUKXwELqxlwTEF'
    local timestamp = os.time()
    local payload = timestamp .. '.' .. key
    local signature = require('utils.sha256').sha256(payload)
    HTTP.Headers.Values['x-client-ts']  = timestamp
    HTTP.Headers.Values['x-client-sig'] = signature
end
```

##### Migrating to the native functions

Once the release that includes native SHA-256 is out, switch to `fmd.crypto`:

| `utils.sha256` (old) | `fmd.crypto` (new) | Notes |
|----------------------|--------------------|-------|
| `sha256.sha256(msg)` | `crypto.SHA256Hex(msg)` | Both return lowercase hex |
| `sha256.sha256_binary(msg)` | `crypto.SHA256(msg)` | Both return the raw 32 bytes |
| `sha256.hmac_sha256(key, text)` | `crypto.HMAC_SHA256Hex(text, key)` | **Argument order is swapped** (data first, key second) |

```lua
-- Before (current release)
local signature = require('utils.sha256').sha256(payload)

-- After (next release)
local crypto = require 'fmd.crypto'
local signature = crypto.SHA256Hex(payload)
```

#### utils.protoc (with the `pb` runtime)

Protocol Buffers support for sites with binary APIs (e.g. MangaPlus).
Two parts:

1. **`utils.protoc`** — pure-Lua `.proto` parser/`protoc` (it compiles
   message definitions into the runtime). Ships in `lua/utils/protoc.lua`.
2. **`pb`** — the compiled protobuf runtime (`pb.dll` in the FMD2 install
   directory; from starwing/lua-protobuf). It provides `pb.decode`,
   `pb.encode`, `pb.load`, etc.

The typical pairing: the module file sits next to a `.proto` file, and
`protoc` feeds it to the `pb` runtime at load time. Real usage —
`lua/modules/MangaPlus.lua` (+ `lua/modules/MangaPlus.proto`):

```lua
local protoc = require 'utils.protoc'
local pb = require 'pb'

protoc.proto3_optional = true
protoc:load(ReadFile(target_file))          -- compile MangaPlus.proto into pb

local manga = pb.decode('Response', HTTP.Document.ToString()).success.allTitlesViewV2.AllTitlesGroup
```

Common `protoc` API (call on the module table or an instance from
`protoc.new()`):

| Call | Description |
|------|-------------|
| `protoc.new()` | fresh parser instance |
| `protoc:load(source[, name])` | parse `.proto` **source text** and register types into `pb` |
| `protoc:loadfile(filename)` | parse a `.proto` file |
| `protoc:compile(source[, name])` | like `load`, but returns the compiled type table instead |
| `protoc:addpath(path)` | extra include path for `import` statements |
| `protoc.reload()` | clear all loaded types |

And the essential `pb` runtime calls:

| Call | Description |
|------|-------------|
| `pb.decode(msgtype, data)` | decode binary `data` into a Lua table |
| `pb.encode(msgtype, table)` | encode a Lua table to binary |
| `pb.load(data)` / `pb.loadfile(fn)` | load pre-compiled type descriptors |

---

## Templates

Templates are reusable module components that handle common website patterns.

### Using Templates

```lua
local Template = require 'templates.Madara'

function GetNameAndLink()
    Template.GetNameAndLink()

    return no_error
end

function GetInfo()
    Template.GetInfo()

    return no_error
end

function GetPageNumber()
    Template.GetPageNumber()

    return true
end
```

### Template Customization

Some templates support customization:

```lua
local Template = require 'templates.Madara'

-- Override constants
ChapterParameters   = 'custom=manga_get_chapters&manga='
DirectoryPagination = '/list?page='
XPathTokenAuthors   = 'Author'
XPathTokenGenres    = 'Genres'

function GetInfo()
    Template.GetInfo()
    -- Add custom processing
    return no_error
end
```

---

## Common Patterns

### Error Handling

```lua
-- Standard pattern
if not HTTP.GET(url) then return net_problem end
-- process
return no_error

-- Or
if HTTP.GET(url) then
    -- process
    return no_error
else
    return net_problem
end
```

### Account Support

```lua
function Init()
    local m = NewWebsiteModule()
    m.ID = '...'
    m.AccountSupport = true
    m.OnLogin = 'Login'
end
```

The `Login` handler itself is described in [Login](#14-login-onlogin), and
Example 3 below is a complete module with login.

### User Options

```lua
function Init()
    local m = NewWebsiteModule()
    m.ID = '...'
    m.AddOptionSpinEdit('timeout', 'Timeout (s)', 60)
    m.AddOptionCheckBox('showgroup', 'Show group name', false)
    m.AddOptionComboBox('imagesize', 'Image size:', 'Auto\n780x\n980x\nOriginal', 0)
end

-- Get option values
local timeout = tonumber(MODULE.GetOption('timeout') or '60')
local show_group = MODULE.GetOption('showgroup')
local image_size = MODULE.GetOption('imagesize')
```

### JSON API Handling

```lua
local json = require 'utils.json'

-- POST a JSON body
HTTP.MimeType = 'application/json'
if HTTP.POST(api_url, json.encode({query = '...'})) then
    local x = CreateTXQuery(HTTP.Document)

    -- Query the response with XPath
    local value = x.XPathString('json(*).results.data.field')

    -- Or decode it into a Lua table
    local data = json.decode(HTTP.Document.ToString())
end
```

All the JSON query styles (dot, `?` lookup, `parse-json`) are described in
[XPath Extensions](#xpath-extensions).

### Multi-Category Sites

```lua
local categories = {'action', 'romance', 'fantasy'}

function Init()
    local m = NewWebsiteModule()
    m.ID = '...'
    m.TotalDirectory = #categories
end

function GetNameAndLink()
    local category = categories[MODULE.CurrentDirectoryIndex + 1]
    HTTP.GET(MODULE.RootURL .. '/category/' .. category)
    -- process
    return no_error
end
```

### Iterating Over Nodes

```lua
local x = CreateTXQuery(HTTP.Document)

-- Method 1: Using for-in loop
for v in x.XPath('//div[@class="item"]/a').Get() do
    LINKS.Add(v.GetProperty('href').ToString())
    NAMES.Add(x.XPathString('.//h2', v))
end

-- Method 2: Using index
local nodes = x.XPath('//div[@class="item"]')
for i = 1, nodes.Count do
    local node = nodes.Get(i)
    local title = x.XPathString('.//h2', node)
end
```

---

## Examples

### Example 1: Simple Module

```lua
----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function GetNameAndLink()
    local u = MODULE.RootURL .. '/manga-list'

    if not HTTP.GET(u) then return net_problem end

    CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="manga-item"]/a', LINKS, NAMES)

    return no_error
end

function GetInfo()
    local u = MaybeFillHost(MODULE.RootURL, URL)

    if not HTTP.GET(u) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)
    MANGAINFO.Title     = x.XPathString('//h1[@class="title"]')
    MANGAINFO.CoverLink = x.XPathString('//img[@class="cover"]/@src')
    MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres"]/a')
    MANGAINFO.Summary   = x.XPathString('//div[@class="summary"]')

    x.XPathHREFAll('//ul[@class="chapters"]/li/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

    return no_error
end

function GetPageNumber()
    local u = MaybeFillHost(MODULE.RootURL, URL)

    if not HTTP.GET(u) then return false end

    CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="reader"]/img/@src', TASK.PageLinks)

    return true
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
    local m = NewWebsiteModule()
    m.ID                       = 'example1234567890abcdef'
    m.Name                     = 'ExampleSite'
    m.RootURL                  = 'https://example.com'
    m.Category                 = 'English'
    m.OnGetNameAndLink         = 'GetNameAndLink'
    m.OnGetInfo                = 'GetInfo'
    m.OnGetPageNumber          = 'GetPageNumber'
end
```

### Example 2: API-Based Module with JSON

```lua
----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function GetDirectoryPageNumber()
    HTTP.MimeType = 'application/json'
    local s = '{"filter":{"tags":[],"status":[],"types":[],"showAdult":true},"page":1}'

    if not HTTP.POST(MODULE.RootURL .. '/api/manga', s) then return net_problem end

    PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).total_pages')) or 1

    return no_error
end

function GetNameAndLink()
    HTTP.MimeType = 'application/json'
    local s = '{"filter":{"tags":[],"status":[],"types":[],"showAdult":true},"page":' .. (URL + 1) .. '}'

    if not HTTP.POST(MODULE.RootURL .. '/api/manga', s) then return net_problem end

    for v in CreateTXQuery(HTTP.Document).XPath('json(*).results()').Get() do
        LINKS.Add(v.GetProperty('id').ToString())
        NAMES.Add(v.GetProperty('title').ToString())
    end

    return no_error
end

function GetInfo()
    local u = MODULE.RootURL .. '/api/manga/' .. URL

    if not HTTP.GET(u) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)
    local data = x.XPath('json(*)')
    MANGAINFO.Title     = x.XPathString('title', data)
    MANGAINFO.CoverLink = x.XPathString('cover_url', data)
    MANGAINFO.Authors   = x.XPathString('author', data)
    MANGAINFO.Genres    = x.XPathString('string-join(genres?*?name, ", ")', data)
    MANGAINFO.Summary   = x.XPathString('description', data)

    for v in x.XPath('chapters?*', data).Get() do
        MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
        MANGAINFO.ChapterNames.Add('Chapter ' .. v.GetProperty('number').ToString())
    end

    return no_error
end

function GetPageNumber()
    local u = MODULE.RootURL .. '/api/chapter/' .. URL

    if not HTTP.GET(u) then return false end

    for v in CreateTXQuery(HTTP.Document).XPath('json(*).pages()').Get() do
        TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.GetProperty('image_url').ToString()))
    end

    return true
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
    local m = NewWebsiteModule()
    m.ID                       = 'api12345678901234567890'
    m.Name                     = 'APISite'
    m.RootURL                  = 'https://api.example.com'
    m.Category                 = 'English'
    m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
    m.OnGetNameAndLink         = 'GetNameAndLink'
    m.OnGetInfo                = 'GetInfo'
    m.OnGetPageNumber          = 'GetPageNumber'
    m.SortedList               = true
end
```

### Example 3: Module with Login

```lua
----------------------------------------------------------------------------------------------------
-- Module Constants
----------------------------------------------------------------------------------------------------

local crypto = require 'fmd.crypto'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function Login()
    if not MODULE.Account.Enabled then return false end

    local data = 'username=' .. crypto.EncodeURLElement(MODULE.Account.Username) ..
                 '&password=' .. crypto.EncodeURLElement(MODULE.Account.Password)

    MODULE.Account.Status = asChecking

    if HTTP.POST(MODULE.RootURL .. '/login', data) then
        if (HTTP.ResultCode == 200) and (HTTP.Cookies.Values['auth_session'] ~= '') then
            MODULE.Account.Status = asValid
            return true
        else
            MODULE.Account.Status = asInvalid
            return false
        end
    end

    MODULE.Account.Status = asUnknown
    return false
end

function GetNameAndLink()
    -- Login will be called automatically
    local u = MODULE.RootURL .. '/manga-list'

    if not HTTP.GET(u) then return net_problem end

    CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="manga"]/a', LINKS, NAMES)

    return no_error
end

function GetInfo()
    local u = MaybeFillHost(MODULE.RootURL, URL)

    if not HTTP.GET(u) then return net_problem end

    local x = CreateTXQuery(HTTP.Document)
    MANGAINFO.Title     = x.XPathString('//h1')
    MANGAINFO.CoverLink = x.XPathString('//img/@src')

    x.XPathHREFAll('//ul[@class="chapters"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)

    return no_error
end

function GetPageNumber()
    local u = MaybeFillHost(MODULE.RootURL, URL)

    if not HTTP.GET(u) then return false end

    CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="reader"]/img/@data-src', TASK.PageLinks)

    return true
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
    local m = NewWebsiteModule()
    m.ID                       = 'login123456789012345678'
    m.Name                     = 'LoginSite'
    m.RootURL                  = 'https://members.example.com'
    m.Category                 = 'English'
    m.OnGetNameAndLink         = 'GetNameAndLink'
    m.OnGetInfo                = 'GetInfo'
    m.OnGetPageNumber          = 'GetPageNumber'
    m.OnLogin                  = 'Login'
    m.AccountSupport           = true
end
```

---

## Development Tips

### 1. Use `--lua-dofile` for Development

When testing modules, use the `--lua-dofile` command-line argument to reload Lua files on each request instead of using cached bytecode:

```bash
FMD2.exe --lua-dofile
```

### 2. Use Templates When Possible

Many websites use common CMS themes. Check the
[Available Templates](#available-templates) before writing a module from scratch.

### 3. Set Proper Headers

Many sites require specific headers (usually `Referer` and `Origin`). Set them in
`OnBeforeDownloadImage`; see [Before Download Image](#10-before-download-image-onbeforedownloadimage).
For JSON APIs, see [JSON API Handling](#json-api-handling).

### 4. Handle JavaScript-Heavy Sites

For sites that require JavaScript execution:

```lua
-- Simple JS execution
local duktape = require 'fmd.duktape'
local script = x.XPathString('//script[contains(., "eval")]')
local result = duktape.ExecJS(script .. ';resultVariable;')

-- Complex sites (requires Node.js)
local nodejs = require 'utils.nodejs'
local html = nodejs.run_html_load(url)
```

### 5. Check for Paywalls

Skip locked chapters:

```lua
for v in x.XPath('//ul[@class="chapters"]/a').Get() do
    if v.GetAttribute('href') ~= '#' then
        MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
        MANGAINFO.ChapterNames.Add(v.ToString())
    end
end

for v in x.XPath('json(*).capitulos()').Get() do
    if v.GetProperty('es_premium').ToString() ~= 'true' then
        MANGAINFO.ChapterLinks.Add(slug .. '/' .. v.GetProperty('slug').ToString())
        MANGAINFO.ChapterNames.Add('Capítulo ' .. v.GetProperty('numero').ToString())
    end
end
```

### 6. Use Persistent Storage

Store data between sessions:

```lua
-- Save data
MODULE.Storage['last_update'] = os.time()
MODULE.Storage['cached_data'] = some_data

-- Load data
local last = MODULE.Storage['last_update']
```

`MODULE.Storage` also exposes `Text` (get/set the whole storage as
`name=value` lines), `Remove(name)`, `Tag` (integer), `Enable` (boolean)
and `Status` (string).

### 7. Debug with Logging

```lua
print('Debug: ' .. variable)

local logger = require 'fmd.logger'
logger.Send('Info message')
logger.SendError('Error: ' .. error_message)
```

### 8. Generate Unique IDs

Use a UUID generator to create unique module IDs:

```lua
-- Generate at https://guidgenerator.com/
m.ID = '56473ae447f04cee93683d7404b32e60'
```

---

## File Locations

```
FMD2/
├── lua/
│   ├── modules/          # Website modules (.lua)
│   ├── templates/        # Shared templates (.lua)
│   ├── utils/            # Utility libraries (.lua, .js)
│   ├── extras/           # Additional helpers
│   └── websitebypass/    # Cloudflare bypass scripts
├── baseunits/
│   └── lua/              # Pascal Lua integration files (.pas)
└── docs/
    └── SUPPORTED_WEBSITES.md
```

---

## Additional Resources

- **Supported Websites:** `docs/SUPPORTED_WEBSITES.md`
- **Example Modules:** `lua/modules/` folder (600+ examples)
- **Templates:** `lua/templates/` folder
- **Utilities:** `lua/utils/` folder