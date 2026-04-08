# FMD2 Lua Module Development Guide

## Table of Contents

1. [Overview](#overview)
2. [Module Structure](#module-structure)
3. [Injected Delphi Objects](#injected-delphi-objects)
4. [Built-in Functions](#built-in-functions)
5. [Lua Libraries](#lua-libraries)
6. [Templates](#templates)
7. [Common Patterns](#common-patterns)
8. [Examples](#examples)
9. [Development Tips](#development-tips)

---

## Overview

**Free Manga Downloader 2 (FMD2)** is a manga downloading application written in Object Pascal (Lazarus) that uses Lua scripts for website modules. The application supports 600+ manga websites through a modular Lua-based plugin system.

**Lua Version:** 5.4.8 (also supports LuaJIT)  
**JavaScript Engine:** Duktape 2.5.0 (embedded) + Node.js (optional)

---

## Module Structure

### Basic Module Template

Every Lua module follows a consistent structure:

```lua
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
    return m
end

----------------------------------------------------------------------------------------------------
-- Local Constants (optional)
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
```

### Required Event Handlers

| Handler | Purpose | Return Values |
|---------|---------|---------------|
| `GetNameAndLink` | Populate manga list | `no_error` (0), `net_problem` (1) |
| `GetInfo` | Fetch manga details | `no_error` (0), `net_problem` (1), `information_not_found` (2) |
| `GetPageNumber` | Fetch chapter pages | `true` (success), `false` (failure) |

### Optional Event Handlers

| Handler | Purpose | Return Values |
|---------|---------|---------------|
| `GetDirectoryPageNumber` | Get total pages in manga list | `no_error`, `net_problem` |
| `BeforeDownloadImage` | Set headers before image download | `true`, `false` |
| `DownloadImage` | Custom image download logic | `true`, `false` |
| `AfterImageSaved` | Post-process saved images | `true`, `false` |
| `GetImageURL` | Extract image URL from chapter page | `true`, `false` |
| `Login` | Handle user authentication | `true`, `false` |
| `TaskStart` | Initialize download task | `true`, `false` |

---

## Injected Delphi Objects

These objects are automatically injected by the Delphi application and available globally in Lua.

### HTTP Object (THTTPSendThread)

HTTP request handler for making web requests.

```lua
-- Basic usage
if HTTP.GET('https://example.com') then
    local html = HTTP.Document
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
| `HTTP.Document` | TMemoryStream | Response body (HTML/XML/JSON) |
| `HTTP.ResultCode` | Integer | HTTP status code (200, 404, etc.) |
| `HTTP.Terminated` | Boolean | Check if download was terminated |
| `HTTP.MimeType` | String | Request content type |
| `HTTP.UserAgent` | String | User agent string |
| `HTTP.RetryCount` | Integer | Number of retry attempts |
| `HTTP.Headers` | TStringList | Request headers |
| `HTTP.Cookies` | TStringList | Cookies |
| `HTTP.EnabledCookies` | Boolean | Enable cookie handling |

#### Methods

| Method | Description |
|--------|-------------|
| `HTTP.GET(url)` | Perform HTTP GET request |
| `HTTP.POST(url, data)` | Perform HTTP POST request |
| `HTTP.XHR(url)` | Perform XMLHttpRequest |
| `HTTP.HEAD(url)` | Perform HTTP HEAD request |
| `HTTP.Reset()` | Reset HTTP state |
| `HTTP.ResetBasic()` | Reset basic settings |
| `HTTP.ClearCookies()` | Clear all cookies |
| `HTTP.GetCookies()` | Get cookies string |
| `HTTP.AddServerCookies(domain, cookie_string)` | Add cookies |
| `HTTP.SetProxy(host, port, username, password, proxyType)` | Configure proxy |

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
    
    return m
end

function GetNameAndLink()
    -- Access module properties
    local timeout = MODULE.GetOption('timeout')
    local root = MODULE.RootURL
    
    if HTTP.GET(root .. '/manga') then
        -- process
        return no_error
    end
    return net_problem
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

These properties are set in `Init()` to register event handler functions:

| Property | Purpose | When Called |
|----------|---------|-------------|
| `OnBeforeUpdateList` | Before manga list update | Before updating manga list from website |
| `OnAfterUpdateList` | After manga list update | After updating manga list from website |
| `OnGetDirectoryPageNumber` | Get total pages | When determining manga list pagination |
| `OnGetNameAndLink` | Get manga list | When fetching manga list for browsing |
| `OnGetInfo` | Get manga details | When fetching manga information |
| `OnTaskStart` | Start download task | When starting a chapter download |
| `OnGetPageNumber` | Get chapter pages | When fetching chapter page count |
| `OnGetImageURL` | Get image URL | When extracting image URL from page |
| `OnBeforeDownloadImage` | Before image download | Before downloading each image |
| `OnDownloadImage` | Download image | Custom image download logic |
| `OnSaveImage` | Save image file | When saving image to disk |
| `OnAfterImageSaved` | After image saved | After image is saved to disk |
| `OnLogin` | User login | When authenticating user account |
| `OnAccountState` | Check account | When verifying account status |
| `OnCheckSite` | Check site | When checking if site is accessible |

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

**Account Status Values:**
- `asUnknown` (-1) - Unknown status
- `asChecking` (0) - Validating credentials
- `asValid` (1) - Credentials valid
- `asInvalid` (2) - Credentials invalid

---

### Event Handlers - Complete Workflow

This section explains how FMD2 calls your module's event handlers and in what order.

#### Injected Objects Reference

Each event handler has different objects and globals injected by FMD2. Here's a complete reference:

| Event Handler | Injected Objects | Injected Globals |
|--------------|------------------|------------------|
| `Init` | None (use `NewWebsiteModule()`) | None |
| `OnBeforeUpdateList` | `UPDATELIST` | None |
| `OnAfterUpdateList` | `UPDATELIST` | None |
| `OnGetDirectoryPageNumber` | `HTTP`, `UPDATELIST` | `PAGENUMBER`, `WORKPTR`, status constants |
| `OnGetNameAndLink` | `HTTP`, `LINKS`, `NAMES`, `UPDATELIST` | `URL`, status constants |
| `OnGetInfo` | `HTTP`, `MANGAINFO` | `URL`, status constants |
| `OnTaskStart` | `TASK` | None |
| `OnGetPageNumber` | `HTTP`, `TASK` | `URL` |
| `OnGetImageURL` | `HTTP`, `TASK` | `URL`, `WORKID` |
| `OnBeforeDownloadImage` | `HTTP`, `TASK` | `URL`, `WORKID` |
| `OnDownloadImage` | `HTTP`, `TASK` | `URL`, `WORKID` |
| `OnSaveImage` | `HTTP` | `PATH`, `FILENAME` |
| `OnAfterImageSaved` | None | `FILENAME` |
| `OnLogin` | `HTTP` | Account status constants |
| `OnAccountState` | None | Account status constants |
| `OnCheckSite` | None | Account status constants |

**Status Constants:**
- `no_error` (0), `net_problem` (1), `information_not_found` (2)
- `asUnknown` (-1), `asChecking` (0), `asValid` (1), `asInvalid` (2)

**Important Notes:**
- `MODULE` object is **always available** in all event handlers (except `Init`)
- `HTTP` is **NOT available** in: `OnAccountState`, `OnCheckSite`, `OnAfterImageSaved`, `OnTaskStart`
- `URL` global contains different things depending on context (page number, manga URL, chapter URL, etc.)

---

#### 1. Module Initialization (`Init`)

**When:** When FMD2 starts and loads Lua modules  
**Called by:** Module loader during application startup  
**Purpose:** Register module metadata and event handlers  

```lua
function Init()
    local m = NewWebsiteModule()
    m.ID = 'unique_hex_id'
    m.Name = 'Site Name'
    m.RootURL = 'https://site.com'
    m.Category = 'English'
    
    -- Register event handlers
    m.OnGetNameAndLink = 'GetNameAndLink'
    m.OnGetInfo = 'GetInfo'
    m.OnGetPageNumber = 'GetPageNumber'
    
    return m
end
```

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
**Return:** `true` to continue, `false` to cancel  

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
**Return:** `no_error`, `net_problem`, or `information_not_found`  
**Sets:** `PAGENUMBER` global variable  

```lua
function GetDirectoryPageNumber()
    if HTTP.GET(MODULE.RootURL .. '/manga-list?page=1') then
        local x = CreateTXQuery(HTTP.Document)
        PAGENUMBER = tonumber(x.XPathString('//last-page')) or 1
        return no_error
    end
    return net_problem
end
```

**Workflow:**
1. FMD2 needs to know total pages
2. Calls `OnGetDirectoryPageNumber`
3. Injected objects available: `HTTP`, `UPDATELIST`, `PAGENUMBER`, `WORKPTR`
4. Module sets `PAGENUMBER` to total page count
5. FMD2 uses this for pagination UI

---

#### 4. Get Name And Link (`OnGetNameAndLink`)

**When:** Fetching manga list from website  
**Called by:** UpdateListManagerThread (repeatedly for each page)  
**Return:** `no_error`, `net_problem`, or `information_not_found`  
**Sets:** `LINKS` and `NAMES` global lists  

```lua
function GetNameAndLink()
    local page = URL  -- URL contains current page number (0-based)
    local u = MODULE.RootURL .. '/manga-list?page=' .. (page + 1)
    
    if HTTP.GET(u) then
        local x = CreateTXQuery(HTTP.Document)
        x.XPathHREFAll('//div[@class="manga"]/a', LINKS, NAMES)
        return no_error
    end
    return net_problem
end
```

**Workflow:**
1. FMD2 iterates through pages (0 to PAGENUMBER-1)
2. For each page, calls `OnGetNameAndLink`
3. `URL` global contains current page number (0-based)
4. Injected objects: `HTTP`, `LINKS`, `NAMES`, `UPDATELIST`, `URL`
5. Module adds manga URLs to `LINKS` and names to `NAMES`
6. FMD2 collects all LINKS/NAMES for display
7. Can call `UPDATELIST.UpdateStatusText()` to show progress
8. Can access `UPDATELIST.CurrentDirectoryPageNumber`

---

#### 5. After Update List (`OnAfterUpdateList`)

**When:** After manga list update completes (optional)  
**Called by:** UpdateListManagerThread  
**Return:** `true` on success, `false` on failure  

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
**Return:** `no_error`, `net_problem`, or `information_not_found`  
**Sets:** `MANGAINFO` object properties  

```lua
function GetInfo()
    local u = MaybeFillHost(MODULE.RootURL, URL)
    
    if not HTTP.GET(u) then
        return net_problem
    end
    
    local x = CreateTXQuery(HTTP.Document)
    MANGAINFO.Title = x.XPathString('//h1')
    MANGAINFO.CoverLink = x.XPathString('//img/@src')
    MANGAINFO.Genres = x.XPathStringAll('//div[@class="genres"]/a')
    
    -- Fetch chapters
    x.XPathHREFAll('//ul[@class="chapters"]/a', 
                   MANGAINFO.ChapterLinks, 
                   MANGAINFO.ChapterNames)
    MANGAINFO.ChapterLinks.Reverse()
    MANGAINFO.ChapterNames.Reverse()
    
    return no_error
end
```

**Workflow:**
1. User clicks on manga or refreshes info
2. FMD2 calls `OnGetInfo`
3. `URL` global contains manga URL
4. Injected objects: `HTTP`, `MANGAINFO`, `URL`
5. Module fills `MANGAINFO` properties:
   - `Title`, `AltTitles`, `CoverLink`
   - `Authors`, `Artists`, `Genres`, `Status`, `Summary`
   - `ChapterLinks`, `ChapterNames` (TStringList)
6. FMD2 displays manga information
7. Chapters are shown in chapter list

---

#### 7. Task Start (`OnTaskStart`)

**When:** Before downloading a chapter (optional)  
**Called by:** TTaskContainer  
**Return:** `true` to continue, `false` to cancel  

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
3. Injected objects: `TASK`
4. Module can set initial values or perform setup
5. If returns `false`, download is cancelled

---

#### 8. Get Page Number (`OnGetPageNumber`)

**When:** Fetching chapter pages (image URLs)  
**Called by:** TTaskThread  
**Return:** `true` on success, `false` on failure  
**Sets:** `TASK.PageLinks` or `TASK.PageContainerLinks`  

```lua
function GetPageNumber()
    local u = MaybeFillHost(MODULE.RootURL, URL)
    
    if not HTTP.GET(u) then
        return false
    end
    
    local x = CreateTXQuery(HTTP.Document)
    
    -- For single-page chapters (all images on one page)
    x.XPathStringAll('//div[@class="reader"]/img/@src', TASK.PageLinks)
    
    -- OR for multi-page chapters (each image on separate page)
    -- x.XPathHREFAll('//div[@class="pages"]/a', 
    --                TASK.PageContainerLinks, 
    --                TASK.PageContainerLinks)
    
    return true
end
```

**Workflow:**
1. FMD2 starts downloading a chapter
2. Calls `OnGetPageNumber`
3. `URL` global contains chapter URL
4. Injected objects: `HTTP`, `TASK`, `URL`
5. Module has two options:
   - **Single-page:** Add all image URLs to `TASK.PageLinks`
   - **Multi-page:** Add page URLs to `TASK.PageContainerLinks`, then `OnGetImageURL` will be called for each
6. Can set `TASK.PageNumber` for progress tracking
7. Can set `TASK.FileNames` for custom filenames

---

#### 9. Get Image URL (`OnGetImageURL`)

**When:** Extracting image URL from chapter page (for multi-page chapters)  
**Called by:** TDownloadThread (for each page in `TASK.PageContainerLinks`)  
**Return:** `true` on success, `false` on failure  
**Sets:** `TASK.PageLinks[WORKID]`  

```lua
function GetImageURL()
    if HTTP.GET(URL) then
        local x = CreateTXQuery(HTTP.Document)
        local img_url = x.XPathString('//img[@id="page"]/@src')
        TASK.PageLinks[WORKID] = MaybeFillHost(MODULE.RootURL, img_url)
        return true
    end
    return false
end
```

**Workflow:**
1. Only called if `TASK.PageContainerLinks` has entries
2. FMD2 calls `OnGetImageURL` for each page
3. `URL` global contains current page URL
4. `WORKID` global contains current work index
5. Injected objects: `HTTP`, `TASK`, `URL`, `WORKID`
6. Module must set `TASK.PageLinks[WORKID]` to image URL
7. FMD2 then downloads the image

---

#### 10. Before Download Image (`OnBeforeDownloadImage`)

**When:** Before downloading each image  
**Called by:** TDownloadThread  
**Return:** `true` to continue, `false` to skip  
**Can modify:** `URL` global (to change image URL)  

```lua
function BeforeDownloadImage()
    -- Set required headers
    HTTP.Headers.Values['Referer'] = MODULE.RootURL
    HTTP.Headers.Values['Origin'] = MODULE.RootURL
    
    -- Optionally modify URL
    -- URL = URL:gsub('thumb', 'full')
    
    return true
end
```

**Workflow:**
1. FMD2 is about to download an image
2. Calls `OnBeforeDownloadImage`
3. `URL` global contains image URL (can be modified)
4. `WORKID` global contains image index
5. Injected objects: `HTTP`, `TASK`, `URL`, `WORKID`
6. Module can set headers, cookies, modify URL
7. If returns `false`, image download is skipped

---

#### 11. Download Image (`OnDownloadImage`)

**When:** Custom image download logic (optional, replaces default)  
**Called by:** TDownloadThread  
**Return:** `true` on success, `false` on failure  

```lua
function DownloadImage()
    -- Custom download logic (e.g., decrypt, decode, etc.)
    if HTTP.GET(URL) then
        -- Image data is in HTTP.Document
        return true
    end
    return false
end
```

**Workflow:**
1. Only called if `OnDownloadImage` is defined
2. Replaces default image download behavior
3. `URL` global contains image URL
4. `WORKID` global contains image index
5. Injected objects: `HTTP`, `TASK`, `URL`, `WORKID`
6. Module handles downloading image data
7. Image data should be in `HTTP.Document` after GET

---

#### 12. Save Image (`OnSaveImage`)

**When:** Custom image save logic (optional)  
**Called by:** TDownloadThread  
**Return:** Filename to save (string)  

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
4. Injected objects: `HTTP`, `PATH`, `FILENAME`
5. Module returns custom filename
6. FMD2 saves image with returned filename

---

#### 13. After Image Saved (`OnAfterImageSaved`)

**When:** After image is saved to disk  
**Called by:** TDownloadThread  
**Return:** `true` on success, `false` on failure  

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
4. Injected objects: `FILENAME`
5. Module can process the saved file
6. Common use: watermark removal

---

#### 14. Login (`OnLogin`)

**When:** Authenticating user account  
**Called by:** THTTPSendThread (when account is enabled)  
**Return:** `true` on success, `false` on failure  
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
3. Injected objects: `HTTP`, account status constants
4. Module should:
   - Check `MODULE.Account.Enabled`
   - Access `MODULE.Account.Username` and `.Password`
   - Set `MODULE.Account.Status = asChecking`
   - Perform login request
   - Set status to `asValid`, `asInvalid`, or `asUnknown`
5. Return `true` if login successful

---

#### 15. Account State (`OnAccountState`)

**When:** Checking account status (optional)  
**Called by:** FMD2 account verification  
**Return:** `true` if account is valid, `false` otherwise  
**Injected:** Account status constants only (`asUnknown`, `asChecking`, `asValid`, `asInvalid`)  

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
3. Only account status constants are injected
4. **No HTTP object available** - cannot make network requests
5. Module should return `true` if account is still valid
6. Use this for simple state checks only

---

#### 16. Check Site (`OnCheckSite`)

**When:** Checking if website is accessible (optional)  
**Called by:** FMD2 site availability check  
**Return:** `true` if site is accessible, `false` otherwise  
**Injected:** Account status constants only (`asUnknown`, `asChecking`, `asValid`, `asInvalid`)  

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
3. Only account status constants are injected
4. **No HTTP object available** - cannot make network requests
5. Module should return `true` if site should be considered active
6. Use this for module-specific state checks only

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
    if HTTP.GET(url) then
        local x = CreateTXQuery(HTTP.Document)
        MANGAINFO.Title = x.XPathString('//h1')
        MANGAINFO.ChapterLinks.Add(chapter_url)
        MANGAINFO.ChapterNames.Add('Chapter 1')
        return no_error
    end
    return net_problem
end
```

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `MANGAINFO.URL` | String | Current manga URL |
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
    if HTTP.GET(chapter_url) then
        local x = CreateTXQuery(HTTP.Document)
        TASK.PageLinks.Add('https://example.com/image1.jpg')
        TASK.PageLinks.Add('https://example.com/image2.jpg')
        return true
    end
    return false
end
```

#### Properties

| Property | Type | Description |
|----------|------|-------------|
| `TASK.PageLinks` | TStringList | Image URLs for current chapter |
| `TASK.ChapterLinks` | TStringList | All chapter URLs |
| `TASK.ChapterNames` | TStringList | Chapter names |
| `TASK.PageContainerLinks` | TStringList | Chapter page URLs (multi-page chapters) |
| `TASK.FileNames` | TStringList | Custom filenames for images |
| `TASK.PageNumber` | Integer | Total page count |
| `TASK.CurrentDownloadChapterPtr` | Integer | Current chapter index |
| `TASK.Link` | String | Current download link |

---

### LINKS and NAMES (TStringList)

Global lists for manga URLs and names.

```lua
function GetNameAndLink()
    if HTTP.GET(url) then
        local x = CreateTXQuery(HTTP.Document)
        x.XPathHREFAll('//div[@class="manga"]/a', LINKS, NAMES)
        return no_error
    end
    return net_problem
end
```

#### Methods

| Method | Description |
|--------|-------------|
| `LINKS.Add(url)` | Add manga URL |
| `NAMES.Add(name)` | Add manga name |
| `LINKS.Count` | Get number of items |
| `LINKS.Clear()` | Clear all items |
| `LINKS.Reverse()` | Reverse list order |
| `LINKS[i]` | Access item by index (0-based) |

Same methods apply to `NAMES`.

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
| `UPDATELIST.CurrentDirectoryPageNumber` | Integer | Current page in manga list |

#### Methods

| Method | Description |
|--------|-------------|
| `UPDATELIST.UpdateStatusText(text)` | Update status message in UI |

---

### PAGENUMBER (Global Variable)

Total pages in manga list directory.

```lua
function GetDirectoryPageNumber()
    if HTTP.GET(url) then
        local x = CreateTXQuery(HTTP.Document)
        PAGENUMBER = tonumber(x.XPathString('//last-page')) or 1
        return no_error
    end
    return net_problem
end
```

---

### URL (Global Variable)

Current URL being processed (varies by context):
- In `GetNameAndLink`: Current page number (0-based)
- In `GetInfo`: Manga URL
- In `GetPageNumber`: Chapter URL

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

#### Methods

| Method | Description |
|--------|-------------|
| `x.ParseHTML(html)` | Parse HTML string or stream |
| `x.XPathString(xpath)` | Get single string result |
| `x.XPathStringAll(xpath)` | Get all matching strings (comma-separated) |
| `x.XPathStringAll(xpath, separator, context)` | Get all strings with custom separator |
| `x.XPathHREFAll(xpath, linksList, namesList)` | Extract href and text to lists |
| `x.XPathHREFTitleAll(xpath, linksList, namesList)` | Extract href and title attribute |
| `x.XPath(xpath)` | Get node list for iteration |
| `x.XPathCount(xpath)` | Count matching nodes |
| `x.GetProperty(name)` | Get JSON property value |
| `x.ToString()` | Get node text content |
| `x.GetAttribute(name)` | Get HTML attribute value |
| `x.Get(i)` | Get node at index (1-based) |
| `x.Count` | Number of nodes |

#### XPath Extensions

FMD2 extends XPath with special functions:

**JSON Parsing:**
```lua
-- Parse JSON content
local json = x.XPath('json(*)')
local value = x.XPathString('data.field', json)
local all = x.XPathStringAll('results[*].name', json)

-- Iterate JSON array
for v in x.XPath('json(*).items()').Get() do
    print(v.GetProperty('name').ToString())
end
```

**String Functions:**
```lua
-- Join multiple strings
x.XPathString('string-join(//title, ", ")')

-- Substring operations
x.XPathString('substring-before(//text, "delimiter")')
x.XPathString('substring-after(//text, "prefix")')
```

**JSON Navigation:**
```lua
jn:keys()      -- Get JSON object keys
jn:members()   -- Get JSON array members
```

---

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
| `Trim(text)` | Remove whitespace |
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

#### Other Functions

```lua
sleep(1000)  -- Pause for 1000ms
print('Debug message')  -- Output to log
```

---

## Lua Libraries

### fmd.* Modules (Injected by Delphi)

#### fmd.crypto

Crypto and encoding functions.

```lua
local crypto = require 'fmd.crypto'

-- URL encoding
local encoded = crypto.EncodeURLElement('hello world')
local decoded = crypto.DecodeURL(encoded)

-- Base64
local b64 = crypto.EncodeBase64('data')
local raw = crypto.DecodeBase64(b64)

-- HTML encoding
local html = crypto.HTMLEncode('<script>')
local plain = crypto.HTMLDecode('&lt;script&gt;')

-- Hash functions
local md5 = crypto.MD5('data')
local md5hex = crypto.MD5Hex('data')
local sha1 = crypto.SHA1('data')
local hmac = crypto.HMAC_MD5('data', 'key')

-- Encryption
local encrypted = crypto.AESDecryptCBC(base64_data, key, iv)
```

#### fmd.duktape

JavaScript execution engine.

```lua
local duktape = require 'fmd.duktape'

-- Execute JavaScript
local script = [[
    var result = someFunction();
    result;
]]
local result = duktape.ExecJS(script)

-- Common pattern for obfuscated JS
local script = x.XPathString('//script[contains(., "eval")]')
local decrypted = duktape.ExecJS(script .. ';decryptedVariable;')
```

#### fmd.env

Environment information.

```lua
local fmd = require 'fmd.env'

print(fmd.SelectedLanguage)  -- Current language
print(fmd.LuaDirectory)      -- Lua files directory
```

#### fmd.fileutil

File utilities.

```lua
local fileutil = require 'fmd.fileutil'

local name = fileutil.ExtractFileNameOnly('/path/to/file.txt')
-- returns 'file'

fileutil.SerializeAndMaintainNames(list1, list2)
```

#### fmd.logger

Logging functionality.

```lua
local logger = require 'fmd.logger'

logger.Send('Info message')
logger.SendError('Error message')
```

#### fmd.subprocess

Run external commands.

```lua
local subprocess = require 'fmd.subprocess'

local result = subprocess.run({'node', 'script.js'})
```

#### fmd.mangafoxwatermark

Watermark removal for FanFox/MangaHere.

```lua
local watermark = require 'fmd.mangafoxwatermark'

function AfterImageSaved()
    watermark.RemoveWatermark(FILENAME, true)
    return true
end
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

JavaScript unpacker for obfuscated code.

```lua
local jsunpack = require 'utils.jsunpack'

local unpacked = jsunpack.unpack(packed_js)
```

#### utils.nodejs

Node.js integration for JS-heavy sites (requires Puppeteer).

```lua
local nodejs = require 'utils.nodejs'

-- Load page with JavaScript execution
local html = nodejs.run_html_load(url)

-- Execute custom script
local result = nodejs.run_js_load(url, 'document.body.innerHTML')
```

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

### Available Templates

| Template | Description | Usage Count |
|----------|-------------|-------------|
| `templates.Madara` | WordPress Madara theme | 100+ sites |
| `templates.FoOlSlide` | FoOlSlide reader | Common |
| `templates.SPA` | Single Page Applications | Common |
| `templates.MangaHub` | MangaHub API | Common |
| `templates.Guya` | Guya.moe reader | Few sites |
| `templates.GroupLe` | Grouple.co based sites | Few sites |
| `templates.HeanCMS` | HeanCMS based sites | Few sites |
| `templates.NineManga` | NineManga template | Few sites |
| `templates.ColorlibAnime` | Colorlib anime theme | Few sites |
| `templates.FuzzyDoodle` | FuzzyDoodle sites | Few sites |
| `templates.FMReader` | FMReader template | Few sites |
| `templates.LibGroup` | LibGroup template | Few sites |
| `templates.Roseveil` | Roseveil template | Few sites |
| `templates.ZManga` | ZManga template | Few sites |

### Template Customization

Some templates support customization:

```lua
local Template = require 'templates.Madara'

-- Override constants
ChapterParameters = 'custom=manga_get_chapters&manga='
DirectoryPagination = '/list?page='
XPathTokenAuthors = 'Author'
XPathTokenGenres = 'Genres'

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
    return m
end

function Login()
    if not MODULE.Account.Enabled then return false end
    
    local crypto = require 'fmd.crypto'
    local data = 'username=' .. crypto.EncodeURLElement(MODULE.Account.Username) ..
                 '&password=' .. crypto.EncodeURLElement(MODULE.Account.Password)
    
    MODULE.Account.Status = asChecking
    
    if HTTP.POST(login_url, data) then
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

### User Options

```lua
function Init()
    local m = NewWebsiteModule()
    m.ID = '...'
    m.AddOptionSpinEdit('timeout', 'Timeout (s)', 60)
    m.AddOptionCheckBox('showgroup', 'Show group name', false)
    m.AddOptionComboBox('imagesize', 'Image size:', 'Auto\n780x\n980x\nOriginal', 0)
    return m
end

-- Get option values
local timeout = tonumber(MODULE.GetOption('timeout') or '60')
local showGroup = MODULE.GetOption('showgroup')
local imageSize = MODULE.GetOption('imagesize')
```

### JSON API Handling

```lua
local json = require 'utils.json'

-- POST with JSON
HTTP.MimeType = 'application/json'
if HTTP.POST(api_url, json.encode({query = '...'})) then
    local x = CreateTXQuery(HTTP.Document)
    local data = x.XPath('json(*)')
    local value = x.XPathString('data.field', data)
end

-- Or using XPath directly
local x = CreateTXQuery(HTTP.Document)
local value = x.XPathString('json(*).results.data.field')
```

### Multi-Category Sites

```lua
local categories = {'action', 'romance', 'fantasy'}

function Init()
    local m = NewWebsiteModule()
    m.ID = '...'
    m.TotalDirectory = #categories
    return m
end

function GetNameAndLink()
    local category = categories[MODULE.CurrentDirectoryIndex + 1]
    HTTP.GET(MODULE.RootURL .. '/category/' .. category)
    -- process
    return no_error
end
```

### Image Download Handlers

```lua
-- Set headers before download
function BeforeDownloadImage()
    HTTP.Headers.Values['Referer'] = MODULE.RootURL
    HTTP.Headers.Values['Origin'] = MODULE.RootURL
    return true
end

-- Custom image download
function DownloadImage()
    if HTTP.GET(url) then
        -- process image data in HTTP.Document
        return true
    end
    return false
end

-- Post-process saved image
function AfterImageSaved()
    require('fmd.mangafoxwatermark').RemoveWatermark(FILENAME, true)
    return true
end

-- Get image URL from chapter page
function GetImageURL()
    if HTTP.GET(page_url) then
        local x = CreateTXQuery(HTTP.Document)
        TASK.PageLinks[WORKID] = x.XPathString('//img/@src')
        return true
    end
    return false
end
```

### Reversing Chapter Lists

```lua
-- Reverse if chapters are in wrong order
MANGAINFO.ChapterLinks.Reverse()
MANGAINFO.ChapterNames.Reverse()
```

### Iterating Over Nodes

```lua
local x = CreateTXQuery(HTTP.Document)

-- Method 1: Using for-in loop
for v in x.XPath('//div[@class="item"]').Get() do
    local title = x.XPathString('.//h2', v)
    local link = x.XPathString('.//a/@href', v)
    LINKS.Add(link)
    NAMES.Add(title)
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
    return m
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function GetNameAndLink()
    local u = MODULE.RootURL .. '/manga-list?page=' .. (URL + 1)
    
    if not HTTP.GET(u) then return net_problem end
    
    local x = CreateTXQuery(HTTP.Document)
    x.XPathHREFAll('//div[@class="manga-item"]/a', LINKS, NAMES)
    
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
    MANGAINFO.ChapterLinks.Reverse()
    MANGAINFO.ChapterNames.Reverse()
    
    return no_error
end

function GetPageNumber()
    local u = MaybeFillHost(MODULE.RootURL, URL)
    
    if not HTTP.GET(u) then return false end
    
    local x = CreateTXQuery(HTTP.Document)
    x.XPathStringAll('//div[@class="reader"]/img/@src', TASK.PageLinks)
    
    return true
end
```

### Example 2: API-Based Module with JSON

```lua
----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
    local m = NewWebsiteModule()
    m.ID                       = 'api12345678901234567890'
    m.Name                     = 'APISite'
    m.RootURL                  = 'https://api.example.com'
    m.Category                 = 'English'
    m.SortedList               = true
    m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
    m.OnGetNameAndLink         = 'GetNameAndLink'
    m.OnGetInfo                = 'GetInfo'
    m.OnGetPageNumber          = 'GetPageNumber'
    return m
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local json = require 'utils.json'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

function GetDirectoryPageNumber()
    HTTP.MimeType = 'application/json'
    
    if HTTP.POST(MODULE.RootURL .. '/api/manga', json.encode({page = 0})) then
        local x = CreateTXQuery(HTTP.Document)
        PAGENUMBER = tonumber(x.XPathString('json(*).total_pages')) or 1
        return no_error
    end
    
    return net_problem
end

function GetNameAndLink()
    HTTP.MimeType = 'application/json'
    local data = json.encode({page = URL})
    
    if HTTP.POST(MODULE.RootURL .. '/api/manga', data) then
        local x = CreateTXQuery(HTTP.Document)
        
        for v in x.XPath('json(*).results()').Get() do
            LINKS.Add(v.GetProperty('id').ToString())
            NAMES.Add(v.GetProperty('title').ToString())
        end
        
        return no_error
    end
    
    return net_problem
end

function GetInfo()
    local url = MODULE.RootURL .. '/api/manga/' .. URL
    
    if HTTP.GET(url) then
        local x = CreateTXQuery(HTTP.Document)
        local data = x.XPath('json(*)')
        
        MANGAINFO.Title     = x.XPathString('title', data)
        MANGAINFO.CoverLink = x.XPathString('cover_url', data)
        MANGAINFO.Authors   = x.XPathString('author', data)
        MANGAINFO.Genres    = x.XPathStringAll('genres[*].name', data)
        MANGAINFO.Summary   = x.XPathString('description', data)
        
        for v in x.XPath('chapters[*]', data).Get() do
            MANGAINFO.ChapterLinks.Add(v.GetProperty('id').ToString())
            MANGAINFO.ChapterNames.Add('Chapter ' .. v.GetProperty('number').ToString())
        end
        
        return no_error
    end
    
    return net_problem
end

function GetPageNumber()
    local url = MODULE.RootURL .. '/api/chapter/' .. URL
    
    if HTTP.GET(url) then
        local x = CreateTXQuery(HTTP.Document)
        
        for v in x.XPath('json(*).pages[*]').Get() do
            TASK.PageLinks.Add(v.GetProperty('image_url').ToString())
        end
        
        return true
    end
    
    return false
end
```

### Example 3: Module with Login

```lua
----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
    local m = NewWebsiteModule()
    m.ID                       = 'login123456789012345678'
    m.Name                     = 'LoginSite'
    m.RootURL                  = 'https://members.example.com'
    m.Category                 = 'English'
    m.AccountSupport           = true
    m.OnLogin                  = 'Login'
    m.OnGetNameAndLink         = 'GetNameAndLink'
    m.OnGetInfo                = 'GetInfo'
    m.OnGetPageNumber          = 'GetPageNumber'
    return m
end

----------------------------------------------------------------------------------------------------
-- Local Constants
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
        if HTTP.ResultCode == 200 and HTTP.Document.Size > 100 then
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
    local u = MODULE.RootURL .. '/manga?page=' .. (URL + 1)
    
    if not HTTP.GET(u) then return net_problem end
    
    local x = CreateTXQuery(HTTP.Document)
    x.XPathHREFAll('//div[@class="manga"]/a', LINKS, NAMES)
    
    return no_error
end

function GetInfo()
    local u = MaybeFillHost(MODULE.RootURL, URL)
    
    if not HTTP.GET(u) then return net_problem end
    
    local x = CreateTXQuery(HTTP.Document)
    MANGAINFO.Title = x.XPathString('//h1')
    MANGAINFO.CoverLink = x.XPathString('//img/@src')
    
    x.XPathHREFAll('//ul[@class="chapters"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    
    return no_error
end

function GetPageNumber()
    HTTP.Headers.Values['Referer'] = MODULE.RootURL
    
    if HTTP.GET(URL) then
        local x = CreateTXQuery(HTTP.Document)
        x.XPathStringAll('//div[@class="reader"]/img/@data-src', TASK.PageLinks)
        return true
    end
    
    return false
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

Many websites use common CMS themes. Check existing templates before writing from scratch:
- **Madara** - WordPress Madara theme (100+ sites)
- **FoOlSlide** - FoOlSlide reader
- **SPA** - Single Page Applications

### 3. Handle JSON APIs

Many modern sites use JSON APIs. Use the built-in JSON support:

```lua
-- Using XPath JSON functions
local x = CreateTXQuery(HTTP.Document)
local value = x.XPathString('json(*).data.field')

-- Or using utils.json
local json = require 'utils.json'
local data = json.decode(HTTP.Document.ReadString)
```

### 4. Set Proper Headers

Many sites require specific headers:

```lua
function BeforeDownloadImage()
    HTTP.Headers.Values['Referer'] = MODULE.RootURL
    HTTP.Headers.Values['Origin'] = MODULE.RootURL
    return true
end
```

### 5. Handle JavaScript-Heavy Sites

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

### 6. Check for Paywalls

Skip locked chapters:

```lua
for v in x.XPath('//ul[@class="chapters"]/a').Get() do
    if v.GetAttribute('href') ~= '#' then
        MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
        MANGAINFO.ChapterNames.Add(v.ToString())
    end
end
```

### 7. Use Persistent Storage

Store data between sessions:

```lua
-- Save data
MODULE.Storage['last_update'] = os.time()
MODULE.Storage['cached_data'] = some_data

-- Load data
local last = MODULE.Storage['last_update']
```

### 8. Debug with Logging

```lua
print('Debug: ' .. variable)
logger.Send('Info message')
logger.SendError('Error: ' .. error_message)
```

### 9. Module Categories

Common category values:
- `English` - English language sites
- `English-Scanlation` - English scanlation groups
- `Raw` - Japanese raw sites
- `Vietnamese` - Vietnamese sites
- `Russian` - Russian sites
- `Arabic-Scanlation` - Arabic scanlation
- `H-Sites` - Adult content sites
- `Webcomics` - Webcomic platforms

### 10. Generate Unique IDs

Use a UUID generator to create unique module IDs:

```lua
-- Generate at https://www.uuidgenerator.net/
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
│   └── lua/              # Delphi Lua integration files (.pas)
└── docs/
    └── SUPPORTED_WEBSITES.md
```

---

## Key Statistics

- **Total Modules:** 600+ website modules
- **Templates:** 35+ reusable templates
- **Lua Version:** 5.4.8
- **JavaScript Engine:** Duktape 2.5.0 (embedded) + Node.js (optional)

---

## Return Values Reference

```lua
-- Standard return values
no_error              = 0  -- Success
net_problem           = 1  -- Network error
information_not_found = 2  -- Content not found

-- Boolean returns
true   -- Success
false  -- Failure

-- Account status
asUnknown  = -1  -- Unknown status
asChecking = 0   -- Validating credentials
asValid    = 1   -- Credentials valid
asInvalid  = 2   -- Credentials invalid
```

---

## Additional Resources

- **Supported Websites:** `docs/SUPPORTED_WEBSITES.md`
- **Example Modules:** `lua/modules/` folder (600+ examples)
- **Templates:** `lua/templates/` folder
- **Utilities:** `lua/utils/` folder
