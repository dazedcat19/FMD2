# FMD2 Lua Module Development Guide

Reference for writing and maintaining Lua website modules for Free Manga Downloader 2 (FMD2).

## Table of Contents

- [1. Overview](#1-overview)
- [2. Quick Start](#2-quick-start)
    - [Module Skeleton](#module-skeleton)
    - [Steps to Create a Module](#steps-to-create-a-module)
- [3. Defining a Module](#3-defining-a-module)
    - [How `Init()` Is Loaded](#how-init-is-loaded)
    - [Module ID](#module-id)
    - [MODULE Object (TModuleContainer)](#module-object-tmodulecontainer)
    - [User Options](#user-options)
    - [Multi-Directory Sites](#multi-directory-sites)
- [4. Event Handlers](#4-event-handlers)
    - [Handler Reference](#handler-reference)
    - [Status Constants and Return Values](#status-constants-and-return-values)
    - [Injected Globals](#injected-globals)
        - [URL](#url)
        - [PAGENUMBER](#pagenumber)
    - [Lifecycle at a Glance](#lifecycle-at-a-glance)
    - [Updating the Manga List](#updating-the-manga-list)
        - [`OnAfterUpdateList`](#onafterupdatelist)
        - [`OnBeforeUpdateList`](#onbeforeupdatelist)
        - [`OnGetDirectoryPageNumber`](#ongetdirectorypagenumber)
        - [`OnGetNameAndLink`](#ongetnameandlink)
    - [Reading Manga Details](#reading-manga-details)
        - [`OnGetInfo`](#ongetinfo)
    - [Downloading Chapters](#downloading-chapters)
        - [`OnTaskStart`](#ontaskstart)
        - [`OnGetPageNumber`](#ongetpagenumber)
        - [`OnGetImageURL`](#ongetimageurl)
        - [`OnBeforeDownloadImage`](#onbeforedownloadimage)
        - [`OnDownloadImage`](#ondownloadimage)
        - [`OnSaveImage`](#onsaveimage)
        - [`OnAfterImageSaved`](#onafterimagesaved)
    - [Module Checker](#module-checker)
        - [`OnCheckSite`](#onchecksite)
- [5. Accounts and Login](#5-accounts-and-login)
    - [Enabling Account Support](#enabling-account-support)
    - [MODULE.Account Properties](#moduleaccount-properties)
    - [`OnLogin`](#onlogin)
    - [`OnAccountState`](#onaccountstate)
- [6. Host Objects](#6-host-objects)
    - [HTTP Object (THTTPSendThread)](#http-object-thttpsendthread)
        - [HTTP Properties](#http-properties)
        - [HTTP Methods](#http-methods)
        - [HTTP.Document (TMemoryStream)](#httpdocument-tmemorystream)
        - [HTTP.Reset() and Header Lifecycle](#httpreset-and-header-lifecycle)
        - [Cookie Storage and Persistence](#cookie-storage-and-persistence)
    - [MANGAINFO Object (TMangaInfo)](#mangainfo-object-tmangainfo)
        - [MANGAINFO Properties](#mangainfo-properties)
    - [TASK Object (TTaskContainer)](#task-object-ttaskcontainer)
        - [TASK Properties](#task-properties)
    - [LINKS and NAMES (TStringList)](#links-and-names-tstringlist)
        - [LINKS and NAMES Methods](#links-and-names-methods)
    - [UPDATELIST Object (TUpdateListManagerThread)](#updatelist-object-tupdatelistmanagerthread)
        - [UPDATELIST Properties](#updatelist-properties)
        - [UPDATELIST Methods](#updatelist-methods)
    - [Storage and Persistence](#storage-and-persistence)
- [7. Parsing and Helper Functions](#7-parsing-and-helper-functions)
    - [TXQuery (XML/HTML/JSON Parser)](#txquery-xmlhtmljson-parser)
    - [Utility Functions](#utility-functions)
- [8. Libraries](#8-libraries)
    - [fmd.* Modules (Injected by the host application)](#fmd-modules-injected-by-the-host-application)
    - [utils.* Modules (Lua Files)](#utils-modules-lua-files)
- [9. Templates](#9-templates)
    - [Available Templates](#available-templates)
    - [Using Templates](#using-templates)
    - [Template Customization](#template-customization)
- [10. Cookbook](#10-cookbook)
    - [Error Handling](#error-handling)
    - [JSON API Handling](#json-api-handling)
    - [Iterating Over Nodes](#iterating-over-nodes)
    - [Headers, Referer and Origin](#headers-referer-and-origin)
    - [Sites That Need JavaScript](#sites-that-need-javascript)
    - [Skipping Locked or Premium Chapters](#skipping-locked-or-premium-chapters)
- [11. Complete Examples](#11-complete-examples)
    - [Example 1: Simple Module](#example-1-simple-module)
    - [Example 2: API-Based Module with JSON](#example-2-api-based-module-with-json)
    - [Example 3: Module with Login](#example-3-module-with-login)
- [12. Development and Debugging](#12-development-and-debugging)
    - [Reloading Modules While Developing](#reloading-modules-while-developing)
    - [Logging](#logging)
    - [Common Pitfalls](#common-pitfalls)
- [Appendix: File Locations and Resources](#appendix-file-locations-and-resources)

---

## 1. Overview

**Free Manga Downloader 2 (FMD2)** is a manga downloader written in Object Pascal (Lazarus). Every supported website is a **Lua module**: one `.lua` file that tells FMD2 how to list manga, read manga details, and find the images of a chapter.

**Modules:** 400+ website modules  
**Templates:** 40+ reusable CMS templates  
**Lua Version:** 5.4.9  
**JavaScript Engine:** Duktape 2.7.0 (embedded) + Node.js (optional)

### Where to find what

| I want to... | Go to |
|--------------|-------|
| Write my first module | [Quick Start](#2-quick-start), then [Defining a Module](#3-defining-a-module) |
| Know which handler runs when, and what it must return | [Event Handlers](#4-event-handlers) |
| Support a site that needs a login | [Accounts and Login](#5-accounts-and-login) |
| Send requests, set headers, handle cookies | [HTTP](#http-object-thttpsendthread) |
| Fill `MANGAINFO`, `TASK`, `LINKS`/`NAMES` | [Host Objects](#6-host-objects) |
| Parse HTML or JSON | [Parsing and Helper Functions](#7-parsing-and-helper-functions) |
| Decrypt data, run JavaScript, descramble images | [Libraries](#8-libraries) |
| Reuse an existing CMS family (Madara, MangaThemesia, ...) | [Templates](#9-templates) |
| Copy a proven pattern | [Cookbook](#10-cookbook) and [Complete Examples](#11-complete-examples) |
| Test and debug a module | [Development and Debugging](#12-development-and-debugging) |

### Conventions used in this guide

- `UPPERCASE` names (`HTTP`, `MODULE`, `MANGAINFO`, `TASK`, `LINKS`, `NAMES`, `URL`, ...) are objects and globals **injected by the host application**. Which ones exist in which handler is listed in the [handler reference](#handler-reference).
- Lists (`LINKS`, `NAMES`, `TASK.PageLinks`, ...) are 0-based `TStrings` objects; see [LINKS and NAMES](#links-and-names-tstringlist).
- "Real usage" examples name the bundled module they were taken from, so you can read the full file in `lua/modules/`.

---

## 2. Quick Start

A module is a single file in `lua/modules/`. It contains **handler functions** (what the site-specific work looks like) and an **`Init()`** function that registers the module and tells FMD2 which handler is which.

### Module Skeleton

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
    m.ID                       = '56473ae447f04cee93683d7404b32e60'  -- GUID with the hyphens removed — generate your own, see Generate Unique IDs
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

### Steps to Create a Module

1. **Check for a template.** Many sites run a common CMS theme. If one fits, start from a [template](#9-templates) and override only what differs.
2. **Otherwise copy a similar bundled module** from `lua/modules/` (or the skeleton above).
3. **Generate a permanent [Module ID](#module-id)** and fill in `Name`, `RootURL` and `Category` in `Init()`.
4. **Implement the three required handlers:** `OnGetNameAndLink`, `OnGetInfo`, `OnGetPageNumber`. See [Handler Reference](#handler-reference) for what each must return.
5. **Run FMD2 with `--lua-dofile`** so edits are picked up without restarting; see [Development and Debugging](#12-development-and-debugging).
6. *(Optional)* Add [`OnCheckSite`](#onchecksite) so the built-in module checker can verify the module.

A complete minimal module is shown in [Example 1](#example-1-simple-module).

---

## 3. Defining a Module

This section covers everything that happens in `Init()` and the `MODULE` object that represents your module at runtime.

### How `Init()` Is Loaded

**When:** When FMD2 starts and loads Lua modules  
**Called by:** Module loader during application startup  
**Purpose:** Register module metadata and event handlers  

See the [Basic Module Template](#module-skeleton) for a complete `Init()`.

**Workflow:**
1. FMD2 scans `lua/modules/` folder for `.lua` files
2. For each file, calls `Init()` function
3. `NewWebsiteModule()` creates module object
4. Module properties are set (ID, Name, RootURL, etc.)
5. Event handler strings are stored (function names)
6. Module is added to FMD2's module list
7. If ID or Name is empty, module is discarded

### Module ID

`m.ID` is the module's permanent identifier. Create it with a GUID generator
and **remove the hyphens** — a GUID without hyphens is exactly 32 hexadecimal
characters:

```lua
-- Generate at https://guidgenerator.com/, then strip the dashes:
--   56473ae4-47f0-4cee-9368-3d7404b32e60
-- becomes
m.ID = '56473ae447f04cee93683d7404b32e60'
```

The host does not validate the format — any unique string is accepted — but
keep to the plain dash-less GUID convention used by the bundled modules:

- **Unique** — no two modules may share an ID; the module list is sorted and
  searched by ID, so a duplicate breaks module lookup.
- **Stable** — never change or reuse an ID after release. Favorites, download
  history, module settings, user options, account credentials and persistent
  cookies are all stored under the ID; a new ID silently orphans the user's
  existing data.
- **Plain form** — no hyphens, no `{}` braces; the 32-character dash-less
  form like every bundled module.

### MODULE Object (TModuleContainer)

Current module information and settings. This is the central object for module configuration and runtime control.

```lua
function Init()
    local m = NewWebsiteModule()
    m.ID = '56473ae447f04cee93683d7404b32e60'  -- GUID with the hyphens removed

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

The `On...` properties that register event handlers are listed in the [Handler Reference](#handler-reference).

#### MODULE Properties

| Property | Type | Access | Description |
|----------|------|--------|-------------|
| `MODULE.ID` | String | Read/Write | Module's unique identifier, **assigned once in `Init()` and never changed** — by convention a GUID/UUID created with a GUID generator and stripped of the hyphens — 32 hexadecimal characters, e.g. `56473ae447f04cee93683d7404b32e60`. The format is not validated, but the ID must be unique among all modules and must never change after release. See [Module ID](#module-id) |
| `MODULE.Name` | String | Read/Write | Module display name — set once in `Init()`; writing it later changes the list entry but not the stored website name |
| `MODULE.RootURL` | String | Read/Write | Website root URL — set once in `Init()` |
| `MODULE.Category` | String | Read/Write | Module category (English, Vietnamese, etc.) — set once in `Init()` |
| `MODULE.MaxTaskLimit` | Integer | Read/Write | Maximum concurrent tasks for this module |
| `MODULE.MaxThreadPerTaskLimit` | Integer | Read/Write | Maximum threads per task |
| `MODULE.MaxConnectionLimit` | Integer | Read/Write | Maximum HTTP connections |
| `MODULE.ActiveTaskCount` | Integer | Read-only | Currently active tasks |
| `MODULE.ActiveConnectionCount` | Integer | Read-only | Currently active connections |
| `MODULE.SortedList` | Boolean | Read/Write | Whether manga list is sorted |
| `MODULE.InformationAvailable` | Boolean | Read/Write | Whether manga info is available |
| `MODULE.FavoriteAvailable` | Boolean | Read/Write | Whether favorites are supported |
| `MODULE.DynamicPageLink` | Boolean | Read/Write | Whether page links are dynamic |
| `MODULE.CurrentDirectoryIndex` | Integer | Read/Write | Current directory index (for multi-directory sites) |
| `MODULE.TotalDirectory` | Integer | Read/Write | Total number of categories |
| `MODULE.AccountSupport` | Boolean | Read/Write | Whether module supports user accounts |
| `MODULE.LastUpdated` | String | Read/Write | Last update timestamp |
| `MODULE.Tag` | Integer | Read/Write | Custom tag for internal use |
| `MODULE.Storage` | TStringsStorage | Read/Write | In-memory key-value storage — one store per module, shared by all of its tasks; a missing key reads as `''`, and everything is gone after an app restart; see [Storage and Persistence](#storage-and-persistence) |
| `MODULE.Account` | TWebsiteModuleAccount | Read-only | User account information — **only present when `AccountSupport` is enabled** (otherwise accessing it raises an error) |
| `MODULE.Guardian` | TCriticalSection | Read-only | Thread synchronization object |

#### MODULE Methods

| Method | Parameters | Description |
|--------|-----------|-------------|
| `MODULE.GetOption(name)` | `name` (string) | Get user-configured option value |
| `MODULE.AddOptionCheckBox(name, caption, default)` | All strings/boolean | Add checkbox option |
| `MODULE.AddOptionEdit(name, caption, default)` | All strings | Add text input option |
| `MODULE.AddOptionSpinEdit(name, caption, default)` | name, caption (string), default (int) | Add numeric option |
| `MODULE.AddOptionComboBox(name, caption, items, default)` | items is newline-separated | Add dropdown option |
| `MODULE.AddServerCookies(domain, cookie_string)` | Both strings | Add cookies for `domain` to the module's shared cookie storage — the same storage `HTTP.AddServerCookies` writes to, with the domain given directly. Only cookies carrying an `expires=`/`max-age=` attribute survive an app restart — see [Cookie Storage and Persistence](#cookie-storage-and-persistence) |
| `MODULE.GetServerCookies(domain, name)` | Both strings | Return cookies stored for `domain` — optionally only the cookie `name` (pass `''` for all) — as `name=value; domain=…; path=…` lines |
| `MODULE.RemoveCookies(domain, name)` | Both strings | Remove cookies stored for `domain` — optionally only the cookie `name` (pass `''` for all) |
| `MODULE.ClearCookies()` | None | Clear the module's shared cookie storage — the same storage as `HTTP.ClearCookiesStorage()` |

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

### Multi-Directory Sites

```lua
local directories = {'action', 'romance', 'fantasy'}

function Init()
    local m = NewWebsiteModule()
    m.ID = '...'
    m.TotalDirectory = #directories
end

function GetNameAndLink()
    local directory = directories[MODULE.CurrentDirectoryIndex + 1]
    if not HTTP.GET(MODULE.RootURL .. '/category/' .. directory) then return net_problem end
    -- process
    return no_error
end
```

---

## 4. Event Handlers

Handlers are the Lua functions FMD2 calls at defined points: while updating a website's manga list, reading manga details, and downloading chapters. You register a handler in `Init()` by assigning its **function name as a string** to the matching `On...` property (for example `m.OnGetInfo = 'GetInfo'`).

Handlers are grouped below by the job they belong to. Account handlers are in [Accounts and Login](#5-accounts-and-login).

### Handler Reference

Each handler is described in the grouped sections that follow; the handlers that deal with accounts are in [Accounts and Login](#5-accounts-and-login).

| Handler property | Required | Returns | Injected objects | Injected globals |
|------------------|----------|---------|------------------|------------------|
| [`OnGetNameAndLink`](#ongetnameandlink) | **Yes** | status code | `HTTP`, `LINKS`, `NAMES`, `UPDATELIST` | `URL` |
| [`OnGetInfo`](#ongetinfo) | **Yes** | status code | `HTTP`, `MANGAINFO` | `URL` |
| [`OnGetPageNumber`](#ongetpagenumber) | **Yes** | `true` / `false` | `HTTP`, `TASK` | `URL` |
| [`OnBeforeUpdateList`](#onbeforeupdatelist) | No | `true` / `false` — **not used by the host** | `UPDATELIST` | none |
| [`OnGetDirectoryPageNumber`](#ongetdirectorypagenumber) | No | status code | `HTTP`, `UPDATELIST` | `PAGENUMBER`, `WORKPTR` |
| [`OnAfterUpdateList`](#onafterupdatelist) | No | `true` / `false` — **not used by the host** | `UPDATELIST` | none |
| [`OnTaskStart`](#ontaskstart) | No | `true` / `false` — **not used by the host** | `TASK` | none |
| [`OnGetImageURL`](#ongetimageurl) | No | `true` / `false` | `HTTP`, `TASK` | `URL`, `WORKID` |
| [`OnBeforeDownloadImage`](#onbeforedownloadimage) | No | `true` / `false` — **not used by the host** | `HTTP`, `TASK` | `URL`, `WORKID` |
| [`OnDownloadImage`](#ondownloadimage) | No | `true` / `false` | `HTTP`, `TASK` | `URL`, `WORKID` |
| [`OnSaveImage`](#onsaveimage) | No | filename (string) | `HTTP` | `PATH`, `FILENAME` |
| [`OnAfterImageSaved`](#onafterimagesaved) | No | `true` / `false` — **not used by the host** | none | `FILENAME` |
| [`OnLogin`](#onlogin) | No | `true` / `false` — **not used by the host** (set `MODULE.Account.Status` instead) | `HTTP` | account status constants |
| [`OnAccountState`](#onaccountstate) | No | `true` / `false` — **not used by the host** | none | account status constants |
| [`OnCheckSite`](#onchecksite) | No | `true` / `false` — **not used by the host** (fill `MANGACHECK` instead) | `MANGACHECK` | account status constants, net status constants |

"Status code" means one of the [status constants](#status-constants-and-return-values).
`Init` has no injected objects; it creates the module with `NewWebsiteModule()`.

> [!IMPORTANT]
> - `MODULE` is **always available** in every handler except `Init`.
> - `HTTP` is **not available** in `OnAccountState`, `OnCheckSite`, `OnAfterImageSaved` and `OnTaskStart`, so those handlers cannot make network requests.
> - `URL` means different things in different handlers; see [URL](#url).
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

### Injected Globals

Besides the injected objects, FMD2 sets plain global variables before calling certain handlers.

| Global | Set in | Meaning |
|--------|--------|---------|
| `URL` | `OnGetNameAndLink`, `OnGetInfo`, `OnGetPageNumber`, `OnGetImageURL`, `OnBeforeDownloadImage`, `OnDownloadImage` | The item being processed; the meaning depends on the handler (see below) |
| `PAGENUMBER` | `OnGetDirectoryPageNumber` | **You set it** to the number of pages in the manga list |
| `WORKPTR` | `OnGetDirectoryPageNumber` | 0-based index of the directory being processed |
| `WORKID` | `OnGetImageURL`, `OnBeforeDownloadImage`, `OnDownloadImage` | 0-based index of the page/image being processed |
| `PATH` | `OnSaveImage` | Target directory |
| `FILENAME` | `OnSaveImage`, `OnAfterImageSaved` | Default filename (`OnSaveImage`) / path of the saved file (`OnAfterImageSaved`) |

#### URL

The current URL being processed. What it holds depends on the handler:

| Handler | `URL` contains |
|---------|----------------|
| [`OnGetNameAndLink`](#ongetnameandlink) | Current page number of the manga list (0-based) |
| [`OnGetInfo`](#ongetinfo) | Manga URL |
| [`OnGetPageNumber`](#ongetpagenumber) | Chapter URL |
| [`OnGetImageURL`](#ongetimageurl) | Chapter URL — take the page URL from `TASK.PageContainerLinks[WORKID]` or derive it with `WORKID` |
| `OnBeforeDownloadImage`, `OnDownloadImage` | Image URL (read-only in `OnBeforeDownloadImage`; the host does not read changes back) |

#### PAGENUMBER

Total pages in manga list directory.

```lua
PAGENUMBER = tonumber(x.XPathString('//last-page')) or 1
```

Set it in `OnGetDirectoryPageNumber`; see
[`OnGetDirectoryPageNumber`](#ongetdirectorypagenumber).

### Lifecycle at a Glance

**Scenario: User updates manga list and downloads a chapter**

```
1. Init()                              -- Module loads at startup
2. OnAfterUpdateList()                 -- Optional: reset state from previous run
3. OnBeforeUpdateList()                -- Optional: prepare for update
4. OnGetDirectoryPageNumber()          -- Get total pages (sets PAGENUMBER)
5. OnGetNameAndLink()                  -- Called for each page (fills LINKS/NAMES)
   - Page 0: GetNameAndLink()
   - Page 1: GetNameAndLink()
   - ...
   - Page N: GetNameAndLink()
6. OnBeforeUpdateList()                -- Called again before the manga-info stage
7. OnGetInfo()                         -- For each new manga found (fills MANGAINFO)

-- User clicks on manga to view info
8. OnGetInfo()                         -- Fetch manga details (fills MANGAINFO)

-- User selects chapter to download
9. OnTaskStart()                       -- Optional: initialize task
10. OnGetPageNumber()                  -- Get chapter pages (fills TASK.PageLinks)

-- For each image in chapter:
11. OnBeforeDownloadImage()             -- Set headers, modify URL if needed
12. OnDownloadImage()                   -- Optional: custom download logic
    -- OR default download if not defined
13. OnSaveImage()                       -- Optional: save the image yourself, return path
14. OnAfterImageSaved()                -- Post-process saved image

-- Repeat 11-14 for each image
```

**Scenario: User has account enabled**

```
1. Init()
2. User clicks "check" in Account Manager
3. OnLogin()                           -- Host sets Status=asChecking, then calls Login
   - Module sets MODULE.Account.Status (asValid/asInvalid/asUnknown)
4. [Normal workflow continues]
5. OnAccountState()                    -- When the account checkbox is toggled
```

### Updating the Manga List

These handlers run when the user presses **Update** on a website's manga list (`TUpdateListManagerThread`). In call order: `OnAfterUpdateList` (reset hook at the start), `OnBeforeUpdateList`, `OnGetDirectoryPageNumber`, `OnGetNameAndLink` once per list page, `OnBeforeUpdateList` again, and then `OnGetInfo` for every manga that is new. Only `OnGetNameAndLink` is required.

#### `OnAfterUpdateList`

**When:** At the start of a website's update, and when an update is cancelled (optional)
**Called by:** UpdateListManagerThread

```lua
function AfterUpdateList()
    -- Reset state accumulated by the previous update run
    return true
end
```

**Workflow:**
1. Before anything else is done for the website, FMD2 calls `OnAfterUpdateList` (if defined) — use it to clean up state from a *previous* update run
2. If the update is terminated (cancelled), `OnAfterUpdateList` is called again on the way out
3. It is **not** called after a successful update — despite the name, this is a reset hook, not a completion hook

#### `OnBeforeUpdateList`

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
3. The return value is **ignored** — the update proceeds either way, so treat this purely as a preparation hook
4. The host actually calls it **twice**: once before the page-count stage, and again after the manga-list pages have been fetched (before the manga-info stage)

#### `OnGetDirectoryPageNumber`

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
the 0-based index of the directory whose page count is being
fetched. For multi-directory sites (`MODULE.TotalDirectory` > 1) use it
together with `MODULE.CurrentDirectoryIndex` to find out which directory is
being updated.

#### `OnGetNameAndLink`

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

### Reading Manga Details

#### `OnGetInfo`

**When:** Fetching manga details (title, chapters, etc.)
**Called by:** manga-info threads — the update list (for new manga), favorites refresh, and the manga info dialog
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

### Downloading Chapters

These handlers run for every chapter the user downloads. `OnGetPageNumber` collects the image URLs; the remaining handlers customize how each image is fetched, saved and post-processed. Only `OnGetPageNumber` is required. The default behaviour of each step applies when its handler is not defined.

#### `OnTaskStart`

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
4. The return value is **ignored** — the download proceeds either way

#### `OnGetPageNumber`

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

> [!WARNING]
> `OnGetPageNumber` runs once per chapter on the **task's shared `HTTP`
> object**, with no reset in between. If you set custom headers, cookies or
> `MimeType`, call `HTTP.Reset()` first — otherwise every chapter after the
> first inherits the previous response's headers and your custom headers are
> silently dropped. See
> [HTTP.Reset() and Header Lifecycle](#httpreset-and-header-lifecycle).

#### `OnGetImageURL`

**When:** Resolving the real image URL for each page (for multi-page chapters)
**Called by:** TDownloadThread (once for each page still marked for work)
**Sets:** `TASK.PageLinks[WORKID]`

```lua
function GetImageURL()
    -- URL here is the CHAPTER url, not the page url!
    -- Build the page url from TASK.PageContainerLinks[WORKID]...
    local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])
    -- ...or derive it from the chapter url + WORKID:
    -- local u = MaybeFillHost(MODULE.RootURL, URL:gsub('/gallery/', '/g/')) .. (WORKID + 1) .. '/'

    if not HTTP.GET(u) then return false end
    local x = CreateTXQuery(HTTP.Document)
    local img_url = x.XPathString('//img[@id="page"]/@src')
    TASK.PageLinks[WORKID] = MaybeFillHost(MODULE.RootURL, img_url)
    return true
end
```

**Workflow:**
1. FMD2 calls `OnGetImageURL` once per work item (per page not yet resolved)
2. `WORKID` global contains the current work index
3. `URL` contains the **chapter URL** — take the page URL from `TASK.PageContainerLinks[WORKID]` or derive it with `WORKID`
4. Module must set `TASK.PageLinks[WORKID]` to the image URL
5. FMD2 then downloads the image

#### `OnBeforeDownloadImage`

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
5. The return value is **ignored** — it is always overwritten by `OnDownloadImage` or the default download, so this hook cannot skip an image

#### `OnDownloadImage`

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

#### `OnSaveImage`

**When:** Custom image save logic (optional)
**Called by:** TDownloadThread

```lua
function SaveImage()
    -- Save the image yourself and return the full path of the saved file
    local filename = PATH .. '/' .. string.format('%03d', WORKID + 1) .. '.jpg'
    HTTP.Document.SaveToFile(filename)
    return filename
end
```

**Workflow:**
1. Only called if `OnSaveImage` is defined (and the file does not already exist on disk)
2. `PATH` global contains target directory
3. `FILENAME` global contains default filename
4. The module **saves the image data itself** — the downloaded bytes are in `HTTP.Document` (e.g. `HTTP.Document.SaveToFile(path)`)
5. Return the full path of the saved file; FMD2 then checks that the file exists

#### `OnAfterImageSaved`

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

### Module Checker

#### `OnCheckSite`

**When:** The user runs the module checker dialog (optional)
**Called by:** FormCheckModules
**Sets:** `MANGACHECK` properties

```lua
function CheckSite()
    -- Provide a known-good manga and chapter for the module checker to test with
    MANGACHECK.MangaURL     = '/manga/6rUzU'
    MANGACHECK.MangaTitle   = 'The Academy’s Sashimi Sword Master'
    MANGACHECK.ChapterURL   = '/6rUzU/pIEtz'
    MANGACHECK.ChapterTitle = 'Episode 74'
    MANGACHECK.ChapterURLAddRootHost = false
end
```

**Workflow:**
1. User opens the module checker tool
2. FMD2 calls `OnCheckSite` (if defined) with a `MANGACHECK` object injected
3. Module fills `MANGACHECK` with a test manga and chapter (URLs and titles) it knows exist
4. The checker then runs a full `GetInfo`/chapter test against those URLs to verify the module still works
5. The return value is **not used by the host** — the `MANGACHECK` fields are the output

Available `MANGACHECK` properties: `MangaURL`, `MangaTitle`, `ChapterURL`,
`ChapterTitle`, `ChapterURLPrefix` (prefix the checker uses to build chapter
links), `MangaURLAddRootHost` and `ChapterURLAddRootHost` (prepend
`MODULE.RootURL` to the given URLs; default `true`).

---

## 5. Accounts and Login

Some sites need a login to read all content. A module opts in with `AccountSupport = true` and provides an `OnLogin` handler. The user enters credentials in FMD2's **Account Manager**; the module reads them from `MODULE.Account`.

> [!IMPORTANT]
> Login is **not** run automatically before other handlers. `OnLogin` is called when the user clicks the *check* button in the Account Manager. Everything else (sending the cookie or token on later requests) is up to your handlers.

### Enabling Account Support

```lua
function Init()
    local m = NewWebsiteModule()
    m.ID = '...'
    m.AccountSupport = true
    m.OnLogin = 'Login'
end
```

The `Login` handler itself is described in [`OnLogin`](#onlogin), and
Example 3 below is a complete module with login.

For sites whose login returns a **token** (JWT/Bearer) instead of a session
cookie, store it in `MODULE.Account.Cookies` — the account field that is saved
to disk and reloaded on startup — and send it yourself as a header:

```lua
function Login()
    if not MODULE.Account.Enabled then return false end
    local crypto = require 'fmd.crypto'
    local data = 'email='    .. crypto.EncodeURLElement(MODULE.Account.Username) ..
                 '&password=' .. crypto.EncodeURLElement(MODULE.Account.Password)
    MODULE.Account.Status = asChecking
    HTTP.Reset()  -- drop stale headers, e.g. the expired Authorization
    if HTTP.POST(MODULE.RootURL .. '/api/auth/login', data) and HTTP.ResultCode == 200 then
        MODULE.Account.Cookies = CreateTXQuery(HTTP.Document).XPathString('json(*).data.token')
        if MODULE.Account.Cookies ~= '' then
            MODULE.Account.Status = asValid
            return true
        end
    end
    MODULE.Account.Status = asInvalid
    return false
end

function GetPageNumber()
    -- first request after enabling the account: log in once
    if MODULE.Account.Enabled and MODULE.Account.Cookies == '' then Login() end

    local retried = false
    ::request:: -- on a rejected token, log in again below and jump back here

    HTTP.Reset()  -- the task's HTTP object is reused for every chapter
    HTTP.Headers.Values['Authorization'] = 'Bearer ' .. MODULE.Account.Cookies
    if not HTTP.GET(MODULE.RootURL .. '/api/.../pages') then return false end

    local x = CreateTXQuery(HTTP.Document)
    -- rejection signal 1: an explicit 401 status
    -- rejection signal 2: the body — this API answers 200 with an error
    -- payload such as {"status":false,"data":[],"errorType":"login_required"}
    local rejected = HTTP.ResultCode == 401
        or x.XPathString('json(*).errorType') == 'login_required'

    if rejected then
        -- token expired or rejected: clear it, log in again, retry once here
        if not retried and MODULE.Account.Enabled then
            MODULE.Account.Cookies = ''
            retried = true
            if Login() then goto request end
        end
        return false
    end

    -- rejection signal 3: a field the normal response always carries (here
    -- `rv`) is missing — XPathString returns '' for a missing key and the
    -- string 'null' for an explicit JSON null
    local rv = x.XPathString('json(*).rv')
    if rv == '' or rv == 'null' then return false end

    for v in x.XPath('json(*).data()').Get() do
        TASK.PageLinks.Add(v.GetProperty('imageUrl').ToString())
    end

    return true
end
```

Not every API signals a rejected token with a `401` — some answer `200` with
an error body instead, e.g.
`{"status":false,"data":[],"message":"...","errorType":"login_required"}`, so
the only reliable signal is the **content** of the response: an error field
such as `errorType`, or the absence of a field the normal response always
carries. Whatever the signal, the pattern is the same: clear the token, log
in again, and retry **once, inside the same handler**. The retry is a backward
`goto` to the request: the `retried` flag is declared *before* the label, so it
survives the jump and limits the retry to a single attempt (Lua has no
`continue`; a backward `goto` is the standard way to restart a block).

### MODULE.Account Properties

| Property | Type | Access | Description |
|----------|------|--------|-------------|
| `MODULE.Account.Enabled` | Boolean | Read/Write | Is account enabled |
| `MODULE.Account.Username` | String | Read/Write | Username |
| `MODULE.Account.Password` | String | Read/Write | Password |
| `MODULE.Account.Status` | Integer | Read/Write | Account status (see below) |
| `MODULE.Account.Cookies` | String | Read/Write | Account cookies / auth token — **saved encrypted to `modules.json` and restored on startup**. Not applied to requests automatically: set the header yourself, e.g. `HTTP.Headers.Values['Authorization'] = 'Bearer ' .. MODULE.Account.Cookies`. Set it back to `''` to force a re-login |
| `MODULE.Account.Guardian` | TCriticalSection | Read-only | Thread sync object |

For the `Status` values (`asUnknown`, `asChecking`, `asValid`, `asInvalid`) see
[Status Constants and Return Values](#status-constants-and-return-values).

> [!NOTE]
> `Username`, `Password` and `Cookies` are written to disk — encrypted, in
> `modules.json` under the module's `ID` — and decrypted again when the module
> is loaded.

### `OnLogin`

**When:** Authenticating user account
**Called by:** TAccountCheckThread — when the user clicks the check button in the Account Manager dialog
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
1. User opens the Account Manager and clicks "check" for the module's account
2. FMD2 sets `MODULE.Account.Status = asChecking`, then calls `OnLogin`
3. Module should:
   - Check `MODULE.Account.Enabled`
   - Access `MODULE.Account.Username` and `.Password`
   - Perform login request
   - Set status to `asValid`, `asInvalid`, or `asUnknown`
4. The return value itself is **not used by the host** — the `Status` you set is what the UI shows

### `OnAccountState`

**When:** The user toggles the account's enabled checkbox in the Account Manager
**Called by:** TAccountManagerForm.vtAccountListChecked — synchronously, on the UI thread, right after the host writes `MODULE.Account.Enabled`

```lua
function AccountState()
    -- MODULE is available; HTTP is NOT injected here.
    -- This is a toggle notification, not a validation hook.
    if not MODULE.Account.Enabled then
        MODULE.Account.Cookies = ''   -- forget the saved token/session
        MODULE.ClearCookies()         -- forget the module's stored cookies
    end
    MODULE.Account.Status = asUnknown
    return true
end
```

**Workflow:**
1. The user (un)checks the module's account in the Account Manager list
2. The host updates `MODULE.Account.Enabled`, then calls `OnAccountState` (if registered) on the UI thread
3. Use it to react to the toggle — clear a cached token (`MODULE.Account.Cookies`), clear stored cookies (`MODULE.ClearCookies()`), reset `MODULE.Account.Status`
4. The return value is **not used by the host**

> [!NOTE]
> This handler is a **notification, not a validator**: it is never called at
> app startup or on a timer, and it cannot make network requests. Validating
> credentials is the job of [`OnLogin`](#onlogin), which the host runs
> in a background thread when the user clicks "check" or saves new credentials.
> The call also runs on the UI thread, so keep it quick — no waiting, no heavy work.

A complete module with login is shown in [Example 3](#example-3-module-with-login).

---

## 6. Host Objects

Objects that FMD2 injects into your handlers (see the [handler reference](#handler-reference) for which handler gets which). `MODULE` is covered in [Defining a Module](#3-defining-a-module).

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

-- Request with custom headers
HTTP.Reset()  -- always reset before setting headers, see HTTP.Reset() and Header Lifecycle
HTTP.Headers.Values['Referer'] = 'https://example.com/'
if HTTP.GET('https://example.com/data') then
    -- process response
end
```

#### HTTP Properties

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

#### HTTP Methods

| Method | Description |
|--------|-------------|
| `HTTP.GET(url)` | Perform HTTP GET request |
| `HTTP.POST(url, data)` | Perform HTTP POST request (`data` is written into the document first; passing `''` sends whatever the document already holds) |
| `HTTP.XHR(url)` | Perform XMLHttpRequest (GET with `X-Requested-With: XMLHttpRequest`) |
| `HTTP.HEAD(url)` | Perform HTTP HEAD request |
| `HTTP.Request(method, url)` | Perform a request with an arbitrary HTTP method, e.g. `HTTP.Request('POST', url)` |
| `HTTP.Reset()` | Clear the request state — document, headers, in-memory cookies, ranges, `MimeType` back to `text/html` — close the keep-alive socket, and restore the default browser headers (`DNT`, `Accept`, `Accept-Language`, `Accept-Charset`, plus `Accept-Encoding` when `HTTP.Compress` is on). **Call it before setting custom request headers** — see [HTTP.Reset() and Header Lifecycle](#httpreset-and-header-lifecycle) |
| `HTTP.ResetBasic()` | Same clear as `Reset()` without re-adding the default browser headers — for when you build the whole header list yourself |
| `HTTP.ClearCookies()` | Clear all cookies |
| `HTTP.GetCookies()` | Get cookies string |
| `HTTP.AddServerCookies(url, cookie_string)` | Add cookies to the module's shared cookie storage, taking the domain and path from `url` — see [Cookie Storage and Persistence](#cookie-storage-and-persistence) |
| `HTTP.ClearCookiesStorage()` | Clear the module's shared cookie storage — every stored cookie, session and persistent alike |
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

#### HTTP.Reset() and Header Lifecycle

`HTTP.Headers` is dual-purpose: it carries your request headers **into** a
request, and the server's response headers **out** of it. Once `HTTP.GET`,
`HTTP.POST`, `HTTP.XHR`, `HTTP.HEAD` or `HTTP.Request` returns, `HTTP.Headers`
no longer holds the headers you set — it holds the status line and the
headers of the response instead:

```
HTTP/1.1 200 OK
Content-Type: application/json
...
```

As a safety net, the request functions detect these leftovers (the `HTTP/`
status line as the first entry) and silently reset the header list when the
next request starts. That safety reset also wipes headers the module set
*after* the previous response arrived, so they never reach the server. A
handler that sets a custom header therefore works on its first call and
fails on every following one:

```lua
function GetPageNumber()
    local u = MODULE.RootURL .. '/api/.../pages'
    HTTP.Headers.Values['X-Organization'] = site -- appended to the PREVIOUS response
    if not HTTP.GET(u) then return false end     -- safety reset drops X-Organization
    ...
end
```

Calling `HTTP.Reset()` **before** setting custom headers starts from a clean
state, so the header survives the request:

```lua
function GetPageNumber()
    local u = MODULE.RootURL .. '/api/.../pages'
    HTTP.Reset()                                  -- clean request state
    HTTP.Headers.Values['X-Organization'] = site  -- now it is sent
    if not HTTP.GET(u) then return false end
    ...
end
```

Real usage — `lua/modules/CapibaraTraductor.lua` (without the `Reset()` the
first chapter of a multi-chapter task downloads fine and every later chapter
fails):

```lua
function GetPageNumber()
    local site, slug, cid = URL:match('^/([^/]+)/([^/]+)/([^/]+)$')
    local u = MODULE.RootURL .. '/api/manga-custom/' .. slug .. '/chapter/' .. cid .. '/pages'
    HTTP.Reset()
    HTTP.Headers.Values['X-Organization'] = site
    if not HTTP.GET(u) then return false end
    CreateTXQuery(HTTP.Document).XPathStringAll('json(*).data().imageUrl', TASK.PageLinks)
    return true
end
```

**When a reset is needed** depends on who owns the `HTTP` object and whether
the host resets it between handler calls:

| Handler | `HTTP` instance | Host resets? |
|---------|-----------------|--------------|
| `OnGetDirectoryPageNumber`, `OnGetNameAndLink` | one per update-list run | yes — after every call |
| `OnGetInfo` (update list) | one per update-list run | yes — after every manga |
| `OnGetInfo` (favorites / manga info dialog) | fresh per lookup | first request starts clean |
| [`OnGetPageNumber`](#ongetpagenumber) | **one per download task — reused for every chapter** | **no** |
| `OnGetImageURL`, `OnBeforeDownloadImage`, `OnDownloadImage`, `OnAfterImageSaved` | one per download worker thread | yes — before every image |

`OnGetPageNumber` is the one to watch: in a multi-chapter task, every chapter
after the first inherits the previous chapter's response headers. The same
applies inside any handler that makes **more than one request of its own** —
after your first request the object is "dirty" again, so reset before
setting the headers for the second one.

> [!IMPORTANT]
> Whenever you set `HTTP.Headers`, `HTTP.Cookies` or `HTTP.MimeType` before a
> request, call `HTTP.Reset()` first — unless you are certain the object has
> not been used since it was created.

`HTTP.Reset()` also clears the in-memory cookie list and closes the keep-alive
socket (a fresh connection is made on the next request). The module's shared
cookie storage is **not** touched — stored cookies are re-applied
automatically when the next request is made, so a logged-in session survives
a `Reset()`. Which properties survive a reset is listed in the note under [HTTP Methods](#http-methods).

#### Cookie Storage and Persistence

Every handler — including `Login()` — gets its own `HTTP` object, but all of
them share **one cookie storage per module**. `HTTP.AddServerCookies`,
`HTTP.ClearCookiesStorage`, `HTTP.ParseServerCookies` and the `MODULE.*`
cookie methods all read and write that same storage.

```lua
-- the same storage, reached two ways:
HTTP.AddServerCookies('https://example.com/', 'session=abc; path=/; max-age=2592000')
MODULE.AddServerCookies('example.com', 'session=abc; path=/; max-age=2592000')
```

- While `HTTP.EnabledCookies` is `true` (the default), stored cookies are
  matched by domain and path and attached automatically as a `Cookie:` header
  on every request. They also survive `HTTP.Reset()`.
- `HTTP.AddServerCookies(url, cookie_string)` derives the domain and path from
  `url`. The first argument may be omitted — but then the cookie is stored with
  an **empty domain**, which matches no host, so it is never sent back (unless
  the cookie string itself carries a `domain=` attribute). Prefer the
  two-argument form, or `MODULE.AddServerCookies(domain, cookie_string)`,
  which takes the domain directly.
- Several cookies can be passed at once, separated by newlines. Recognized
  attributes: `domain`, `path`, `expires`, `max-age`, `secure`, `httponly`,
  `samesite` — anything else is ignored.
- **A cookie is written to disk only when its string contains `expires=` or
  `max-age=`** (a *persistent* cookie in RFC 6265 terms). Without either
  attribute it is a *session* cookie: it lives in memory for the current run
  and is lost when the app restarts — even though `AddServerCookies` was used.
- Persistent cookies are saved to `modules.json` under the module's `ID`,
  together with the account and the user options, and are reloaded on startup.

The cookie storage can only ever produce a `Cookie:` header — it cannot feed
an `Authorization: Bearer ...` header. For token-based APIs keep the token in
`MODULE.Account.Cookies` instead (see
[MODULE.Account Properties](#moduleaccount-properties)).

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

A full example is in [`OnGetInfo`](#ongetinfo).

#### MANGAINFO Properties

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

### TASK Object (TTaskContainer)

Current download task information.

```lua
function GetPageNumber()
    TASK.PageLinks.Add('https://example.com/image1.jpg')
    TASK.PageLinks.Add('https://example.com/image2.jpg')
    return true
end
```

A full example is in [`OnGetPageNumber`](#ongetpagenumber).

#### TASK Properties

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
[`OnGetNameAndLink`](#ongetnameandlink).

#### LINKS and NAMES Methods

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

#### UPDATELIST Properties

| Property | Type | Description |
|----------|------|-------------|
| `UPDATELIST.CurrentDirectoryPageNumber` | Integer (read/write) | Current page in manga list |

#### UPDATELIST Methods

| Method | Description |
|--------|-------------|
| `UPDATELIST.UpdateStatusText(text)` | Update status message in UI |

### Storage and Persistence

What survives an app restart — and what does not:

| Data | Persisted? | Where |
|------|-----------|-------|
| `MODULE.Account.Username` / `.Password` / `.Cookies` | yes | `modules.json`, encrypted, keyed by the module `ID` |
| `MODULE.Account.Status` | yes | `modules.json` |
| user options (`MODULE.GetOption`) and module settings | yes | `modules.json` |
| cookies stored with an `expires=` / `max-age=` attribute | yes | `modules.json` |
| cookies stored without an expiry (session cookies) | no | memory only |
| `MODULE.Storage` key/value pairs | **no** | memory only |
| `HTTP` state (headers, document, `HTTP.Cookies`) | no | memory only |

`MODULE.Storage` is a plain in-memory key-value store — handy for caching
within one run of the application, but nothing in it is written to disk, so it
starts empty after every restart. There is **one storage per module**: every
handler call and every running task of the website shares the same keys, so
concurrent downloads from the same site see and overwrite each other's
entries.

Values are always stored and returned as **strings** — a number is converted
on write, so convert it back when reading:

```lua
MODULE.Storage['last_update'] = os.time()             -- gone after a restart
local last = tonumber(MODULE.Storage['last_update'])  -- reads back as a string
```

Reading a key that was never set returns the **empty string**, never `nil` —
and storing a boolean or a table yields `''` as well:

```lua
if MODULE.Storage['token'] ~= '' then  -- check for '', not for nil
    ...
end
```

`MODULE.Storage` also exposes:

| Member | Description |
|--------|-------------|
| `.Text` | Get or set the whole storage as `name=value` lines — `MODULE.Storage.Text = ''` drops every key at once |
| `.Remove(name)` | Delete a single key (a no-op when the key does not exist) |
| `.Tag`, `.Enable`, `.Status` | Spare read/write fields (integer, boolean, string), also memory-only |

Real usage — cache the rotating key of the site and drop the whole cache when
it goes stale (`lua/modules/CapibaraTraductor.lua`):

```lua
if MODULE.Storage.Text == '' then  -- nothing cached yet
    -- fetch the chapter page and store its key variables
    for name, hex in HTTP.Document.ToString():gmatch('([%w_$]+)%s*=%s*"([0-9a-f]+)"') do
        if #hex == 24 then MODULE.Storage[name] = hex end
    end
end
...
MODULE.Storage.Text = ''  -- the key did not match, clear everything
```

Single writes are internally locked, but reads are not — a read-modify-write
sequence is not atomic. When several tasks can touch the same keys at the same
time, guard the whole sequence with `MODULE.Guardian`:

```lua
MODULE.Guardian.Enter()
local n = tonumber(MODULE.Storage['count']) or 0
MODULE.Storage['count'] = n + 1
MODULE.Guardian.Leave()
```

For data that must survive a restart, use the account fields or a persistent
cookie:

```lua
-- an auth token: saved encrypted and reloaded on startup
MODULE.Account.Cookies = token

-- a cookie the server accepts: the expiry attribute is what makes it persist
HTTP.AddServerCookies(MODULE.RootURL, 'session=abc; path=/; max-age=2592000')
```

---

## 7. Parsing and Helper Functions

Global functions available in every handler. `CreateTXQuery` is the main tool for reading HTML, XML and JSON; the utility functions cover common string and URL chores.

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
| `x.ParseHTML(html)` | Parse an HTML string or stream, replacing the previous tree. An empty string is ignored (the old tree is kept) |
| `x.XPath(xpath)` | Get a value/node object (a sequence) for iteration |
| `x.XPath(xpath, context)` | Same, evaluated against `context` |
| `x.XPathString(xpath)` | Get the result as one string |
| `x.XPathString(xpath, context)` | Same, but evaluated against `context` (a value returned by `x.XPath(...)`, e.g. parsed JSON) instead of the whole document |
| `x.XPathStringAll(xpath)` | Get all matching items as one string, joined with `", "` |
| `x.XPathStringAll(xpath, separator)` | Same, with a custom separator |
| `x.XPathStringAll(xpath, context)` | Same as `XPathStringAll(xpath)` (`", "` separator), evaluated against `context` |
| `x.XPathStringAll(xpath, separator, context)` | Custom separator **and** a context |
| `x.XPathStringAll(xpath, list)` | Append all matches to a `TStrings` list (`TASK.PageLinks`, `LINKS`, a `fmd.strings` list, …) |
| `x.XPathStringAll(xpath, list, context)` | Same, evaluated against `context` |
| `x.XPathHREFAll(xpath, linksList, namesList)` | Extract `href` and text (trimmed) to lists |
| `x.XPathHREFAll(xpath, linksList, namesList, context)` | Same, evaluated against `context` |
| `x.XPathHREFTitleAll(xpath, linksList, namesList)` | Extract `href` and `title` attribute to lists |
| `x.XPathHREFTitleAll(xpath, linksList, namesList, context)` | Same, evaluated against `context` |
| `x.XPathCount(xpath)` | Count matching items |
| `x.XPathCount(xpath, context)` | Same, evaluated against `context` |

The optional **context** is always the **last** argument and must be a value
object returned by `x.XPath(...)`. The expression is then evaluated relative to
that value instead of the whole document; see
[Parsing once, querying many times](#parsing-once-querying-many-times).

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

-- All values of a field as one string: "A, B" (lookup style)
MANGAINFO.Genres = x.XPathStringAll('json(*)?data?items?*?name')

-- Same thing, dot style
MANGAINFO.Genres = x.XPathStringAll('json(*).data.items().name')

-- All values of a field appended to a list (e.g. TASK.PageLinks)
x.XPathStringAll('json(*)?data?items?*?url', TASK.PageLinks)

-- Using parse-json
local title2 = x.XPathString('parse-json(.)?data?title')

-- XPathStringAll already joins with ", ", so string-join() is not needed
local names = x.XPathStringAll('json(*)?data?items?*?name')            -- "A, B"
local slash = x.XPathStringAll('json(*)?data?items?*?name', ' / ')    -- "A / B"

-- XPathString concatenates without a separator, so string-join() is needed there
local joined = x.XPathString('string-join(json(*)?data?items?*?name, ", ")')
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

-- Several values from an array: the context is the last argument
local all = x.XPathStringAll('data?items?*?name', json)         -- "A, B"
local sep = x.XPathStringAll('data?items?*?name', ' / ', json)  -- "A / B"
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
local x = CreateTXQuery(HTTP.Document)
local json = x.XPath('json(*)')

-- jn:keys(object)    -> the keys of a JSON object
-- jn:members(array)  -> the members of a JSON array
local count = x.XPathCount('jn:keys(data)', json)
for v in x.XPath('jn:members(data?items)', json).Get() do ... end
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

## 8. Libraries

Load a library with `require`. `fmd.*` libraries are implemented natively by the host application; `utils.*` libraries are Lua (or JavaScript) files shipped in `lua/utils/`.

| Library | Purpose |
|---------|---------|
| [`fmd.crypto`](#fmdcrypto) | Hashing, HMAC, AES, RC4, key exchange, Base64/URL/HTML/hex encoding, checksums |
| [`fmd.duktape`](#fmdduktape) | Run JavaScript (Duktape) |
| [`fmd.env`](#fmdenv) | FMD2 paths, version and language |
| [`fmd.fileutil`](#fmdfileutil) | File name helpers |
| [`fmd.logger`](#fmdlogger) | Write to the FMD2 log |
| [`fmd.subprocess`](#fmdsubprocess) | Run external programs |
| [`fmd.mangafoxwatermark`](#fmdmangafoxwatermark) | Remove the FanFox/MangaHere watermark |
| [`fmd.gzip`](#fmdgzip) | Inflate gzip/zlib/deflate data |
| [`fmd.pcre2`](#fmdpcre2) | PCRE2 regular expressions |
| [`fmd.imagepuzzle`](#fmdimagepuzzle) | Descramble tile-shuffled images |
| [`fmd.strings`](#fmdstrings) | `TStringList` objects |
| [`utils.json`](#utilsjson) | JSON encode/decode in Lua |
| [`utils.lzstring`](#utilslzstring) | LZ-String decompression |
| [`utils.jsunpack`](#utilsjsunpack) | `p,a,c,k,e,d` string-table unpacker |
| [`utils.nodejs`](#utilsnodejs) | Render JS-heavy pages through Node.js |
| [`utils.nextjs`](#utilsnextjs) | Extract data from Next.js flight payloads |
| [`utils.sha256`](#utilssha256) | Pure-Lua SHA-256 (temporary) |
| [`utils.protoc`](#utilsprotoc-with-the-pb-runtime) | Protocol Buffers |

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

JavaScript execution engine (Duktape 2.7.0; ES5.1 plus parts of ES6).

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

## 9. Templates

Templates are reusable module components that handle common website patterns.

### Available Templates

41 templates ship in `lua/templates/` — `require` them by file name:

| Template | Common for |
|----------|------------|
| `Madara` | Madara-theme WordPress sites (largest family) |
| `MangaThemesia` | MangaThemesia / WP Manga reader themes |
| `FMReader` | FMReader-based sites |
| `HeanCMS` | HeanCMS sites |
| `MadTheme` | MadTheme sites |
| `Genkan` | Genkan (Kemono-style reader) sites |
| `FoOlSlide` | FoOlSlide-powered scanlators |
| `MangaHub` | MangaHub themes |
| `MangaEsp` | Spanish MangaES theme |
| `WPComics` | WPComics themes |
| `KeyoApp` | Keyoapp sites |
| `Hiper` | Hiper themes |
| `MangAdventure` | MangAdventure instances |
| `Guya` | Guya.moe-style readers |
| `SchaleNetwork` | SchaleNetwork sites |
| `SinMH` | SinMH (Chinese) sites |
| `MangaBox` | MangaBox (mobile reader) sites |
| `NineManga`, `MultiChan`, `ZManga`, `ZeroTheme`, `VTheme`, `SpicyTheme`, `MangaK`, `MangaTaro`, `MangaReaderOnline`, `MangaCatalog`, `Liliana`, `LibGroup`, `KiruBase`, `GroupLe`, `GoDa`, `EZManga`, `DataLife`, `ColorlibAnime`, `NiAdd`, `NovelCool`, `NovelsHub`, `Roseveil`, `SPA`, `FuzzyDoodle` | assorted smaller CMS families |

Pick the closest match, then override the module-level constants it exposes
(see [Template Customization](#template-customization)).

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

## 10. Cookbook

Short, proven patterns for situations that come up in many modules.

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

### Headers, Referer and Origin

Many sites require specific headers (usually `Referer` and `Origin`). Set them in
`OnBeforeDownloadImage`; see [`OnBeforeDownloadImage`](#onbeforedownloadimage).
For JSON APIs, see [JSON API Handling](#json-api-handling).

**Always call `HTTP.Reset()` before setting custom headers** — especially in
`OnGetPageNumber`. The reason is explained in
[HTTP.Reset() and Header Lifecycle](#httpreset-and-header-lifecycle).

### Sites That Need JavaScript

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

### Skipping Locked or Premium Chapters

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

---

## 11. Complete Examples

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
    m.ID                       = 'a1b2c3d4e5f60718293a4b5c6d7e8f90'
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
    MANGAINFO.Genres    = x.XPathStringAll('genres?*?name', data)
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
    m.ID                       = '0f9e8d7c6b5a4938271605f4e3d2c1b0'
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
    -- Login() is NOT called automatically: the user triggers it with the check button in
    -- the Account Manager. Cookies set by the login response are kept in the module's
    -- shared cookie storage and sent with later requests.
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
    m.ID                       = '1a2b3c4d5e6f708192a3b4c5d6e7f809'
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

## 12. Development and Debugging

### Reloading Modules While Developing

When testing modules, use the `--lua-dofile` command-line argument to reload Lua files on each request instead of using cached bytecode:

```bash
FMD2.exe --lua-dofile
```

### Logging

```lua
print('Debug: ' .. variable)

local logger = require 'fmd.logger'
logger.Send('Info message')
logger.SendError('Error: ' .. error_message)
```

### Common Pitfalls

| Symptom | Cause and fix |
|---------|---------------|
| Custom headers work for the first chapter only | `OnGetPageNumber` reuses one `HTTP` object for every chapter. Call `HTTP.Reset()` before setting headers: [HTTP.Reset() and Header Lifecycle](#httpreset-and-header-lifecycle) |
| `OnGetImageURL` cannot find the page | `URL` is the **chapter** URL there. Use `TASK.PageContainerLinks[WORKID]`: [`OnGetImageURL`](#ongetimageurl) |
| Changing a return value has no effect | Several handlers' return values are ignored by the host: [Handler Reference](#handler-reference) |
| `HTTP` is not available in a handler | `HTTP` is not injected in `OnAccountState`, `OnCheckSite`, `OnAfterImageSaved` and `OnTaskStart` |
| `MODULE.Account` raises an error | The module did not set `AccountSupport = true` |
| `MODULE.Storage[...]` is empty after a restart | It is memory-only and reads missing keys as `''`: [Storage and Persistence](#storage-and-persistence) |
| A saved cookie is gone after a restart | Only cookies with `expires=`/`max-age=` are persisted: [Cookie Storage and Persistence](#cookie-storage-and-persistence) |
| Library call returns `''` | Crypto functions return `''` on malformed input: [`fmd.crypto`](#fmdcrypto) |

---

## Appendix: File Locations and Resources

```
FMD2/
├── lua/
│   ├── modules/          # Website modules (.lua)
│   ├── templates/        # Shared templates (.lua)
│   ├── utils/            # Utility libraries (.lua, .js)
│   ├── extras/           # Additional helpers
│   └── websitebypass/    # Cloudflare / anti-bot bypass scripts
├── baseunits/
│   └── lua/              # Pascal Lua integration files (.pas)
└── docs/
    └── SUPPORTED_WEBSITES.md
```

- **Supported websites:** `docs/SUPPORTED_WEBSITES.md`
- **Example modules:** `lua/modules/`
- **Templates:** `lua/templates/`
- **Host bindings (source of truth for the Lua API):** `baseunits/lua/*.pas`