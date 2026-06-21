local node_executor = {}
local node_installed = false
local initialised = false
local debugging = false

-- Centralized error handling function
local function handle_error(message)
    return "Error: " .. (message or "An unknown error occurred.")
end

local function stringify(value)
    if type(value) == "table" then
        return "<table>"
    elseif type(value) == "userdata" then
        return "<userdata>"
    elseif type(value) == "function" then
        return "<function>"
    else
        return tostring(value)
    end
end

local function safe_concat(...)
    local args = {...}
    for i = 1, #args do
        args[i] = stringify(args[i]) -- Convert each element to a string
    end
    return table.concat(args, " ")
end

-- Centralized debugging function
local function debug_print(...)
    if debugging then
        local message = "Utils[NodeJS]: " .. safe_concat(...)
        print(message)
    end
end

local function run_command(...)
    debug_print("running command:", ...)
    local _status, result, _errors = require("fmd.subprocess").RunCommandHide("cmd.exe", "/c", ...)

    if not _status or _errors ~= "" or result:find("Error:") then
        debug_print(_errors ~= "" and _errors or result)
        return (_errors ~= "" and _errors or result)
    end

    return result
end

-- Preliminary check function for Node.js
local function initialise()
    if initialised then return node_installed and true or false, "Node.js is not installed or not available in the system path." end
    initialised = true

    if run_command("node", "-v"):match("v%d+%.%d+%.%d+") then
        node_installed = true
        debug_print("Node.JS is already installed")
    else
        return false, "Node.js is not installed or not available in the system path."
    end

    return true
end

-- Ensure the npm install directory exists, compatible with Windows
local function ensure_install_directory(install_dir)
    install_dir = install_dir:gsub("/", "\\")
    if os.rename(install_dir, install_dir) then return true end

    local output = run_command("mkdir", install_dir)
    if output:find("Access is denied") or output:find("The syntax of the command is incorrect") then
        debug_print("Failed to create directory. Command output:", output)
        return false, "Failed to create directory: " .. install_dir
    end

    return true
end

-- Check if a module is already installed using npm list
local function is_module_installed(mod, install_dir)
    local output = run_command("cd", install_dir, "&&", "npm", "list", mod)
    local cleaned_output = output:gsub("[%s%p]", "")  -- Removes spaces and punctuation
    local cleaned_mod = mod:gsub("%-", "")
    
    return cleaned_output:match(cleaned_mod) ~= nil
end

-- Install required modules from JavaScript code
local function install_required_modules(js_code)
    local install_dir = "lua/utils/npm"
    local success, err = ensure_install_directory(install_dir)
    if not success then debug_print(err) return false, err end

    local modules = {"puppeteer"}
    --for mod in js_code:gmatch("require%s*%(%s*['\"](.-)['\"]%s*%)") do -- auto install any npm modules required by the script
    for _, mod in pairs(modules) do
        if not is_module_installed(mod, install_dir) then
            run_command("cd", install_dir, "&&", "npm", "install", mod)
        else
            debug_print("Module is already installed:", mod)
        end
    end
    return true
end

-- Execute JavaScript code with Node.js
local function execute_js_script(js_code)
    local success, err = initialise()
    if not success then return handle_error(err) end

    success, err = install_required_modules(js_code)
    if not success then return handle_error(err) end

    local js_file = "lua/utils/npm/tmp_scrpt.js"
    local file = io.open(js_file, "w")
    file:write(js_code)
    file:close()

    local output = run_command("node", js_file)
    os.remove(js_file)

    return output
end

local function isolatevm_js(js_code, pass_page)
    if not js_code then return handle_error("No JavaScript code provided.") end

    if pass_page then return js_code end

    local safe_js_code = string.format("%q", js_code)

    return [[
    const vm = require('vm');
    (async () => {
        const sandbox = {
            console: {
                log: (...args) => { console.log(...args); },
                error: (...args) => { console.error(...args); }
            }
        };
        vm.createContext(sandbox);

        const jsCode = ]] .. safe_js_code .. [[;

        try {
            const p = vm.runInContext('(async () => { ' + jsCode + ' })()', sandbox);
            await Promise.race([
                p,
                new Promise((_, reject) =>
                    setTimeout(() => reject(new Error('Execution timed out')), 5000)
                )
            ]);
        } catch (error) {
            console.error("VM Execution Error:", error.message);
        }
    })();
    ]]
end

-- Helper function to fetch FlareSolverr configuration
local function get_flaresolverr_config()
    local ip = "127.0.0.1"
    local port = 8191
    local use_fs = false
    local config_json = "lua/websitebypass/websitebypass_config.json"
    
    local file = io.open(config_json, "r")
    if file then
        local content = file:read("*a")
        file:close()
        
        local json = require "utils.json"
        local config_table = json.decode(content)
        ip = config_table['flaresolverr_ip'] or ip
        port = tonumber(config_table['flaresolverr_port']) or port
        use_fs = config_table['use_webdriver'] or false
    end
    return ip, port, use_fs
end

-- Helper functions to read/write persistent session data
local function get_stored_data()
    local cookies = "[]"
    local ua = "null"
    
    if MODULE.Storage["puppeteer_cookies"] ~= "" then
        cookies = MODULE.Storage["puppeteer_cookies"]
    end
    if MODULE.Storage["puppeteer_ua"] ~= "" then
        ua = string.format("%q", MODULE.Storage["puppeteer_ua"])
    end
    return cookies, ua
end

local function save_stored_data(cookies_json, ua_str)
    MODULE.Storage["puppeteer_cookies"] = cookies_json
    MODULE.Storage["puppeteer_ua"] = ua_str
end

-- Function to load HTML content and optionally execute JavaScript on it
local function run_html_with_js(url, js_code)
    local fs_ip, fs_port, use_fs = get_flaresolverr_config()
    local stored_cookies, stored_ua = get_stored_data()

    -- ALWAYS attempt to use active FMD2 session tokens first
    if HTTP.UserAgent ~= "" then
        stored_ua = string.format("%q", HTTP.UserAgent)
    end

    local cf = HTTP.Cookies.Values["cf_clearance"]
    if cf ~= "" then
        local domain = url:match("https?://([^/]+)")
        if domain then
            local json = require "utils.json"
            stored_cookies = json.encode({{
                name = "cf_clearance",
                value = cf,
                domain = domain,
                path = "/"
            }})
        end
    end

    local setup_js = [[
        const puppeteer = require('puppeteer');
        const http = require('http');
        const fs = require('fs');

        (async () => {
            const url = "]] .. url .. [[";
            const fsHost = "]] .. fs_ip .. [[";
            const fsPort = ]] .. fs_port .. [[;
            const useFS = ]] .. tostring(use_fs) .. [[;

            let currentCookies = ]] .. stored_cookies .. [[;
            let currentUA = ]] .. stored_ua .. [[;

            const fsRequest = () => new Promise(resolve => {
                const payload = JSON.stringify({ cmd: 'request.get', url: url, maxTimeout: 60000 });
                const req = http.request({
                    hostname: fsHost,
                    port: fsPort,
                    path: '/v1',
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json', 'Content-Length': Buffer.byteLength(payload) }
                }, res => {
                    let body = '';
                    res.on('data', d => body += d);
                    res.on('end', () => {
                        try { resolve(JSON.parse(body)); } catch(e) { resolve(null); }
                    });
                });
                req.on('error', () => resolve(null));
                req.write(payload);
                req.end();
            });

            let browser;
            try {
                browser = await puppeteer.launch({
                    headless: true,
                    args: [
                        '--disable-extensions', // Prevent browser extensions
                        '--disable-webgl', // Disable WebGL
                        '--disable-webrtc', // Disable WebRTC
                        '--disable-background-networking' // Prevents background requests
                    ]
                });
                const page = await browser.newPage();
                await page.setBypassCSP(false); // Enforce CSP

                if (currentUA) {
                    await page.setUserAgent(currentUA);
                }
                if (currentCookies && currentCookies.length > 0) {
                    await page.setCookie(...currentCookies);
                }

                let response;
                try {
                    response = await page.goto(url, { waitUntil: "domcontentloaded", timeout: 90000 });
                } catch (navError) {
                    console.log("Navigation error:", navError.message);
                }

                let status = response ? response.status() : 200;
                let title = await page.title();
                
                // Determine if active FMD2 credentials hit a wall
                let isCloudflare = [503, 403, 429].includes(status) || /Just a moment|Attention Required|Cloudflare/i.test(title);

                if (isCloudflare && useFS) {
                    const fsData = await fsRequest();
                    
                    if (fsData && fsData.status === 'ok') {
                        currentUA = fsData.solution.userAgent;
                        currentCookies = fsData.solution.cookies;

                        if (currentUA) await page.setUserAgent(currentUA);
                        if (currentCookies && currentCookies.length > 0) {
                            await page.setCookie(...currentCookies);
                        }

                        try {
                            await page.goto(url, { waitUntil: "domcontentloaded", timeout: 90000 });
                        } catch (navError) {
                            console.log("Re-navigation error:", navError.message);
                        }
                    } else {
                        console.log("FlareSolverr failed or unreachable. Proceeding with existing structure...");
                    }
                }

                ]] .. (js_code and js_code or [[
                const content = await page.content();
                console.log(content);
                ]]) .. [[

                const finalCookies = await page.cookies();
                const finalUA = currentUA || (await browser.userAgent());
                
                fs.writeFileSync('lua/utils/npm/tmp_cookies.json', JSON.stringify({
                    cookies: finalCookies,
                    ua: finalUA
                }));

            } catch (error) {
                console.log(JSON.stringify({ error: error.toString() }));
                process.exit(1); // Exit the process with error code
            } finally {
                if (browser) {
                    await browser.close();
                }
            }
        })();
    ]]

    local output = execute_js_script(setup_js)

    local cookie_file_path = "lua/utils/npm/tmp_cookies.json"
    local cookie_file = io.open(cookie_file_path, "r")
    if cookie_file then
        local content = cookie_file:read("*a")
        cookie_file:close()
        os.remove(cookie_file_path)
        
        local json = require "utils.json"
        local parsed = json.decode(content)
        if parsed and parsed.cookies then
            save_stored_data(json.encode(parsed.cookies), parsed.ua)
        end
    end

    return output
end

-- Public functions
function node_executor.run_js(js_code)
    return execute_js_script(isolatevm_js(js_code))
end

function node_executor.run_html_load(url)
    return run_html_with_js(url)
end

function node_executor.run_html_load_with_js(url, js_code)
    return run_html_with_js(url, isolatevm_js(js_code, true))
end

return node_executor