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

    modules = {"puppeteer"}
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

-- Function to load HTML content and optionally execute JavaScript on it
local function run_html_with_js(url, js_code)
    local setup_js = [[
        const puppeteer = require('puppeteer');

        (async () => {
            const url = "]] .. url .. [[";

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

                try {
                    // Attempt to navigate to the page with a 10-second timeout
                    await page.goto(url, { waitUntil: "networkidle2", timeout: 60000 });
                } catch (navError) {
                    console.error("Navigation error or timeout:", navError);
                    throw navError; // Re-throw to handle in outer catch
                }

                ]] .. (js_code and js_code or [[
                const content = await page.content();
                console.log(content);
                ]]) .. [[

            } catch (error) {
                console.error('Unhandled error in script:', error);
                process.exit(1); // Exit the process with error code
            } finally {
                if (browser) {
                    await browser.close();
                }
            }
        })();
    ]]

    return execute_js_script(setup_js)
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