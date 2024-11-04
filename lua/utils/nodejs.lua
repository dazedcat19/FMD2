local node_executor = {}
local node_installed = false
local initialised = false
local debugging = false

-- Centralized error handling function
local function handle_error(message)
    return "Error: " .. (message or "An unknown error occurred.")
end

-- Centralized debugging function
local function debug_print(...)
    if debugging then
        local message = table.concat({...}, " ")
        print(message)
    end
end

local function run_command(...)
    debug_print("Utils[NodeJS]: running command:", ...)
    local _status, result, _errors = require("fmd.subprocess").RunCommandHide("cmd.exe", "/c", ...)

    if not _status or _errors ~= "" or result:find("Error") then
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
local function is_module_installed(module, install_dir)
    return run_command("cd", install_dir, "&&", "npm", "list", module):find(module) ~= nil
end

-- Install required modules from JavaScript code
local function install_required_modules(js_code)
    local install_dir = "lua/utils/npm"
    local success, err = ensure_install_directory(install_dir)
    if not success then debug_print(err) return false, err end

    for module in js_code:gmatch("require%s*%(%s*['\"](.-)['\"]%s*%)") do
        if not is_module_installed(module, install_dir) then
            run_command("cd", install_dir, "&&", "npm", "install", module)
        else
            debug_print("Module already installed:", module)
        end
    end
    return true
end

-- Execute JavaScript code with Node.js
local function execute_js_script(js_code)
    local success, err = initialise()
    if not success then return handle_error(err) end

    if not js_code then return handle_error("No JavaScript code provided.") end

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

-- Function to load HTML content and optionally execute JavaScript on it
local function run_html_with_js(url, js_code)
    local timeout = 10000

    local setup_js = [[
        const puppeteer = require('puppeteer');

        (async () => {
            const url = "]] .. url .. [[";

            try {
                const browser = await puppeteer.launch({ headless: true });
                const page = await browser.newPage();
                await page.goto(url, { waitUntil: "networkidle2" });

                // Execute the custom JavaScript if provided, with a timeout
                ]] .. (js_code and [[
                // Function to execute js_code with a timeout
                async function executeWithTimeout(fn, timeout) {
                    return new Promise((resolve, reject) => {
                        const timer = setTimeout(() => {
                            reject(new Error("Custom JavaScript execution timed out"));
                        }, timeout);

                        fn().then((result) => {
                            clearTimeout(timer);
                            resolve(result);
                        }).catch((error) => {
                            clearTimeout(timer);
                            reject(error);
                        });
                    });
                }

                try {
                    const result = await executeWithTimeout(() => page.evaluate(() => {
                        return (function() {]] .. js_code .. [[})();
                    }), ]] .. timeout .. [[);
                    console.log(result);
                } catch (error) {
                    console.error("Error or timeout in custom JavaScript execution:", error);
                }
                ]] or [[
                const content = await page.content();
                console.log(content);
                ]]) .. [[

                await browser.close();
            } catch (error) {
                console.error('Error:', error);
            }
        })();
    ]]

    return execute_js_script(setup_js)
end

-- Public functions
function node_executor.run_js(js_code)
    return execute_js_script(js_code)
end

function node_executor.run_html_load(url)
    return run_html_with_js(url)
end

function node_executor.run_html_load_with_js(url, js_code)
    -- TODO: properly integrate custom js into page
    return run_html_with_js(url, js_code)
end

return node_executor