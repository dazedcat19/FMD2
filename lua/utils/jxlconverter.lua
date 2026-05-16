local json   = require("utils.json")
local crypto = require('fmd.crypto')
local nodejs = require("utils.nodejs")

local jxlconverter_executor = {}
local debugging = false
local b64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

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
        local message = "Utils[JXL_Converter]: " .. safe_concat(...)
        print(message)
    end
end

local function b64_encode(data)
    local parts = {}
    local i = 1
    while i <= #data do
        local a = string.byte(data, i)
        local b = string.byte(data, i + 1)
        local c = string.byte(data, i + 2)
        if a and b and c then
            local n = a * 65536 + b * 256 + c
            parts[#parts + 1] = b64chars:sub(math.floor(n / 262144) % 64 + 1, math.floor(n / 262144) % 64 + 1)
            parts[#parts + 1] = b64chars:sub(math.floor(n / 4096) % 64 + 1, math.floor(n / 4096) % 64 + 1)
            parts[#parts + 1] = b64chars:sub(math.floor(n / 64) % 64 + 1, math.floor(n / 64) % 64 + 1)
            parts[#parts + 1] = b64chars:sub(n % 64 + 1, n % 64 + 1)
        elseif a and b then
            local n = a * 256 + b
            parts[#parts + 1] = b64chars:sub(math.floor(n / 1024) % 64 + 1, math.floor(n / 1024) % 64 + 1)
            parts[#parts + 1] = b64chars:sub(math.floor(n / 16) % 64 + 1, math.floor(n / 16) % 64 + 1)
            parts[#parts + 1] = b64chars:sub((n % 16) * 4 + 1, (n % 16) * 4 + 1)
            parts[#parts + 1] = "="
        else
            parts[#parts + 1] = b64chars:sub(math.floor(a / 4) + 1, math.floor(a / 4) + 1)
            parts[#parts + 1] = b64chars:sub((a % 4) * 16 + 1, (a % 4) * 16 + 1)
            parts[#parts + 1] = "=="
        end
        i = i + 3
    end
    return table.concat(parts)
end

local function b64_decode(str)
    str = str:gsub("%s", "")
    local parts = {}
    local i = 1
    while i < #str do
        local c1, c2, c3, c4 = str:sub(i, i), str:sub(i + 1, i + 1), str:sub(i + 2, i + 2), str:sub(i + 3, i + 3)
        if c1 == "" or c2 == "" then break end
        local v1 = b64chars:find(c1, 1, true) - 1
        local v2 = b64chars:find(c2, 1, true) - 1
        local v3 = (c3 ~= "=") and (b64chars:find(c3, 1, true) - 1) or 0
        local v4 = (c4 ~= "=") and (b64chars:find(c4, 1, true) - 1) or 0
        local n = v1 * 262144 + v2 * 4096 + v3 * 64 + v4
        parts[#parts + 1] = string.char(math.floor(n / 65536) % 256)
        if c3 ~= "=" then parts[#parts + 1] = string.char(math.floor(n / 256) % 256) end
        if c4 ~= "=" then parts[#parts + 1] = string.char(n % 256) end
        i = i + 4
    end
    return table.concat(parts)
end

local function __convert(jxl_data, convert_target)
    if not jxl_data then return handle_error("No JXL data was provided.") end
	convert_target = convert_target or 'png'
	
    debug_print('JXL data size:', #jxl_data)
    
    --Convering JXL data to base64
    debug_print('Convering JXL data to base64')
    local jxl_data64 = b64_encode(jxl_data)
    debug_print('JXL base64 data size:', #jxl_data64)
    
    --Writing js_code to pass to nodejs
    local js_code = [=[

var __input = {'convert_target': ']=] .. convert_target .. [=[', 'base64': ']=] .. jxl_data64 .. [=['};
var b64 = __input.base64;
const { join } = require('node:path');
const fs = require('fs');
var initJXLDecode = require('@jsquash/jxl/decode.js').init;
var jxl_decode = require("@jsquash/jxl").decode;
var sharp = require("sharp");

const SUPPORTED_TARGETS = ['png', 'jpg', 'jpeg', 'webp', 'gif'];
const FORMAT_META = {
    png:  { mime: 'image/png',  extension: 'png'  },
    jpg:  { mime: 'image/jpeg', extension: 'jpg'  },
    jpeg: { mime: 'image/jpeg', extension: 'jpg'  },
    webp: { mime: 'image/webp', extension: 'webp' },
    gif:  { mime: 'image/gif',  extension: 'gif'  },
};

function buf_to_arrybuf(buf) {
    const arrayBuffer = buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.byteLength);
    return arrayBuffer;
}

function fail(error_msg) {
    console.log(JSON.stringify({ status: 'Failed', error_msg }));
    process.exit(1);
}

const wasmPath = join(__dirname, 'node_modules/@jsquash/jxl/codec/dec/jxl_dec.wasm');
var buf = Buffer.from(b64, "base64");
var arrybuf = buf_to_arrybuf(buf);
b64 = null;

(async () => {
    try {
        const rawTarget = __input.convert_target;
        const target = SUPPORTED_TARGETS.includes(rawTarget) ? rawTarget : 'png';
        const meta = FORMAT_META[target];

        const wasmBuffer = fs.readFileSync(wasmPath);
        const wasmModule = await WebAssembly.compile(wasmBuffer);
        await initJXLDecode(wasmModule);

        const imageData = await jxl_decode(arrybuf);

        const rawBuffer = Buffer.from(imageData.data);
        const sharpImg = sharp(rawBuffer, {
            raw: { width: imageData.width, height: imageData.height, channels: 4 }
        });

        var out;
        if (target === 'png') {
            out = (await sharpImg.png().toBuffer()).toString("base64");
        } else if (target === 'jpg' || target === 'jpeg') {
            out = (await sharpImg.jpeg({ quality: 90 }).toBuffer()).toString("base64");
        } else if (target === 'webp') {
            out = (await sharpImg.webp({ quality: 90 }).toBuffer()).toString("base64");
        } else if (target === 'gif') {
            out = (await sharpImg.gif().toBuffer()).toString("base64");
        }

        console.log(JSON.stringify({
            status: 'Success',
            convert_target: target,
            mime: meta.mime,
            extension: meta.extension,
            width: imageData.width,
            height: imageData.height,
            base64: out,
        }));

    } catch (error) {
        fail(error.message || String(error));
    }
})();

]=]
        debug_print('js_code:', js_code)
        --clean variable jxl_data64
        jxl_data64 = nil
        debug_print('Start Running js_code in Nonejs')
        local output = nodejs.run_js(js_code, nil, nil, false)
        debug_print('Nodejs ouput size:',#output)
        debug_print('Nodejs ouput:',output)
        
        --clean variable js_code
        js_code = nil
        
        --Convering JXL data to base64
        debug_print('Convering JXL data to base64')
        if output and output ~= "" then
            --Parsing Nonejs output
            debug_print('Parsing Nonejs output')
            local jpg_data64 = json.decode(output)["base64"]
            debug_print('jpg base64 data size:', #jpg_data64)
          
            --Convering JPG base64 to data
            debug_print('Convering JPG base64 to data')
            local jpg_data = crypto.DecodeBase64(jpg_data64)
            debug_print('JPG data size:', #jpg_data)
          
            --clean variable output
            output = nil
          
            if jpg_data and jpg_data ~= "" then
                return jpg_data
            end
        end
    return true
end



-- Public functions
function jxlconverter_executor.convert(jxl_data, convert_target)
    convert_target = convert_target or 'png'
    debug_print('Start Convering JXL to ' .. string.upper(convert_target) .. '...')
    return __convert(jxl_data, convert_target)
end

return jxlconverter_executor