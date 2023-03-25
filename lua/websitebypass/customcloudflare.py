from __future__ import print_function
from subprocess import call

def installPip(log=print):
    """
    Pip is the standard package manager for Python. Starting with Python 3.4
    it's included in the default installation, but older versions may need to
    download and install it. This code should pretty cleanly do just that.
    """
    log("Installing pip, the standard Python Package Manager, first")
    from os     import remove
    from urllib.request import urlretrieve
    urlretrieve("https://bootstrap.pypa.io/get-pip.py", "get-pip.py")
    call(["python", "get-pip.py"])

    # Clean up now...
    remove("get-pip.py")

def getPip(log=print):
    """
    Pip is the standard package manager for Python.
    This returns the path to the pip executable, installing it if necessary.
    """
    from os.path import isfile, join
    from sys     import prefix
    # Generate the path to where pip is or will be installed... this has been
    # tested and works on Windows, but will likely need tweaking for other OS's.
    # On OS X, I seem to have pip at /usr/local/bin/pip?
    pipPath = join(prefix, 'Scripts', 'pip.exe')

    # Check if pip is installed, and install it if it isn't.
    if not isfile(pipPath):
        installPip(log)
        if not isfile(pipPath):
            raise RuntimeError("Failed to find or install pip!")
    return pipPath

def installIfNeeded(moduleName, nameOnPip=None, notes="", log=print):
    """ Installs a Python library using pip, if it isn't already installed. """
    from pkgutil import iter_modules

    # Check if the module is installed
    if moduleName not in [tuple_[1] for tuple_ in iter_modules()]:
        log("Installing " + moduleName + notes + " Library for Python")
        call([getPip(log), "install", nameOnPip if nameOnPip else moduleName])
    if moduleName == "playwright":
        call("playwright install")

import sys
import json
installIfNeeded("playwright", "playwright")
from playwright.sync_api import sync_playwright
installIfNeeded("cf_clearance", "cf_clearance")
from cf_clearance import sync_cf_retry, sync_stealth
installIfNeeded("requests", "requests")
import requests

url = sys.argv[1]
headless = sys.argv[2].lower() == "true"
res = requests.get(url)
if "<title>Please Wait... | Cloudflare</title>" in res.text:
    print("cf challenge fail")
# get cf_clearance
with sync_playwright() as p:
    browser = p.chromium.launch(headless=headless)
    page = browser.new_page()
    sync_stealth(page, pure=True)
    page.goto(url)
    res = sync_cf_retry(page)
    if res:
        result = []
        cookies = page.context.cookies()
        for cookie in cookies:
            result.append({cookie.get("name"): cookie.get("value")})
        ua = page.evaluate("() => {return navigator.userAgent}")
        # add user agent as additional cookie
        result.append({"user_agent": ua})
        # encode cookies as JSON object and print to console
        cookies_json = json.dumps(result)
        print(cookies_json)
    else:
        print("cf challenge fail")
    browser.close()