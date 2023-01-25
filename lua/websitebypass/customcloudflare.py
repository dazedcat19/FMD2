from playwright.sync_api import sync_playwright
from cf_clearance import sync_cf_retry, sync_stealth
import requests
import sys
import json

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