import sys
import rookiepy
import time

def formatCookies(cooks):
    result = []

    for cookie in cooks:
        if cookie["value"] == "":
            continue

        if cookie["expires"]:
            if cookie["expires"] < round(time.time()):
                continue
        
        result.append({cookie["name"]: cookie["value"]})
    
    return result

def tryBrowsers(url):
    cookies = []
    user_agent = [{"user_agent": ""}]
    url = f'{url.replace("https://", "")}'
    errors = "Error: No browser cookies/working cookies found."
    browsers = ["chrome", "edge", "firefox", "opera", "opera_gx"] #Re-arrange according to your browser prefrence
    rookiepyFuncs = {"chrome": rookiepy.chrome,
        "edge": rookiepy.edge,
        "firefox": rookiepy.firefox,
        "opera": rookiepy.opera,
        "opera_gx": rookiepy.opera_gx
        }
    defaultUA = {"chrome": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36",
        "edge": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36 Edg/129.0.0.0",
        "firefox": "Mozilla/5.0 (Windows NT 10.0; Win64; x64rv:131.0) Gecko/20100101 Firefox/131.0",
        "opera": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36 OPR/113.0.0.0",
        "opera_gx": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36 OPR/113.0.0.0"
        }

    for browser in browsers:
        try:
            if not cookies:
                cookies = rookiepyFuncs[browser]([url])
                cookies = formatCookies(cookies)
                user_agent[0]["user_agent"] = defaultUA[browser]
        except:
            continue

    if not cookies:
        return errors
    
    cookies.extend(user_agent)
    return cookies

url = sys.argv[1]

print(tryBrowsers(url))