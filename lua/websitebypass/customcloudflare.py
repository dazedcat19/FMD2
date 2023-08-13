import requests
import sys
import json

url = sys.argv[1]
post_body = {
  "cmd": "request.get",
  "url": url,
  "returnOnlyCookies": True,
  "maxTimeout": 60000
}

response = requests.post('http://localhost:8191/v1', headers={'Content-Type': 'application/json'}, json=post_body)

if response.status_code == 200:
    json_response = response.json()
    if json_response.get('status') == 'ok':
        result = []

        ## Get Cookies & Clean
        cookies = json_response['solution']['cookies']
        for cookie in cookies:
            result.append({cookie.get("name"): cookie.get("value")})

        ## Get User-Agent
        result.append({"user_agent": json_response['solution']['userAgent']})

        print(json.dumps(result))
else:
    print(response.json().get('message'))