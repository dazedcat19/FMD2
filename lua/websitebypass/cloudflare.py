import requests
import sys
import json
import logging
import base64
from pathlib import Path
from datetime import datetime, timedelta
from urllib.parse import urlparse

try:
    import rookiepy
    ROOKIEPY_AVAILABLE = True
except (ImportError, FileNotFoundError):
    ROOKIEPY_AVAILABLE = False
    

logging.basicConfig(filename=f'{Path(__file__).stem}.log', filemode='a',
                    format='%(asctime)s - %(levelname)s - %(message)s',
                    level=logging.INFO)

class CloudSolver:
    def __init__(self, debug=False):
        self.debug = debug
        # just random UUID
        self.session_id = 'e69e9ce7-8118-470c-a069-13af7bee6e8b'
        self.post_body = {"cmd": "request.get",
                          "session": self.session_id}
        if self.debug:
            self.post_body.update(
                {"returnScreenshot": True}
                )
        else:
            self.post_body.update(
                {"returnOnlyCookies": True}
                )

    def try_to_clear_sessions(self):
        temp_cloudflare = Path('.') / 'temp_cloudflare.json'
        if not temp_cloudflare.exists():
            cloudflare_json = {'next_run':(datetime.now() - timedelta(seconds=1)).timestamp()}
        else:
            cloudflare_json = json.loads(temp_cloudflare.read_text())

        if cloudflare_json.get('next_run') < datetime.now().timestamp():
            requests.post(
                'http://localhost:8191/v1',
                headers= {'Content-Type': 'application/json'},
                json= {"cmd": "sessions.destroy",
                       "session": self.session_id}
                )
            cloudflare_json = {'next_run':(datetime.now() + timedelta(hours=1)).timestamp()}

            with open(temp_cloudflare, 'w') as temp_file:
                temp_file.write(json.dumps(cloudflare_json))
        
    def solve_flare(self, url_):
        try:
            test_flaresolver = requests.get('http://127.0.0.1:8191/')
            if test_flaresolver.status_code == 200:
                if test_flaresolver.json()['msg'] != 'FlareSolverr is ready!':
                    logging.info('FlareSolverr is not running!')
                    return 404, 'FlareSolverr is not running!'
        except requests.exceptions.ConnectionError:
            return 404, 'FlareSolverr is not running!'
        logging.info(url_)
        post_body = self.post_body
        post_body['url'] = url_
        response = requests.post(
            'http://localhost:8191/v1',
            headers={'Content-Type': 'application/json'},
            json=post_body)
        logging.info(response.status_code)
        json_response = response.json()
        if response.status_code == 200:
            if self.debug:
                with open('json_response.json', 'w') as json_file:
                    json.dump(json_response , json_file, indent=4)
            if json_response.get('status') == 'ok':
                if json_response.get('solution'):
                    if json_response.get('solution').get('screenshot'):
                        image_data = base64.b64decode(json_response.get('solution').get('screenshot'))
                        with open("response_image.png", "wb") as image_file:
                            image_file.write(image_data)
                '''
                result = []
            
                ## Get Cookies & Clean
                cookies = json_response['solution']['cookies']
                for cookie in cookies:
                    result.append({cookie.get("name"): cookie.get("value")})

                ## Get User-Agent
                result.append({"user_agent": json_response['solution']['userAgent']})
                '''
                result = {}
                ## Get Cookies & Clean
                cookies = json_response['solution']['cookies']
                for cookie in cookies:
                    result.update({cookie.get("name"): cookie.get("value")})

                ## Get User-Agent
                result.update({"user_agent": json_response['solution']['userAgent']})

                
                #print(json.dumps(result))
                solve_flare_result = json.dumps(result)
                logging.info(json_response)
        else:
            #print(json_response.get('message'))
            solve_flare_result = json_response.get('message')

        self.try_to_clear_sessions()

        return response.status_code, solve_flare_result

    @staticmethod
    def formatCookies(cooks):
        result = {}

        for cookie in cooks:
            if cookie["value"] == "":
                continue

            if cookie["expires"]:
                if cookie["expires"] < round(datetime.now().timestamp()):
                    continue
        
            result.update({cookie["name"]: cookie["value"]})
    
        return result

    def solve_rookie(self, url_):
        #baseURL = f'{url_.replace("https://", "")}'
        baseURL = urlparse(url_).netloc
        cookies = {}
        user_agent = {"user_agent": ""}
        error = f"Error: No working cookies found. Try visiting {url_} in your browser first."
        browserError = ""
        browsers = ["chrome", "edge", "firefox", "opera", "opera_gx"] #Re-arrange according to your browser preference
        rookiepyFuncs = {"chrome": rookiepy.chrome,
                         "edge": rookiepy.edge,
                         "firefox": rookiepy.firefox,
                         "opera": rookiepy.opera,
                         "opera_gx": rookiepy.opera_gx
                         }
        defaultUA = {"chrome": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36",
                     "edge": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36 Edg/129.0.0.0",
                     "firefox": "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:131.0) Gecko/20100101 Firefox/131.0",
                     "opera": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36 OPR/113.0.0.0",
                     "opera_gx": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36 OPR/113.0.0.0"
                     }
        for browser in browsers:
            try:
                if not cookies:
                    cookies = rookiepyFuncs[browser]([baseURL])
                    cookies = self.formatCookies(cookies)
                    user_agent["user_agent"] = defaultUA[browser]
            except RuntimeError:
                browserError = "\nOr try installing any of the following [Chrome, Edge, Firefox, Opera, Opera GX] browsers."

        if not cookies:
            return 500, error + browserError

        cookies.update(user_agent)
        return 200, cookies
    
    def solve(self, url_):
        final_result = {'flaresolver_status_code': '',
                        'flaresolver_result': '',
                        'rookiepy_status_code': 400,
                        'rookiepy_result': 'rookiepy did not run'}
        if not ROOKIEPY_AVAILABLE:
            final_result['rookiepy_result'] = "Please make sure rookiepy is installed. {use: pip install rookiepy}"
        status_code, result_ = self.solve_flare(url_)
        #status_code, result_ = (404, 'FlareSolverr is not running!')
        final_result['flaresolver_status_code'] = status_code
        final_result['flaresolver_result'] = result_
        if status_code != 200:
            if ROOKIEPY_AVAILABLE:
                status_code, result_  = self.solve_rookie(url_)
                final_result['rookiepy_status_code'] = status_code
                final_result['rookiepy_result'] = result_

        print(final_result)
            

            

if __name__ == "__main__":
    url = sys.argv[1]
    cloudsolver = CloudSolver()
    cloudsolver.solve(url)
