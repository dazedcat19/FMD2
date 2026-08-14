import requests
import sys
import argparse
import json
import logging
import base64
from pathlib import Path
from datetime import datetime, timedelta
from urllib.parse import urlparse, urlunparse

try:
    import rookiepy
    ROOKIEPY_AVAILABLE = True
except (ImportError, FileNotFoundError):
    ROOKIEPY_AVAILABLE = False

script_path = Path(__file__)
if (Path(__file__)/ '..' / 'lua/websitebypass').exists():
    websitebypass_path = Path(__file__)/ '..' / 'lua/websitebypass'
else:
    websitebypass_path = Path(__file__)/ '..'
debug_path = (websitebypass_path / 'debug').resolve()

class CloudSolver:
    def __init__(self, debug=False, testing_result=False,
                 flare_ip= "localhost", flare_port= 8191):
        if not (debug or testing_result):
            logging.disable()
        else:
            debug_path.mkdir(exist_ok=True)
            logging.basicConfig(filename=f'{debug_path / f"{Path(__file__).stem}.log"}', filemode='a',
                    format='%(asctime)s - %(levelname)s - %(message)s',
                    level=logging.INFO)
        self.debug = debug
        self.testing_result = testing_result
        flare_api_parsed = urlparse('http://localhost:8191/v1')
        self.flare_api_parsed = flare_api_parsed._replace(netloc=f"{flare_ip}:{flare_port}" if flare_port else flare_ip)
        # just random UUID
        self.session_id = 'e69e9ce7-8118-470c-a069-13af7bee6e8b'
        self.post_body = {"cmd": "request.get",
                          "session": self.session_id
                          }
        if self.debug:
            self.post_body.update(
                {"returnScreenshot": True}
                )
        else:
            self.post_body.update(
                {"returnOnlyCookies": True}
                )

    def try_to_clear_sessions(self):
        temp_cloudflare = websitebypass_path / 'temp_cloudflare.json'
        if not temp_cloudflare.exists():
            cloudflare_json = {'next_run':(datetime.now() - timedelta(seconds=1)).timestamp()}
        else:
            cloudflare_json = json.loads(temp_cloudflare.read_text())

        if cloudflare_json.get('next_run') < datetime.now().timestamp():
            sessions_list = requests.post(
                self.flare_api_parsed.geturl(),
                headers= {'Content-Type': 'application/json'},
                json= {"cmd": "sessions.list",}
                ).json()['sessions']
            logging.info(f'flaresolverr sessions list: {sessions_list}')
            if self.session_id in sessions_list:
                requests.post(
                    self.flare_api_parsed.geturl(),
                    headers= {'Content-Type': 'application/json'},
                    json= {"cmd": "sessions.destroy",
                           "session": self.session_id}
                    )
                logging.info(f'session {self.session_id} was cleared')
            cloudflare_json = {'next_run':(datetime.now() + timedelta(hours=1)).timestamp()}

            with open(temp_cloudflare, 'w') as temp_file:
                temp_file.write(json.dumps(cloudflare_json))
        
    def solve_flare(self, url_):
        try:
            test_flaresolver = requests.get(self.flare_api_parsed._replace(path='').geturl())
            if test_flaresolver.status_code == 200:
                if test_flaresolver.json()['msg'] != 'FlareSolverr is ready!':
                    logging.info('FlareSolverr is not running!')
                    return 404, 'FlareSolverr is not running!'
        except requests.exceptions.ConnectionError:
            return 404, 'FlareSolverr is not running!'
        self.try_to_clear_sessions()
        logging.info(url_)
        post_body = self.post_body
        post_body['url'] = url_
        response = requests.post(
            self.flare_api_parsed.geturl(),
            headers={'Content-Type': 'application/json'},
            json=post_body)
        logging.info(response.status_code)
        json_response = response.json()
        if response.status_code == 200:
            if self.debug:
                with open(debug_path / 'json_response.json', 'w') as json_file:
                    json.dump(json_response , json_file, indent=4)
            if json_response.get('status') == 'ok':
                if json_response.get('solution'):
                    if json_response.get('solution').get('screenshot'):
                        image_data = base64.b64decode(json_response.get('solution').get('screenshot'))
                        with open(debug_path / "response_image.png", "wb") as image_file:
                            image_file.write(image_data)
                result = {}
                ## Get Cookies & Clean
                cookies = json_response.get('solution')['cookies']
                for cookie in cookies:
                    result.update({cookie.get("name"): cookie.get("value")})

                ## Get User-Agent
                result.update({"user_agent": json_response.get('solution')['userAgent']})

                ## include solution for debugging and testing
                result.update({'solution': json_response.get('solution')})
                
                solve_flare_result = result
                logging.info(json_response)
        else:
            solve_flare_result = json_response.get('message')

        return response.status_code, solve_flare_result

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

    def testing_solve_result(self, url_, result_dict):
        result_dict_ = dict(result_dict)
        cookies_dict = {}
        solution_ = {"cookies":{}}
        useragent_ = result_dict_.pop('user_agent')
        if result_dict_.get('solution'):
            solution_ = result_dict_.pop('solution')
        for cf_key in ['cf_clearance', 'csrftoken']:
            if cf_key in result_dict_:
                cookies_dict[cf_key] = result_dict_.get(cf_key)
        testing_session = requests.session()
        testing_cookies = requests.sessions.cookiejar_from_dict(cookies_dict)
        testing_session.cookies.update(testing_cookies)
        testing_session.headers.update({"User-Agent": useragent_})
        request = testing_session.get(url_)
        if request.status_code !=200:
            for solution_cookie in solution_.get("cookies"):
                solution_cookie.pop('sameSite')
                expires_ = solution_cookie.pop('expiry')
                rest_ = {'HttpOnly': solution_cookie.pop('httpOnly')}
                solution_cookie.update({'expires': expires_,
                                        'rest': rest_})
                testing_cookies.set(**solution_cookie)
        return request.status_code
    
    def solve(self, url_):
        final_result = {'flaresolver_status_code': '',
                        'flaresolver_result': '',
                        'rookiepy_status_code': 400,
                        'rookiepy_result': 'rookiepy did not run',
                        'testing_cookies': self.testing_result,
                        'testing_cookies_result': '',
                        'enable_debug': self.debug,
                        }
        if not ROOKIEPY_AVAILABLE:
            final_result['rookiepy_result'] = "Please make sure rookiepy is installed. {use: pip install rookiepy}"
        status_code, result_ = self.solve_flare(url_)
        final_result['flaresolver_status_code'] = status_code
        final_result['flaresolver_result'] = result_
        if status_code != 200:
            if ROOKIEPY_AVAILABLE:
                status_code, result_  = self.solve_rookie(url_)
                final_result['rookiepy_status_code'] = status_code
                final_result['rookiepy_result'] = result_
        if status_code == 200:
            if self.testing_result:
                test_sc = self.testing_solve_result(url_, result_)
                if test_sc == 200:
                    final_result['testing_cookies_result'] = 'testing cookies using python requests return code "200"'
                else:
                    if final_result['rookiepy_status_code'] == 200:
                        final_result['testing_cookies_result'] = f'FlareSolverr failed and Rookiepy did return cookies for {url_}, but it failed with code {test_sc} while testing it'
                    else:
                        final_result['testing_cookies_result'] = f'FlareSolverr did return cookies for {url_}, but it failed with code {test_sc} while testing it'
        # Delete solution befoer dumping the final result
        if isinstance(final_result.get('flaresolver_result'), dict):
            if final_result.get('flaresolver_result').get('solution'):
                del final_result['flaresolver_result']['solution']
            final_result['flaresolver_result'] = json.dumps(final_result.get('flaresolver_result'))
        print(json.dumps(final_result))
            

def parse_arguments():
    parser = argparse.ArgumentParser(description="A script to solve cloudflare.")

    # Add a required positional argument for the filename
    parser.add_argument("Url", help="the url for site that we want to try to solve.")


    # Add the debug flag using action="store_true"
    parser.add_argument(
        "-d", "--debug",
        action="store_true",
        help="Enable debug mode."
    )

    # Add the testing flag using action="store_true"
    parser.add_argument(
        "-t", "--testing",
        action="store_true",
        help="Enable testing restult mode."
    )

    # Use custom Flaresolverr ip
    parser.add_argument(
        "-Fip", "--flaresolverr-ip",
        default= "localhost",
        help="Use custom Flaresolverr ip.(default: %(default)s)"
    )

    # Use custom Flaresolverr port
    parser.add_argument(
        "-Fp", "--flaresolverr-port",
        default= 8191,
        help="Use custom Flaresolverr port.(default: %(default)s)"
    )
    
    return parser.parse_args()
            

if __name__ == "__main__":
    args = parse_arguments()
    cloudsolver = CloudSolver(debug=args.debug, testing_result=args.testing,
                              flare_ip= args.flaresolverr_ip, flare_port= args.flaresolverr_port)
    cloudsolver.solve(args.Url)
