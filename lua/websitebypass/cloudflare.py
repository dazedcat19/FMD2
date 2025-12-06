import requests
import sys
import argparse
import json
import logging
import base64
from pathlib import Path
from datetime import datetime, timedelta
from urllib.parse import urlparse

logging.basicConfig(filename=f'{Path(__file__).stem}.log', filemode='a',
                    format='%(asctime)s - %(levelname)s - %(message)s',
                    level=logging.INFO)

class CloudSolver:
    def __init__(self, debug=False, testing_result=False):
        self.debug = debug
        self.testing_result = testing_result
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
            sessions_list = requests.post(
                'http://localhost:8191/v1',
                headers= {'Content-Type': 'application/json'},
                json= {"cmd": "sessions.list",}
                ).json()['sessions']
            logging.info(f'flaresolverr sessions list: {sessions_list}')
            if self.session_id in sessions_list:
                requests.post(
                    'http://localhost:8191/v1',
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
            test_flaresolver = requests.get('http://127.0.0.1:8191/')
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
                result = {}
                ## Get Cookies & Clean
                cookies = json_response.get('solution')['cookies']
                for cookie in cookies:
                    result.update({cookie.get("name"): cookie.get("value")})

                ## Get User-Agent
                result.update({"user_agent": json_response.get('solution')['userAgent']})

                ## include solution for debugging and testing
                result.update({'solution': json_response.get('solution')})
                
                #print(json.dumps(result))
                #solve_flare_result = json.dumps(result)
                #solve_flare_result = result
                logging.info(json_response)
        else:
            #print(json_response.get('message'))
            solve_flare_result = json_response.get('message')

        return response.status_code, result

    def testing_solve_result(self, url_, result_dict):
        result_dict_ = dict(result_dict)
        useragent_ = result_dict_.pop('user_agent')
        solution_ = result_dict_.pop('solution')
        cookies_dict = result_dict_
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
                        }
        
        status_code, result_ = self.solve_flare(url_)
        #status_code, result_ = (404, 'FlareSolverr is not running!')
        final_result['flaresolver_status_code'] = status_code
        final_result['flaresolver_result'] = result_
        if status_code == 200:
            if self.testing_result:
                test_sc = self.testing_solve_result(url_, result_)
                if test_sc != 200:
                    final_result['flaresolver_status_code'] = test_sc
                    final_result['flaresolver_result'] = f'FlareSolverr did return cookies for {url_}, but it failed while testing it'
        # Delete solution befoer dumping the final result
        if isinstance(final_result.get('flaresolver_result'), dict):
            if final_result.get('flaresolver_result').get('solution'):
                del final_result['flaresolver_result']['solution']
            final_result['flaresolver_result'] = json.dumps(final_result.get('flaresolver_result'))
        

        print(final_result)
        #print(json.dumps(final_result))
            

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
    
    return parser.parse_args()
            

if __name__ == "__main__":
    #url = sys.argv[1]
    args = parse_arguments()
    cloudsolver = CloudSolver(debug= args.debug,testing_result=args.testing)
    cloudsolver.solve(args.Url)
